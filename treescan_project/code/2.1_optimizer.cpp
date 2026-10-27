// [[Rcpp::plugins(cpp17)]]
// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>
// Native event parser and ancestor aggregations
#ifndef FAST_LAG_CORE_HPP
#define FAST_LAG_CORE_HPP

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <limits>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace nssp {

struct Result {
    int row_id;
    std::string code;
    double visit_epoch;
    double first_epoch;
    double delay_hours;
};

inline bool digit(char c) { return c >= '0' && c <= '9'; }
inline bool upper(char c) { return c >= 'A' && c <= 'Z'; }
inline bool alnum(char c) { return digit(c) || upper(c); }
inline bool leap(int y) { return y % 4 == 0 && (y % 100 != 0 || y % 400 == 0); }
inline int days_in_month(int year, int month) {
    static const int days[] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    return month == 2 && leap(year) ? 29 : days[month];
}
inline int64_t days_from_civil(int y, unsigned m, unsigned d) {
    y -= m <= 2;
    const int era = (y >= 0 ? y : y - 399) / 400;
    const unsigned yoe = static_cast<unsigned>(y - era * 400);
    const unsigned doy = (153 * (m + (m > 2 ? -3 : 9)) + 2) / 5 + d - 1;
    const unsigned doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    return era * 146097LL + static_cast<int>(doe) - 719468LL;
}
inline int number(const char* s, int len) {
    int value = 0;
    for (int i = 0; i < len; ++i) value = value * 10 + (s[i] - '0');
    return value;
}
// Match exactly the anchored MDT event prefix; preserve the event even if its date is invalid.
inline bool parse_event_prefix(const char* start, const char* end, int& id, const char*& payload, bool diag) {
    if (end - start < 4 || *start != '{') return false;
    const char* p = start + 1;
    if (p == end || !digit(*p)) return false;
    uint64_t n = 0;
    while (p != end && digit(*p)) {
        n = n * 10 + (*p - '0');
        if (n > INT32_MAX) return false; // as.integer() returns NA on overflow
        ++p;
    }
    if (p == end || *p++ != '}' || p == end || *p++ != ';') return false;
    if (diag) {
        if (p == end || *p++ != ';') return false;
        // The original regex used '.' without DOTALL, so internal newlines fail.
        for (const char* q = p; q < end; ++q) if (*q == '\n' || *q == '\r') return false;
    }
    id = static_cast<int>(n);
    payload = p;
    return true;
}
inline bool timestamp_prefix(const char* p, const char* end) {
    if (end - p < 19) return false;
    for (int i = 0; i < 19; ++i) {
        if (i == 4 || i == 7) { if (p[i] != '-') return false; }
        else if (i == 10) { if (p[i] != ' ') return false; }
        else if (i == 13 || i == 16) { if (p[i] != ':') return false; }
        else if (!digit(p[i])) return false;
    }
    return true;
}
inline double parse_utc_19(const char* p) {
    const int y = number(p, 4), m = number(p + 5, 2), d = number(p + 8, 2);
    const int h = number(p + 11, 2), mi = number(p + 14, 2), s = number(p + 17, 2);
    if (m < 1 || m > 12 || d < 1 || d > days_in_month(y, m) || h > 23 || mi > 59 || s > 59)
        return std::numeric_limits<double>::infinity(); // min(..., na.rm=TRUE) when all invalid
    return static_cast<double>(days_from_civil(y, m, d) * 86400LL + h * 3600 + mi * 60 + s);
}

inline void process_row(const char* diag, const char* mdt, double visit, int row_id, std::vector<Result>& output) {
    if (!diag || !mdt || !*diag || !*mdt || std::isnan(visit)) return;
    const double inf = std::numeric_limits<double>::infinity();
    std::unordered_map<int, double> earliest_by_event;
    earliest_by_event.reserve(8);
    const char* mdt_end = mdt + std::strlen(mdt);
    for (const char* item = mdt; item < mdt_end;) {
        const char* stop = static_cast<const char*>(std::memchr(item, '|', mdt_end - item));
        if (!stop) stop = mdt_end;
        int id = 0;
        const char* payload = nullptr;
        if (parse_event_prefix(item, stop, id, payload, false) && timestamp_prefix(payload, stop)) {
            const double tm = parse_utc_19(payload);
            auto it = earliest_by_event.find(id);
            if (it == earliest_by_event.end()) earliest_by_event.emplace(id, tm);
            else if (tm < it->second) it->second = tm;
        }
        item = stop == mdt_end ? mdt_end : stop + 1;
    }
    if (earliest_by_event.empty()) return;

    std::unordered_map<std::string, double> earliest_by_code;
    earliest_by_code.reserve(12);
    const char* diag_end = diag + std::strlen(diag);
    for (const char* item = diag; item < diag_end;) {
        const char* stop = static_cast<const char*>(std::memchr(item, '|', diag_end - item));
        if (!stop) stop = diag_end;
        int id = 0;
        const char* payload = nullptr;
        if (parse_event_prefix(item, stop, id, payload, true)) {
            const auto event = earliest_by_event.find(id);
            if (event != earliest_by_event.end()) {
                for (const char* p = payload; p + 3 <= stop;) {
                    if (upper(p[0]) && digit(p[1]) && alnum(p[2])) {
                        const char* q = p + 3;
                        if (q + 1 < stop && *q == '.' && alnum(q[1])) {
                            ++q;
                            int k = 0;
                            while (q < stop && k < 4 && alnum(*q)) { ++q; ++k; }
                        }
                        std::string code(p, q);
                        const auto it = earliest_by_code.find(code);
                        if (it == earliest_by_code.end()) earliest_by_code.emplace(std::move(code), event->second);
                        else if (event->second < it->second) it->second = event->second;
                        p = q; // non-overlapping stri_extract_all_regex semantics
                    } else ++p;
                }
            }
        }
        item = stop == diag_end ? diag_end : stop + 1;
    }
    if (earliest_by_code.empty()) return;

    std::vector<std::pair<std::string, double>> ordered;
    ordered.reserve(earliest_by_code.size());
    for (auto& kv : earliest_by_code) {
        const double hours = (kv.second - visit) / 3600.0;
        if (!std::isnan(hours) && hours >= 0.0) ordered.emplace_back(kv.first, kv.second);
    }
    std::sort(ordered.begin(), ordered.end(), [](const auto& a, const auto& b) { return a.first < b.first; });
    for (auto& kv : ordered) output.push_back({row_id, std::move(kv.first), visit, kv.second, (kv.second - visit) / 3600.0});
}

inline void first_appearances(const std::vector<std::string>& diags,
                             const std::vector<std::string>& times,
                             const std::vector<double>& visits,
                             std::vector<Result>& output) {
    for (size_t i = 0; i < diags.size(); ++i)
        process_row(diags[i].c_str(), times[i].c_str(), visits[i], static_cast<int>(i + 1), output);
}

struct SeenKey {
    size_t lag;
    uint64_t bits;
    bool operator==(const SeenKey& o) const { return lag == o.lag && bits == o.bits; }
};
struct SeenHash {
    size_t operator()(const SeenKey& k) const {
        return std::hash<size_t>()(k.lag) ^ (std::hash<uint64_t>()(k.bits) + 0x9e3779b97f4a7c15ULL);
    }
};
inline uint64_t value_bits(double d) {
    if (d == 0.0) d = 0.0; // normalize -0, as unique.data.table() does
    uint64_t bits;
    std::memcpy(&bits, &d, sizeof(bits));
    return bits;
}

struct QuantileGroup {
    std::string code;
    std::vector<double> values;
};

// Accumulate unique (row, ancestor, delay) tuples WITHOUT expanding them to an R table.
inline std::vector<QuantileGroup> group_delays(const std::vector<Result>& rows,
                                               const std::vector<std::pair<std::string, std::string>>& map) {
    std::unordered_map<std::string, std::vector<std::string>> ancestors;
    ancestors.reserve(map.size());
    for (const auto& p : map) ancestors[p.first].push_back(p.second);
    std::unordered_map<std::string, size_t> group_index;
    group_index.reserve(map.size());
    std::vector<QuantileGroup> groups;
    std::unordered_set<SeenKey, SeenHash> seen;
    int previous_row = -1;
    for (const Result& row : rows) {
        if (row.row_id != previous_row) { seen.clear(); previous_row = row.row_id; }
        const auto itr = ancestors.find(row.code);
        if (itr == ancestors.end()) continue;
        for (const auto& ancestor : itr->second) {
            auto idx = group_index.find(ancestor);
            size_t j;
            if (idx == group_index.end()) {
                j = groups.size();
                group_index.emplace(ancestor, j);
                groups.push_back({ancestor, {}});
            } else j = idx->second;
            if (seen.insert({j, value_bits(row.delay_hours)}).second)
                groups[j].values.push_back(row.delay_hours);
        }
    }
    return groups;
}
inline std::vector<double> quantiles_type7(std::vector<double>& v, const std::vector<double>& probs) {
    std::sort(v.begin(), v.end());
    std::vector<double> out;
    out.reserve(probs.size());
    if (v.empty()) { out.assign(probs.size(), NAN); return out; }
    for (double p : probs) {
        const double idx = 1.0 + (static_cast<double>(v.size()) - 1.0) * p;
        const size_t lo = static_cast<size_t>(std::floor(idx)) - 1;
        const size_t hi = static_cast<size_t>(std::ceil(idx)) - 1;
        const double a = v[lo];
        if (lo == hi || a == v[hi]) out.push_back(a);
        else {
            const double fraction = idx - std::floor(idx);
            out.push_back((1.0 - fraction) * a + fraction * v[hi]);
        }
    }
    return out;
}

} // namespace nssp
#endif

#ifdef _OPENMP
#include <omp.h>
#endif
using namespace Rcpp;

// [[Rcpp::export]]
List fast_lag_first_appearances(CharacterVector diag, CharacterVector mdt,
                                 NumericVector visit_epoch, IntegerVector original_row,
                                 int threads = 1) {
    const R_xlen_t n = diag.size();
    if (mdt.size() != n || visit_epoch.size() != n || original_row.size() != n)
        stop("Diagnosis, MDT, visit and row ID vectors must have the same length.");
    // Access all R character objects on the main thread; workers only touch C pointers.
    std::vector<const char*> d(n), t(n);
    for (R_xlen_t i = 0; i < n; ++i) {
        d[i] = STRING_ELT(diag, i) == NA_STRING ? nullptr : CHAR(STRING_ELT(diag, i));
        t[i] = STRING_ELT(mdt, i) == NA_STRING ? nullptr : CHAR(STRING_ELT(mdt, i));
    }
    int count = std::max(1, threads);
#ifdef _OPENMP
    count = std::min(count, omp_get_max_threads());
#else
    count = 1;
#endif
    if (n < 20000) count = 1;
    std::vector<std::vector<nssp::Result>> partition(static_cast<size_t>(count));
#ifdef _OPENMP
#pragma omp parallel for num_threads(count) schedule(static)
#endif
    for (int worker = 0; worker < count; ++worker) {
        const R_xlen_t begin = static_cast<R_xlen_t>(worker) * n / count;
        const R_xlen_t end = static_cast<R_xlen_t>(worker + 1) * n / count;
        auto& result = partition[worker];
        for (R_xlen_t i = begin; i < end; ++i)
            nssp::process_row(d[i], t[i], visit_epoch[i], original_row[i], result);
    }
    size_t total = 0;
    for (const auto& x : partition) total += x.size();
    IntegerVector row(total);
    CharacterVector code(total);
    NumericVector visit(total), first(total), delay(total);
    size_t k = 0;
    for (const auto& group : partition)
        for (const auto& x : group) {
            row[k] = x.row_id;
            code[k] = x.code;
            visit[k] = x.visit_epoch;
            first[k] = x.first_epoch;
            delay[k] = x.delay_hours;
            ++k;
        }
    visit.attr("class") = CharacterVector::create("POSIXct", "POSIXt");
    visit.attr("tzone") = "UTC";
    first.attr("class") = CharacterVector::create("POSIXct", "POSIXt");
    first.attr("tzone") = "UTC";
    return List::create(_["row_id"] = row, _["code"] = code,
                        _["visit_time"] = visit, _["first_time"] = first,
                        _["delay_hours"] = delay);
}

// [[Rcpp::export]]
List fast_lag_group_quantiles(IntegerVector row_id, CharacterVector code,
                               NumericVector delay, CharacterVector map_code,
                               CharacterVector map_ancestor, NumericVector probs) {
    if (row_id.size() != code.size() || code.size() != delay.size() || map_code.size() != map_ancestor.size())
        stop("Input vector length mismatch.");
    std::vector<nssp::Result> rows;
    rows.reserve(row_id.size());
    for (R_xlen_t i = 0; i < row_id.size(); ++i)
        rows.push_back({row_id[i], as<std::string>(code[i]), 0.0, 0.0, delay[i]});
    std::vector<std::pair<std::string, std::string>> map;
    map.reserve(map_code.size());
    for (R_xlen_t i = 0; i < map_code.size(); ++i)
        map.emplace_back(as<std::string>(map_code[i]), as<std::string>(map_ancestor[i]));
    auto groups = nssp::group_delays(rows, map);
    std::vector<double> pr(probs.begin(), probs.end());
    CharacterVector names(groups.size());
    List values(pr.size());
    for (size_t j = 0; j < pr.size(); ++j) values[j] = NumericVector(groups.size());
    for (size_t i = 0; i < groups.size(); ++i) {
        names[i] = groups[i].code;
        auto q = nssp::quantiles_type7(groups[i].values, pr);
        for (size_t j = 0; j < pr.size(); ++j) REAL(values[j])[i] = q[j];
    }
    return List::create(_["code"] = names, _["quantiles"] = values);
}
