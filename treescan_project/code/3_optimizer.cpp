// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <string>
#include <vector>
#include <unordered_set>
#include <cstdlib>
#include <algorithm>

using namespace Rcpp;

namespace {

// Keep token order when making the diagnosis search strings
std::vector<std::string> split_token(const std::string& x, char delim) {
  std::vector<std::string> out;
  if (x.empty()) return out;
  size_t start = 0;
  for (size_t i = 0; i < x.size(); ++i) {
    if (x[i] == delim) {
      out.emplace_back(x.substr(start, i - start));
      start = i + 1;
    }
  }
  if (start < x.size()) out.emplace_back(x.substr(start));
  return out;
}

// No hash allocation for the common short histories
inline void insert_unique_linear(std::vector<std::string>& out,
                                 const std::vector<std::string>& more) {
  for (const auto& item : more) {
    if (std::find(out.begin(), out.end(), item) == out.end())
      out.push_back(item);
  }
}

// Avoid hash allocations for the common short patient histories;
// switch to hashing only when the number of codes grows large.
struct StableUnique {
  std::vector<std::string> values;
  std::unordered_set<std::string> seen;
  bool hashed = false;

  void add(const std::vector<std::string>& more) {
    if (!hashed && values.size() + more.size() > 24) {
      seen.insert(values.begin(), values.end());
      hashed = true;
    }
    for (const auto& item : more) {
      const bool fresh = hashed
        ? seen.insert(item).second
        : std::find(values.begin(), values.end(), item) == values.end();
      if (fresh) values.push_back(item);
    }
  }
};

std::string join_tokens(const std::vector<std::string>& tokens) {
  if (tokens.empty()) return "";
  size_t size = tokens.size() - 1;
  for (const auto& x : tokens) size += x.size();
  std::string out;
  out.reserve(size);
  for (size_t i = 0; i < tokens.size(); ++i) {
    if (i) out.push_back('|');
    out += tokens[i];
  }
  return out;
}

inline bool within(int a, int b) {
  return a != NA_INTEGER && b != NA_INTEGER &&
    static_cast<long long>(a) - b <= 365 &&
    static_cast<long long>(b) - a <= 365;
}

} // namespace

// Match the original patient's index-based logic, including search2[i]
// [[Rcpp::export]]
Rcpp::List nssp_patient_search_cpp(Rcpp::IntegerVector dates,
                                   Rcpp::CharacterVector severity,
                                   Rcpp::CharacterVector Level3,
                                   Rcpp::IntegerVector group_id) {
  const R_xlen_t n = dates.size();
  if (severity.size() != n || Level3.size() != n || group_id.size() != n)
    stop("Patient vectors must have the same length");

  CharacterVector search(n), search2(n), search3(n);
  std::fill(search.begin(), search.end(), NA_STRING);
  std::fill(search2.begin(), search2.end(), NA_STRING);
  std::fill(search3.begin(), search3.end(), NA_STRING);

  R_xlen_t begin = 0;
  while (begin < n) {
    R_xlen_t end = begin + 1;
    while (end < n && group_id[end] == group_id[begin]) ++end;
    const size_t m = static_cast<size_t>(end - begin);

    std::vector<std::vector<std::string>> parts(m);
    std::vector<unsigned char> admit(m, 0);
    std::vector<size_t> admit_rows;
    admit_rows.reserve(m);

    for (size_t i = 0; i < m; ++i) {
      R_xlen_t k = begin + i;
      if (Level3[k] != NA_STRING)
        parts[i] = split_token(as<std::string>(Level3[k]), ',');
      if (severity[k] != NA_STRING && as<std::string>(severity[k]) == "A") {
        admit[i] = 1;
        admit_rows.push_back(i);
      }
    }

    // search = earlier rows within 365 days, not necessarily earlier dates
    std::vector<std::string> s(m);
    std::vector<std::string> s2(m);
    std::vector<unsigned char> has2(m, 0);
    for (size_t i = 0; i < m; ++i) {
      if (m <= 24) {
        std::vector<std::string> codes;
        for (size_t j = 0; j < i; ++j) {
          if (within(dates[begin + j], dates[begin + i]))
            insert_unique_linear(codes, parts[j]);
        }
        s[i] = codes.empty() ? "NONE" : join_tokens(codes);
      } else {
        StableUnique codes;
        for (size_t j = 0; j < i; ++j) {
          if (within(dates[begin + j], dates[begin + i]))
            codes.add(parts[j]);
        }
        s[i] = codes.values.empty() ? "NONE" : join_tokens(codes.values);
      }
    }

    // search2 = admissions that affect earlier visits
    for (size_t ai = 0; ai < admit_rows.size(); ++ai) {
      const size_t ri = admit_rows[ai];
      const int d = dates[begin + ri];
      bool earlier_admit = false;
      for (size_t aj = 0; aj < ai; ++aj) {
        if (within(d, dates[begin + admit_rows[aj]])) {
          earlier_admit = true;
          break;
        }
      }
      if (!earlier_admit) {
        has2[ri] = 1;
        s2[ri] = "NONE";
      }
      // The reference reads search2[i] (the admission ordinal), not search2[ri].
      bool any_prior = false;
      for (size_t vi = 0; vi < m; ++vi) {
        const int vd = dates[begin + vi];
        if (!admit[vi] && vd != NA_INTEGER && d != NA_INTEGER &&
            static_cast<long long>(vd) < d &&
            static_cast<long long>(vd) >= static_cast<long long>(d) - 365) {
          any_prior = true;
          break;
        }
      }
      if (any_prior) {
        std::string updated;
        if (m <= 24) {
          std::vector<std::string> codes;
          if (has2[ai]) insert_unique_linear(codes, split_token(s2[ai], '|'));
          insert_unique_linear(codes, parts[ri]);
          updated = join_tokens(codes);
        } else {
          StableUnique codes;
          if (has2[ai]) codes.add(split_token(s2[ai], '|'));
          codes.add(parts[ri]);
          updated = join_tokens(codes.values);
        }
        for (size_t vi = 0; vi < m; ++vi) {
          const int vd = dates[begin + vi];
          if (!admit[vi] && vd != NA_INTEGER && d != NA_INTEGER &&
              static_cast<long long>(vd) < d &&
              static_cast<long long>(vd) >= static_cast<long long>(d) - 365) {
            has2[vi] = 1;
            s2[vi] = updated;
          }
        }
      }
    }

    // search3 = current admission codes absent from earlier-dated admissions
    if (admit_rows.size() >= 2) {
      for (size_t ai = 1; ai < admit_rows.size(); ++ai) {
        const size_t ri = admit_rows[ai];
        const int d = dates[begin + ri];
        std::unordered_set<std::string> prior;
        for (size_t aj = 0; aj < admit_rows.size(); ++aj) {
          const size_t rj = admit_rows[aj];
          const int pd = dates[begin + rj];
          if (d != NA_INTEGER && pd != NA_INTEGER && pd < d &&
              static_cast<long long>(pd) >= static_cast<long long>(d) - 365) {
            for (const auto& code : parts[rj]) prior.insert(code);
          }
        }
        std::unordered_set<std::string> emitted;
        std::vector<std::string> remaining;
        for (const auto& code : parts[ri]) {
          if (!prior.count(code) && emitted.insert(code).second)
            remaining.push_back(code);
        }
        search3[begin + ri] = join_tokens(remaining);
      }
    }

    for (size_t i = 0; i < m; ++i) {
      search[begin + i] = s[i];
      if (has2[i]) search2[begin + i] = s2[i];
    }
    begin = end;
  }
  return List::create(_["search"] = search,
                      _["search2"] = search2,
                      _["search3"] = search3);
}
