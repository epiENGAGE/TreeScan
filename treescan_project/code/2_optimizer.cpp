#include <algorithm>
#include <cstring>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

// Clean
// Split at semicolons, remove empty and repeated codes, and keep their order
struct CodePiece {
    std::size_t pos;
    std::size_t size;
};

static void clean_one_code_string(
    const char* text,
    const std::size_t length,
    std::vector<CodePiece>& seen,
    std::unordered_set<std::string>& large_seen,
    std::string& cleaned
) {
    seen.clear();
    large_seen.clear();
    cleaned.clear();

    std::size_t start = 0;
    while (start < length) {
        std::size_t end = start;
        while (end < length && text[end] != ';') ++end;

        const std::size_t size = end - start;
        if (size != 0) {
            bool add = true;

            if (seen.size() < 24 && large_seen.empty()) {
                for (const auto& p : seen) {
                    if (p.size == size &&
                        std::memcmp(text + p.pos, text + start, size) == 0) {
                        add = false;
                        break;
                    }
                }
            } else {
                if (large_seen.empty()) {
                    large_seen.reserve(64);
                    for (const auto& p : seen)
                        large_seen.emplace(text + p.pos, p.size);
                }
                add = large_seen.emplace(text + start, size).second;
            }

            if (add) {
                if (!cleaned.empty()) cleaned.push_back(' ');
                cleaned.append(text + start, size);
                if (large_seen.empty()) seen.push_back({start, size});
            }
        }

        if (end == length) break;
        start = end + 1;
    }
    return;
}

#ifndef STANDALONE
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::CharacterVector fast_clean_codes_cpp(Rcpp::CharacterVector x) {
    const R_xlen_t n = x.size();
    Rcpp::CharacterVector result(n);
    std::vector<CodePiece> seen;
    std::unordered_set<std::string> large_seen;
    std::string cleaned;

    // Clean
    // Cache whole diagnosis strings when many repeated values are present
    bool use_cache = false;
    if (n >= 200000) {
        const R_xlen_t sample_size = std::min<R_xlen_t>(n, 20000);
        std::unordered_set<SEXP> sample;
        sample.reserve(static_cast<std::size_t>(sample_size));
        for (R_xlen_t i = 0; i < sample_size; ++i)
            sample.insert(STRING_ELT(x, i));
        use_cache = sample.size() * 4 < static_cast<std::size_t>(sample_size) * 3;
    }

    std::unordered_map<SEXP, SEXP> cache;
    if (use_cache) cache.reserve(100000);

    for (R_xlen_t i = 0; i < n; ++i) {
        const SEXP value = STRING_ELT(x, i);
        if (value == NA_STRING) {
            SET_STRING_ELT(result, i, NA_STRING);
            continue;
        }
        if (use_cache) {
            const auto old = cache.find(value);
            if (old != cache.end()) {
                SET_STRING_ELT(result, i, old->second);
                continue;
            }
        }

        const char* raw = CHAR(value);
        clean_one_code_string(raw, std::strlen(raw), seen, large_seen, cleaned);
        const SEXP output = cleaned.empty() ? NA_STRING :
            Rf_mkCharLenCE(cleaned.data(), static_cast<int>(cleaned.size()), Rf_getCharCE(value));
        SET_STRING_ELT(result, i, output);
        if (use_cache) cache.emplace(value, output);
    }
    return result;
}
#else
#include <cassert>
#include <chrono>
#include <iostream>
#include <random>

static std::string reference_clean(const std::string& x) {
    std::unordered_set<std::string> prior;
    std::string output;
    std::size_t from = 0;
    while (from < x.size()) {
        const auto to = x.find(';', from);
        const auto finish = to == std::string::npos ? x.size() : to;
        if (finish > from) {
            std::string piece = x.substr(from, finish - from);
            if (prior.insert(piece).second) {
                if (!output.empty()) output += ' ';
                output += piece;
            }
        }
        if (finish == x.size()) break;
        from = finish + 1;
    }
    return output;
}

int main() {
    std::vector<CodePiece> seen;
    std::unordered_set<std::string> large_seen;
    std::string cleaned;
    for (const char* test : {"", ";;;", "A", "A;B;A;;C;B;", "NA;B", ";A;", "A ;A;A ;", "a;A;a", "A;;;A", "A;"}) {
        const std::string x(test);
        clean_one_code_string(x.data(), x.size(), seen, large_seen, cleaned);
        assert(cleaned == reference_clean(x));
    }
    std::string many;
    for (int i=0; i<200; ++i) many += "C" + std::to_string(i % 103) + ";";
    clean_one_code_string(many.data(), many.size(), seen, large_seen, cleaned);
    assert(cleaned == reference_clean(many));

    std::mt19937 rng(23);
    std::vector<std::string> inputs;
    inputs.reserve(400000);
    for (int i = 0; i < 400000; ++i) {
        std::string s;
        const int count = 2 + rng() % 9;
        for (int j = 0; j < count; ++j) {
            if (j != 0) s.push_back(';');
            if (rng() % 13 == 0) continue;
            s += 'C';
            s += std::to_string(rng() % 150);
        }
        inputs.push_back(std::move(s));
    }

    for (const std::string& x: inputs) {
        clean_one_code_string(x.data(), x.size(), seen, large_seen, cleaned);
        if (cleaned != reference_clean(x)) { std::cerr << "Mismatch: " << x << "\n"; return 1; }
    }

    std::size_t checksum = 0;
    auto bench = [&](const bool optimized) {
        const auto begin = std::chrono::steady_clock::now();
        for (const auto& x : inputs) {
            if (optimized) {
                clean_one_code_string(x.data(), x.size(), seen, large_seen, cleaned);
                checksum += cleaned.size();
            } else {
                checksum += reference_clean(x).size();
            }
        }
        return std::chrono::duration<double>(std::chrono::steady_clock::now() - begin).count();
    };
    bench(true);
    bench(false);
    for (int round=0; round<3; ++round) {
        const double t1 = bench(true);
        const double t2 = bench(false);
        std::cout << "round=" << round << " fast_core=" << t1 << "s hash_reference=" << t2
                  << "s speedup=" << (t2/t1) << "x\n";
    }
    std::cout << "verified " << inputs.size() << " native cases, checksum=" << checksum << "\n";
}
#endif
