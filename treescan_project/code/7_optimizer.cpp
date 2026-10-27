// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#ifndef NSSP_FAST_SIGNAL_CORE_HPP
#define NSSP_FAST_SIGNAL_CORE_HPP
#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <queue>
#include <string>
#include <vector>

// Count literal signal codes with the original lookaround boundaries.
// mode 0: (?<![A-Z0-9.])CODE(?![A-Z0-9.])
// mode 1: ;CODE (no restriction on the character following CODE).
namespace nssp_fast {
inline bool blocked(unsigned char ch) {
  return (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9') || ch == '.';
}
inline bool word(unsigned char ch) {
  return (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') ||
         (ch >= '0' && ch <= '9') || ch == '_';
}
inline bool allowed_suffix(unsigned char ch) {
  return (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9');
}

struct State {
  std::array<int, 256> next;
  int failure;
  std::vector<int> outputs;
  State() : failure(0) { next.fill(-1); }
};

class Matcher {
  std::vector<State> states;
  std::vector<std::size_t> lengths;
  int mode;
public:
  explicit Matcher(const std::vector<std::string>& patterns, int mode_in)
    : lengths(patterns.size()), mode(mode_in) {
    states.emplace_back();
    for (std::size_t p = 0; p < patterns.size(); ++p) {
      const auto& pattern = patterns[p];
      lengths[p] = pattern.size();
      if (pattern.empty()) continue;
      int current = 0;
      for (unsigned char ch : pattern) {
        int next = states[current].next[ch];
        if (next == -1) {
          next = static_cast<int>(states.size());
          states[current].next[ch] = next;
          states.emplace_back();
        }
        current = next;
      }
      states[current].outputs.push_back(static_cast<int>(p));
    }
    std::queue<int> work;
    for (int ch = 0; ch < 256; ++ch) {
      int next = states[0].next[ch];
      if (next == -1) states[0].next[ch] = 0;
      else { states[next].failure = 0; work.push(next); }
    }
    while (!work.empty()) {
      int current = work.front(); work.pop();
      const int failure = states[current].failure;
      for (int ch = 0; ch < 256; ++ch) {
        int next = states[current].next[ch];
        if (next == -1) {
          states[current].next[ch] = states[failure].next[ch];
        } else {
          states[next].failure = states[failure].next[ch];
          const auto& inherited = states[states[next].failure].outputs;
          states[next].outputs.insert(states[next].outputs.end(), inherited.begin(), inherited.end());
          work.push(next);
        }
      }
    }
  }

  void count_one(const char* input, std::size_t n, std::vector<std::uint64_t>& counts,
                 std::vector<std::size_t>& last_end,
                 std::vector<std::uint64_t>& last_row,
                 std::uint64_t row) const {
    int state = 0;
    for (std::size_t k = 0; k < n; ++k) {
      const unsigned char ch = static_cast<unsigned char>(input[k]);
      state = states[state].next[ch];
      const auto& outputs = states[state].outputs;
      if (outputs.empty()) continue;
      const std::size_t end = k + 1;
      for (int id : outputs) {
        const std::size_t length = lengths[id];
        if (!length || length > end) continue;
        const std::size_t start = end - length;
        if (mode == 0) {
          if (start > 0 && blocked(static_cast<unsigned char>(input[start - 1]))) continue;
          if (end < n && blocked(static_cast<unsigned char>(input[end]))) continue;
        } else {
          if (start == 0 || input[start - 1] != ';') continue;
        }
        // stringr::str_count counts nonoverlapping matches within each pattern.
        if (last_row[id] == row && start < last_end[id]) continue;
        counts[id]++;
        last_row[id] = row;
        last_end[id] = end;
      }
    }
  }
};

// Count the global ICD expression without materializing matches:
// \\b[A-Z][0-9]{2}(?:\\.[0-9A-Z]+)?\\b
// Character-class and word-boundary behavior is ASCII here.
inline std::uint64_t total_icd(const char* text, std::size_t n) {
  std::uint64_t count = 0;
  const auto* p = reinterpret_cast<const unsigned char*>(text);
  std::size_t i = 0;
  while (i + 2 < n) {
    if (p[i] >= 'A' && p[i] <= 'Z' &&
        p[i + 1] >= '0' && p[i + 1] <= '9' &&
        p[i + 2] >= '0' && p[i + 2] <= '9' &&
        (i == 0 || !word(p[i - 1]))) {
      std::size_t end = i + 3;
      if (end < n && p[end] == '.' && end + 1 < n && allowed_suffix(p[end + 1])) {
        std::size_t tail = end + 1;
        while (tail < n && allowed_suffix(p[tail])) ++tail;
        if (tail == n || !word(p[tail])) end = tail;
      }
      if (end == n || !word(p[end])) { ++count; i = end; continue; }
    }
    ++i;
  }
  return count;
}
} // namespace nssp_fast
#endif

#include <limits>
#include <string>
#include <vector>
using namespace Rcpp;

// Count every requested signal code in one pass over the diagnosis strings.
// [[Rcpp::export]]
NumericVector nssp_signal_counts_cpp(CharacterVector values,
                                     CharacterVector codes,
                                     int mode = 0) {
  if (mode != 0 && mode != 1) stop("mode must be 0 or 1");
  std::vector<std::string> patterns;
  patterns.reserve(codes.size());
  for (R_xlen_t i = 0; i < codes.size(); ++i) {
    patterns.push_back(codes[i] == NA_STRING ? std::string() : as<std::string>(codes[i]));
  }
  nssp_fast::Matcher matcher(patterns, mode);
  std::vector<std::uint64_t> count(patterns.size(), 0), last_row(patterns.size(), 0);
  std::vector<std::size_t> last_end(patterns.size(), 0);
  std::uint64_t row = 1;
  for (R_xlen_t i = 0; i < values.size(); ++i, ++row) {
    SEXP cell = STRING_ELT(values, i);
    if (cell == NA_STRING) continue;
    matcher.count_one(CHAR(cell), LENGTH(cell), count, last_end, last_row, row);
  }
  NumericVector out(count.size());
  for (std::size_t j = 0; j < count.size(); ++j) out[j] = static_cast<double>(count[j]);
  return out;
}

// Count all ICD-like code appearances without allocating a long code table.
// [[Rcpp::export]]
double nssp_total_icd_cpp(CharacterVector values) {
  std::uint64_t count = 0;
  for (R_xlen_t i = 0; i < values.size(); ++i) {
    SEXP cell = STRING_ELT(values, i);
    if (cell != NA_STRING) count += nssp_fast::total_icd(CHAR(cell), LENGTH(cell));
  }
  return static_cast<double>(count);
}
