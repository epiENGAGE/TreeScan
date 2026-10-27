// Fixed-substring matching for all signal nodes in one pass.
// Aho-Corasick preserves grepl(pattern, x, fixed = TRUE) semantics for nonempty patterns.
#include <array>
#include <cstdlib>
#include <algorithm>
#include <chrono>
#include <cstdint>
#include <iostream>
#include <numeric>
#include <queue>
#include <random>
#include <string>
#include <unordered_set>
#include <vector>

struct Node {
  std::array<int, 256> next;
  int fail = 0;
  std::vector<int> output;
  Node() { next.fill(-1); }
};

class Matcher {
 public:
  explicit Matcher(const std::vector<std::string>& needles) {
    trie.emplace_back();
    for (size_t id = 0; id < needles.size(); ++id) {
      if (needles[id].empty()) continue;
      int state = 0;
      for (unsigned char c : needles[id]) {
        int nxt = trie[state].next[c];
        if (nxt < 0) {
          nxt = static_cast<int>(trie.size());
          trie[state].next[c] = nxt;
          trie.emplace_back();
        }
        state = nxt;
      }
      trie[state].output.push_back(static_cast<int>(id));
    }
    std::queue<int> q;
    for (int c = 0; c < 256; ++c) {
      int v = trie[0].next[c];
      if (v < 0) trie[0].next[c] = 0;
      else { trie[v].fail = 0; q.push(v); }
    }
    while (!q.empty()) {
      int u = q.front(); q.pop();
      const int f = trie[u].fail;
      for (int id : trie[f].output) trie[u].output.push_back(id);
      for (int c = 0; c < 256; ++c) {
        int v = trie[u].next[c];
        if (v < 0) trie[u].next[c] = trie[f].next[c];
        else {
          trie[v].fail = trie[f].next[c];
          q.push(v);
        }
      }
    }
  }
  template <typename Callback>
  void scan(const std::string& haystack, Callback found) const {
    int state = 0;
    for (unsigned char c : haystack) {
      state = trie[state].next[c];
      for (int id : trie[state].output) found(id);
    }
  }
  // Scan R's existing UTF-8/byte buffer without allocating a std::string for each row
  template <typename Callback>
  void scan_cstr(const char* haystack, Callback found, bool skip_dots = false) const {
    int state = 0;
    for (const unsigned char* p = reinterpret_cast<const unsigned char*>(haystack); *p; ++p) {
      if (skip_dots && *p == '.') continue;
      state = trie[state].next[*p];
      for (int id : trie[state].output) found(id);
    }
  }
 private:
  std::vector<Node> trie;
};

#ifndef NSSP_STANDALONE
#include <Rcpp.h>
// [[Rcpp::plugins(cpp17)]]
// [[Rcpp::export]]
Rcpp::List nssp_fixed_hits(Rcpp::CharacterVector haystack, Rcpp::CharacterVector patterns, bool skip_dots = false) {
  const int m = patterns.size();
  std::vector<std::string> needles(m);
  for (int i = 0; i < m; ++i) {
    if (patterns[i] != NA_STRING) needles[i] = Rcpp::as<std::string>(patterns[i]);
  }
  const Matcher matcher(needles);
  std::vector<std::vector<int>> hits(m);
  std::vector<int> last_seen(m, -1);
  for (R_xlen_t row = 0; row < haystack.size(); ++row) {
    SEXP cell = STRING_ELT(haystack, row);
    if (cell == NA_STRING) continue;
    matcher.scan_cstr(CHAR(cell), [&](int id) {
      if (last_seen[id] != row) {
        last_seen[id] = static_cast<int>(row);
        hits[id].push_back(static_cast<int>(row + 1));
      }
    }, skip_dots);
  }
  Rcpp::List out(m);
  for (int i = 0; i < m; ++i) out[i] = Rcpp::wrap(hits[i]);
  return out;
}
#else
int main(int argc, char** argv) {
  // Check overlapping patterns, duplicates, dots, empty tokens, and repeated matches
  const std::vector<std::string> probes = {
    "T83.511;T83511", "T.8.3.5.1.1", "A10A101", "", "...", "a10", "A.10B200"
  };
  const std::vector<std::string> probe_keys = {
    "T83511", "T83", "A10", "A101", "A10", "", "B200", "A10B200"
  };
  Matcher probe_matcher(probe_keys);
  std::vector<std::vector<int>> observed(probe_keys.size());
  std::vector<std::vector<int>> expected(probe_keys.size());
  std::vector<int> probe_seen(probe_keys.size(), -1);
  for (size_t i = 0; i < probes.size(); ++i) {
    std::string cleaned;
    for (char c : probes[i]) if (c != '.') cleaned.push_back(c);
    for (size_t j = 0; j < probe_keys.size(); ++j) {
      if (!probe_keys[j].empty() && cleaned.find(probe_keys[j]) != std::string::npos)
        expected[j].push_back(static_cast<int>(i + 1));
    }
    probe_matcher.scan_cstr(probes[i].c_str(), [&](int id) {
      if (probe_seen[id] != static_cast<int>(i)) {
        probe_seen[id] = static_cast<int>(i);
        observed[id].push_back(static_cast<int>(i + 1));
      }
    }, true);
  }
  if (observed != expected) {
    std::cerr << "FAILED: overlapping/duplicate/dotted fixture\n";
    return 2;
  }
  std::mt19937 rng(1337);
  const int records = argc > 2 ? std::max(1, std::atoi(argv[2])) : 180000;
  const int patterns = argc > 1 ? std::max(1, std::atoi(argv[1])) : 64;
  std::vector<std::string> needles;
  needles.reserve(patterns);
  for (int i = 0; i < patterns; ++i) {
    std::string s = "A";
    s += std::to_string(i + 10);
    s += "Z";
    needles.push_back(s);
  }
  std::vector<std::string> text;
  text.reserve(records);
  for (int i = 0; i < records; ++i) {
    std::string s = "DX;Q98.10;OTHER";
    if (i % 3 == 0) s += ";" + needles[rng() % patterns];
    if (i % 11 == 0) s += ";" + needles[rng() % patterns];
    text.push_back(std::move(s));
  }
  // Include dots in diagnosis strings to verify removal in the streaming scanner.
  for (int i = 0; i < records; ++i) {
    if (i % 5 == 0) text[i].insert(2, ".");
    if (i % 7 == 0) text[i] += ";A.999Z";
  }
  auto begin = std::chrono::steady_clock::now();
  std::vector<std::vector<int>> naive(patterns);
  for (int i = 0; i < records; ++i) {
    std::string cleaned;
    for (char c : text[i]) if (c != '.') cleaned.push_back(c);
    for (int p = 0; p < patterns; ++p)
      if (cleaned.find(needles[p]) != std::string::npos) naive[p].push_back(i + 1);
  }
  auto middle = std::chrono::steady_clock::now();
  Matcher matcher(needles);
  std::vector<std::vector<int>> legacy(patterns);
  std::vector<int> old_seen(patterns, -1);
  for (int i = 0; i < records; ++i) {
    // Simulate the per-row Rcpp::as<std::string>() copy made by the older kernel
    std::string row_copy;
    for (char c : text[i]) if (c != '.') row_copy.push_back(c);
    matcher.scan(row_copy, [&](int p) {
      if (old_seen[p] != i) {
        old_seen[p] = i;
        legacy[p].push_back(i + 1);
      }
    });
  }
  auto middle2 = std::chrono::steady_clock::now();
  std::vector<std::vector<int>> fast(patterns);
  std::vector<int> last_seen(patterns, -1);
  for (int i = 0; i < records; ++i) {
    matcher.scan_cstr(text[i].c_str(), [&](int p) {
      if (last_seen[p] != i) {
        last_seen[p] = i;
        fast[p].push_back(i + 1);
      }
    }, true);
  }
  auto end = std::chrono::steady_clock::now();
  if (naive != fast || naive != legacy) { std::cerr << "FAILED: results differ\n"; return 1; }
  const auto ms = [](auto a, auto b) {
    return std::chrono::duration<double, std::milli>(b - a).count();
  };
  std::cout << "PASS: " << records << " records x " << patterns << " patterns\n";
  std::cout << "Reference: " << ms(begin, middle) << " ms\n";
  std::cout << "Older native: " << ms(middle, middle2) << " ms\n";
  std::cout << "Zero-copy native: " << ms(middle2, end) << " ms\n";
  std::cout << "Speedup vs reference: " << ms(begin, middle) / ms(middle2, end) << "x\n";
  std::cout << "Speedup vs older native: " << ms(middle, middle2) / ms(middle2, end) << "x\n";
  return 0;
}
#endif
