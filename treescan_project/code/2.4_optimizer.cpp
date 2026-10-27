#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>
#include <utility>
#include <vector>

using namespace Rcpp;

// Compute the original grid-based trapezoidal AUC, not the continuous AUC.
// Quantile columns are stored contiguously for each code (p x n matrix).
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
List nssp_auc_scores_cpp(const NumericMatrix& q,
                         const NumericVector& probs,
                         const NumericVector& pooled,
                         const NumericVector& grid,
                         const double step,
                         const double T) {
  const int p = q.nrow();
  const int n = q.ncol();
  const int ng = grid.size();

  if (probs.size() != p || pooled.size() != ng || ng < 2)
    stop("Inconsistent percentile or time-grid dimensions.");

  NumericVector scores(n, NA_REAL);
  std::vector<int> fallback;
  fallback.reserve(8);

  // Reuse buffers rather than allocating one R data frame per code.
  std::vector<std::pair<double, double> > knots;
  std::vector<double> xs, ys;
  knots.reserve(p);
  xs.reserve(p + 1);
  ys.reserve(p + 1);

  for (int i = 0; i < n; ++i) {
    knots.clear();
    xs.clear();
    ys.clear();

    bool ordered = true;
    bool regular = true;
    double last = -std::numeric_limits<double>::infinity();

    for (int j = 0; j < p; ++j) {
      const double x = q(j, i);
      if (ISNAN(x)) continue; // NA and NaN are discarded by the original R code.
      // Preserve reference behavior on unusual inputs by handling them in R.
      if (!std::isfinite(x) || x < 0.0) {
        regular = false;
        break;
      }
      if (x < last) ordered = false;
      last = x;
      knots.push_back(std::make_pair(x, probs[j]));
    }

    if (!regular) {
      fallback.push_back(i + 1); // R uses 1-based indices.
      continue;
    }
    if (knots.empty()) continue;

    // Quantiles are normally sorted already. Only sort anomalous rows.
    if (!ordered) {
      std::sort(knots.begin(), knots.end(),
        [](const std::pair<double,double>& a,
           const std::pair<double,double>& b) {
          return a.first < b.first ||
                 (a.first == b.first && a.second < b.second);
        });
    }

    // Anchor at time 0 with CDF 0, including duplicate zero-time knots.
    xs.push_back(0.0);
    ys.push_back(0.0);
    for (std::size_t j = 0; j < knots.size(); ++j) {
      xs.push_back(knots[j].first);
      ys.push_back(knots[j].second);
    }

    // Two-pointer interpolation: O(number of quantiles + grid length).
    // Advancing across tied knots gives the final CDF at exact ties.
    std::size_t at = 0;
    double previous = 0.0;
    long double area_twice = 0.0L;
    const std::size_t nx = xs.size();

    for (int g = 0; g < ng; ++g) {
      const double t = grid[g];
      while (at + 1 < nx && xs[at + 1] <= t) ++at;

      double cdf;
      if (t < xs[0]) {
        cdf = ys[0];
      } else if (at + 1 == nx) {
        cdf = ys[at];
      } else {
        const double f = (t - xs[at]) / (xs[at + 1] - xs[at]);
        cdf = ys[at] + f * (ys[at + 1] - ys[at]);
      }

      const double difference = cdf - pooled[g];
      if (g) area_twice += difference + previous;
      previous = difference;
    }

    scores[i] = static_cast<double>(area_twice) * step / (2.0 * T);
  }

  return List::create(_["scores"] = scores,
                      _["fallback"] = wrap(fallback));
}
