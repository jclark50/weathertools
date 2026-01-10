// [[Rcpp::depends(RcppParallel)]]
// on linux, must do: sudo apt-get install libtbb-dev
#include <Rcpp.h>
#include <RcppParallel.h>
using namespace Rcpp;
using namespace RcppParallel;

struct HeatIndexCalculator : public Worker {
  const RVector<double> airTemp;
  const RVector<double> relativeHumidity;
  RVector<double> heatIndex;

  HeatIndexCalculator(NumericVector airTemp, NumericVector relativeHumidity, NumericVector heatIndex)
      : airTemp(airTemp), relativeHumidity(relativeHumidity), heatIndex(heatIndex) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; ++i) {
      double t = airTemp[i];
      double rh = relativeHumidity[i];
      double hi;

      if (Rcpp::NumericVector::is_na(t) || Rcpp::NumericVector::is_na(rh)) {
        heatIndex[i] = NA_REAL;
        continue;
      }

      if (t <= 40) {
        hi = 0.5 * (t + 61 + ((t - 68) * 1.2) + (rh * 0.094)) + t / 2.0;
      } else {
        double alpha = 61 + ((t - 68) * 1.2) + (rh * 0.094);
        hi = 0.5 * (alpha + t);
        if (hi > 79) {
          hi = -42.379 + 2.04901523 * t + 10.14333127 * rh -
               0.22475541 * t * rh - 0.00683783 * t * t -
               0.05481717 * rh * rh + 0.00122874 * t * t * rh +
               0.00085282 * t * rh * rh - 0.00000199 * t * t * rh * rh;

          if (rh <= 13 && t >= 80 && t <= 112) {
            double adjustment1 = (13 - rh) / 4;
            double adjustment2 = sqrt((17 - abs(t - 95)) / 17);
            hi -= adjustment1 * adjustment2;
          } else if (rh > 85 && t >= 80 && t <= 87) {
            double adjustment1 = (rh - 85) / 10;
            double adjustment2 = (87 - t) / 5;
            hi += adjustment1 * adjustment2;
          }
        }
      }

      heatIndex[i] = hi;

      // Debug: Print intermediate results (remove for large datasets)

    }
  }
};

// [[Rcpp::export]]
NumericVector calcHI_parallel(NumericVector airTemp, NumericVector relativeHumidity) {
  NumericVector heatIndex(airTemp.size());
  HeatIndexCalculator calc(airTemp, relativeHumidity, heatIndex);
  parallelFor(0, airTemp.size(), calc);
  return heatIndex;
}
