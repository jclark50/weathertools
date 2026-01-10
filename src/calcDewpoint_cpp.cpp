
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calcDewpoint_cpp(NumericVector airTemp, NumericVector relativeHumidity, 
                               std::string inputunits = "degF", std::string outputunits = "degF", int roundby = 2) {
    int n = airTemp.size();
    NumericVector dewPoint(n);

    // Create a copy of airTemp to avoid modifying the original vector
    NumericVector tempCopy = clone(airTemp);

    // Convert input to Celsius if needed
    if (inputunits == "degF") {
        for (int i = 0; i < n; i++) {
            tempCopy[i] = (tempCopy[i] - 32) * 5.0 / 9.0;
        }
    } else if (inputunits != "degC") {
        stop("Input units must be \"F\" or \"C\"");
    }

    // Calculate dew point
    for (int i = 0; i < n; i++) {
        double rh = fmin(relativeHumidity[i], 100.0) / 100.0;
        double log_rh = log(rh);
        double temp = tempCopy[i];
        dewPoint[i] = (243.04 * (log_rh + (17.625 * temp) / (243.04 + temp))) / 
                      (17.625 - log_rh - (17.625 * temp) / (243.04 + temp));
    }

    // Convert output to Fahrenheit if needed
    if (outputunits == "degF") {
        for (int i = 0; i < n; i++) {
            dewPoint[i] = dewPoint[i] * 9.0 / 5.0 + 32.0;
        }
    } else if (outputunits != "degC") {
        stop("Output units must be \"F\" or \"C\"");
    }

    // Round values
    for (int i = 0; i < n; i++) {
        dewPoint[i] = round(dewPoint[i] * pow(10, roundby)) / pow(10, roundby);
    }

    return dewPoint;
}