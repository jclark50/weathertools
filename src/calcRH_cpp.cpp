
#include <Rcpp.h>
using namespace Rcpp;

// NumericVector calcRH_cpp(NumericVector airTemp, NumericVector dewPoint, 
                         // String inputunits = 'F') {
    // int n = airTemp.size();
    // NumericVector relativeHumidity(n);

    // // Convert units if necessary
    // if (inputunits == "F") {
        // for (int i = 0; i < n; i++) {
            // airTemp[i] = (airTemp[i] - 32) * 5.0 / 9.0;
            // dewPoint[i] = (dewPoint[i] - 32) * 5.0 / 9.0;
        // }
    // } else if (inputunits != "C") {
        // stop("Input units must be 'F' or 'C'");
    // }

    // for (int i = 0; i < n; i++) {
        // // Calculate relative humidity
        // double exp_dew = exp((17.625 * dewPoint[i]) / (243.04 + dewPoint[i]));
        // double exp_air = exp((17.625 * airTemp[i]) / (243.04 + airTemp[i]));
        // relativeHumidity[i] = 100 * (exp_dew / exp_air);
    // }
    // return relativeHumidity;
// }

// [[Rcpp::export]]
NumericVector calcRH_cpp(NumericVector airTemp, NumericVector dewPoint, 
                         String inputunits = "degC") {
    int n = airTemp.size();
    NumericVector relativeHumidity(n);

    // Convert units if necessary
    if (inputunits == "degF") {
        for (int i = 0; i < n; i++) {
            airTemp[i] = (airTemp[i] - 32) * 5.0 / 9.0;
            dewPoint[i] = (dewPoint[i] - 32) * 5.0 / 9.0;
        }
    } else if (inputunits != "degC") {
        stop("Input units must be 'F' or 'C'");
    }

    for (int i = 0; i < n; i++) {
        // Calculate relative humidity
        double exp_dew = exp((17.625 * dewPoint[i]) / (243.04 + dewPoint[i]));
        double exp_air = exp((17.625 * airTemp[i]) / (243.04 + airTemp[i]));
        relativeHumidity[i] = 100 * (exp_dew / exp_air);
    }
    return relativeHumidity;
}

// [[Rcpp::export]]
NumericVector calcRH_vpd_cpp(NumericVector airTemp, NumericVector vpd, 
                             String inputunits = "degC") {
    int n = airTemp.size();
    NumericVector relativeHumidity(n);

    // Convert units if necessary
    if (inputunits == "degF") {
        for (int i = 0; i < n; i++) {
            airTemp[i] = (airTemp[i] - 32) * 5.0 / 9.0;
        }
    } else if (inputunits != "degC") {
        stop("Input units must be 'F' or 'C'");
    }

    for (int i = 0; i < n; i++) {
        // Calculate saturation vapor pressure
        double svp = 610.78 * exp((17.2694 * airTemp[i]) / (airTemp[i] + 238.3)) / 1000.0; // Convert to kPa
        relativeHumidity[i] = ((vpd[i] / svp) - 1) * -100;
    }
    return relativeHumidity;
}
