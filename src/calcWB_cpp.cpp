
#include <Rcpp.h>
using namespace Rcpp;

// Helper function to convert a string to uppercase
std::string to_uppercase(const std::string& str) {
    std::string upper_str = str;
    for(int i = 0; i < upper_str.size(); ++i) {
        upper_str[i] = std::toupper(upper_str[i]);
    }
    return upper_str;
}

// [[Rcpp::export]]
NumericVector calcWB_cpp(NumericVector airTemp, NumericVector relativeHumidity, 
                          std::string inputunits = "degC", std::string outputunits = "degF", 
                          Nullable<std::string> method = R_NilValue) {
    int n = airTemp.size();
    if (relativeHumidity.size() != n) {
        stop("airTemp and relativeHumidity must be the same length.");
    }
    
    // Convert input units to uppercase
    std::string inputunits_upper = to_uppercase(inputunits);
    std::string outputunits_upper = to_uppercase(outputunits);
    
    // Convert input units to Celsius if necessary
    if (inputunits_upper == "degF" || inputunits_upper == "FAHRENHEIT") {
        for(int i = 0; i < n; ++i) {
            if (!NumericVector::is_na(airTemp[i])) {
                airTemp[i] = (airTemp[i] - 32.0) * 5.0 / 9.0;
            }
        }
    }
    
    // Initialize wetb vector
    NumericVector wetb(n, NA_REAL);
    
    // Determine which method to use
    bool use_old_method = false;
    if (!method.isNull()) {
        std::string method_str = to_uppercase(as<std::string>(method));
        if (method_str == "OLD") {
            use_old_method = true;
        }
    }
    
    if (!use_old_method) {
        // Default method
        for(int i = 0; i < n; ++i) {
            if (NumericVector::is_na(airTemp[i]) || NumericVector::is_na(relativeHumidity[i])) {
                wetb[i] = NA_REAL;
                continue;
            }
            double rh = relativeHumidity[i];
            double at = airTemp[i];
            double term1 = at * std::atan(0.151977 * std::sqrt(rh + 8.313659));
            double term2 = std::atan(at + rh);
            double term3 = - std::atan(rh - 1.676331);
            double term4 = 0.00391838 * std::pow(rh, 1.5) * std::atan(0.023101 * rh);
            double term5 = -4.686035;
            wetb[i] = term1 + term2 + term3 + term4 + term5;
        }
    } else {
        // \'old\' method
        for(int i = 0; i < n; ++i) {
            if (NumericVector::is_na(airTemp[i]) || NumericVector::is_na(relativeHumidity[i])) {
                wetb[i] = NA_REAL;
                continue;
            }
            double rh = relativeHumidity[i];
            double at = airTemp[i];
            wetb[i] = (-5.806 + 0.672 * at - 0.006 * at * at 
                      + (0.061 + 0.004 * at + 9.9e-05 * at * at) * rh 
                      + (-3.3e-05 - 5e-06 * at - 1e-07 * at * at) * rh * rh);
        }
    }
    
    // Convert output units to Fahrenheit if necessary
    if (outputunits_upper == "degF" || outputunits_upper == "FAHRENHEIT") {
        for(int i = 0; i < n; ++i) {
            if (!NumericVector::is_na(wetb[i])) {
                wetb[i] = wetb[i] * 9.0 / 5.0 + 32.0;
            }
        }
    }
    
    return wetb;
}