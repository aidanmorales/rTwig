#include <Rcpp.h>
using namespace Rcpp;

//' @title Generate random colors
//'
//' @description Generates random hex colors
//'
//' @param n number of colors to generate as an integer
//' @return returns a character vector of hexidecimal colors
//'
//' @noRd
//'
// [[Rcpp::export]]
CharacterVector generate_random_colors(int n) {
  CharacterVector colors(n);

  // Loop to generate n random colors
  for (int i = 0; i < n; ++i) {
    std::stringstream colorStream;
    colorStream << "#";

    // Generate random values for each component (RR, GG, BB)
    for (int j = 0; j < 3; ++j) {
      int component = trunc(R::runif(85, 255)); // Generate random number between 85 and 255
      colorStream << std::hex << std::setw(2) << std::setfill('0') << component;
    }

    colors[i] = colorStream.str();
  }

  return colors;
}

//' @title Hexidecimal to floating point
//'
//' @description Convert hex colors to floating points between 0-1
//'
//' @param hex_colors character string of hex colors
//' @return returns a character vector of floating point colors
//'
//' @noRd
//'
// [[Rcpp::export]]
NumericVector hex_to_float(std::vector<std::string> hex_colors) {

  // Vector to store RGB color values
  int n = hex_colors.size();
  NumericVector colors(n);

  // Convert hexadecimal to an integer value
  auto hex_to_int = [](std::string hex) {
    int value = 0;
    for (char c : hex) {
      value = value * 16 + std::stoi(std::string(1, c), nullptr, 16);
    }
    return value;
  };

  for (int i = 0; i < n; ++i) {
    std::string hex_color = hex_colors[i];

    // Remove #
    hex_color = hex_color.substr(1);

    // Extract RGB components
    int red = hex_to_int(hex_color.substr(0, 2));
    int green = hex_to_int(hex_color.substr(2, 2));
    int blue = hex_to_int(hex_color.substr(4, 2));

    // Combine RGB components into a single double value
    double colorValue = (double(red) * 65536 + double(green) * 256 + double(blue)) / 16777216.0;

    colors[i] = colorValue;
  }

  return colors;
}
