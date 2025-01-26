#include <Rcpp.h>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix read_obj_cpp(std::string file_path) {

  // Open the OBJ file
  std::ifstream file(file_path);

  // Vectors to store vertex and face data
  std::vector<double> vertices;
  std::vector<int> faces;

  std::string line;

  // Process lines
  while (std::getline(file, line)) {
    if (line.empty() || line[0] == '#') {
      continue; // Skip empty lines or comments
    }

    // Vertex lines
    if (line[0] == 'v' && line[1] == ' ') {
      // (v x y z)
      size_t pos = 2;
      double x = std::stod(line.substr(pos, line.find(" ", pos) - pos));
      pos = line.find(" ", pos) + 1;
      double y = std::stod(line.substr(pos, line.find(" ", pos) - pos));
      pos = line.find(" ", pos) + 1;
      double z = std::stod(line.substr(pos));
      vertices.push_back(x);
      vertices.push_back(y);
      vertices.push_back(z);
    }
    // Face lines
    else if (line[0] == 'f' && line[1] == ' ') {
      // (f v1 v2 v3) using R 1-based indexing
      size_t pos = 2;
      int v1 = std::stoi(line.substr(pos, line.find(" ", pos) - pos)) - 1;
      pos = line.find(" ", pos) + 1;
      int v2 = std::stoi(line.substr(pos, line.find(" ", pos) - pos)) - 1;
      pos = line.find(" ", pos) + 1;
      int v3 = std::stoi(line.substr(pos)) - 1;
      faces.push_back(v1);
      faces.push_back(v2);
      faces.push_back(v3);
    }
  }

  file.close();

  // Create triangles matrix
  size_t num_faces = faces.size() / 3;
  NumericMatrix triangles(3 * num_faces, 3);

  // Fill triangles matrix
  for (size_t i = 0; i < num_faces; ++i) {
    int v1 = faces[3 * i];
    int v2 = faces[3 * i + 1];
    int v3 = faces[3 * i + 2];

    triangles(3 * i, 0) = vertices[3 * v1];
    triangles(3 * i, 1) = vertices[3 * v1 + 1];
    triangles(3 * i, 2) = vertices[3 * v1 + 2];

    triangles(3 * i + 1, 0) = vertices[3 * v2];
    triangles(3 * i + 1, 1) = vertices[3 * v2 + 1];
    triangles(3 * i + 1, 2) = vertices[3 * v2 + 2];

    triangles(3 * i + 2, 0) = vertices[3 * v3];
    triangles(3 * i + 2, 1) = vertices[3 * v3 + 1];
    triangles(3 * i + 2, 2) = vertices[3 * v3 + 2];
  }

  return triangles;
}
