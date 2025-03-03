#include <Rcpp.h>
#include <fstream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>
#include <tuple>
#include "helper_functions.h"

using namespace Rcpp;

// Generate a hash for a vertex
std::size_t vertex_hash(NumericVector vertex) {
  return std::hash<float>()(vertex[0]) ^ std::hash<float>()(vertex[1]) ^ std::hash<float>()(vertex[2]);
}

//' @title Write PLY
//'
//' @description Export a QSM cylinder mesh to .ply
//'
//' @param vertices NumericMatrix
//' @param colors NumericMatrix
//' @param normals NumericMatrix
//' @param filename string
//' @return ply
//'
//' @noRd
//'
// [[Rcpp::export]]
void write_ply(
    Rcpp::NumericMatrix vertices,
    Rcpp::Nullable<Rcpp::NumericMatrix> colors,
    Rcpp::Nullable<Rcpp::NumericMatrix> normals,
    std::string filename) {

  // Open the PLY file in binary mode
  std::ofstream file(filename, std::ios::out | std::ios::binary);

  // Get the number of vertices
  int n_vertices = vertices.nrow();

  // Create a map to track unique vertices and their indices
  std::unordered_map<std::size_t, int> vertexMap;
  std::vector<Rcpp::NumericVector> unique_vertices;
  std::vector<int> face_indices; // To store the face indices

  // Store color and normal indices and corresponding unique values
  std::vector<int> color_indices;
  std::vector<int> normal_indices;
  std::vector<Rcpp::NumericVector> unique_colors;
  std::vector<Rcpp::NumericVector> unique_normals;

  // If colors are provided, cast to a NumericMatrix
  Rcpp::NumericMatrix color_matrix;
  if (colors.isNotNull()) {
    color_matrix = Rcpp::as<Rcpp::NumericMatrix>(colors);
  }

  // If normals are provided, cast to a NumericMatrix
  Rcpp::NumericMatrix normal_matrix;
  if (normals.isNotNull()) {
    normal_matrix = Rcpp::as<Rcpp::NumericMatrix>(normals);
  }

  // Iterate over the vertices to remove duplicates
  for (int i = 0; i < n_vertices; ++i) {
    Rcpp::NumericVector vertex = vertices(i, Rcpp::_);
    std::size_t hashValue = vertex_hash(vertex);

    // If the vertex is already seen, add the index of the existing vertex
    if (vertexMap.find(hashValue) == vertexMap.end()) {
      // Otherwise, add it to the unique_vertices list
      vertexMap[hashValue] = unique_vertices.size();
      unique_vertices.push_back(vertex);

      // Store the corresponding color and normal values if provided
      if (colors.isNotNull()) {
        unique_colors.push_back(color_matrix.row(i));
      }

      if (normals.isNotNull()) {
        unique_normals.push_back(normal_matrix.row(i));
      }
    }
    // Add the index of the vertex in the face
    face_indices.push_back(vertexMap[hashValue]);

    // Store the indices for colors and normals
    color_indices.push_back(i);
    normal_indices.push_back(i);
  }

  // Write the PLY header
  file << "ply\n";
  file << "format binary_little_endian 1.0\n";
  file << "element vertex " << unique_vertices.size() << "\n";
  file << "property float x\n";
  file << "property float y\n";
  file << "property float z\n";

  // Only write color and alpha properties if there are more than 3 columns
  bool has_colors = colors.isNotNull();
  if (has_colors) {
    file << "property uchar red\n";
    file << "property uchar green\n";
    file << "property uchar blue\n";
    file << "property uchar alpha\n";
  }

  bool has_normals = normals.isNotNull();
  if (has_normals) {
    file << "property float nx\n";
    file << "property float ny\n";
    file << "property float nz\n";
  }

  file << "element face " << face_indices.size() / 3 << "\n";
  file << "property list uchar int vertex_index\n";
  file << "end_header\n";

  // Write the unique vertex data in binary format
  for (size_t i = 0; i < unique_vertices.size(); ++i) { // Change to size_t
    Rcpp::NumericVector vertex = unique_vertices[i];
    float x = vertex[0];
    float y = vertex[1];
    float z = vertex[2];

    // Write binary data for the vertex coordinates
    file.write(reinterpret_cast<char*>(&x), sizeof(float));
    file.write(reinterpret_cast<char*>(&y), sizeof(float));
    file.write(reinterpret_cast<char*>(&z), sizeof(float));

    // Write color data if available
    if (has_colors) {
      Rcpp::NumericVector color = unique_colors[i];
      unsigned char r = static_cast<unsigned char>(color[0]);
      unsigned char g = static_cast<unsigned char>(color[1]);
      unsigned char b = static_cast<unsigned char>(color[2]);
      unsigned char a = static_cast<unsigned char>(color[3]);

      // Write binary data for color and alpha
      file.write(reinterpret_cast<char*>(&r), sizeof(unsigned char));
      file.write(reinterpret_cast<char*>(&g), sizeof(unsigned char));
      file.write(reinterpret_cast<char*>(&b), sizeof(unsigned char));
      file.write(reinterpret_cast<char*>(&a), sizeof(unsigned char));
    }

    // Write normal data if available
    if (has_normals) {
      Rcpp::NumericVector normal = unique_normals[i];
      float nx = normal[0];
      float ny = normal[1];
      float nz = normal[2];

      // Write binary data for normals
      file.write(reinterpret_cast<char*>(&nx), sizeof(float));
      file.write(reinterpret_cast<char*>(&ny), sizeof(float));
      file.write(reinterpret_cast<char*>(&nz), sizeof(float));
    }
  }

  // Write the face data in binary format
  for (size_t i = 0; i < face_indices.size(); i += 3) { // Change to size_t
    unsigned char vertexCount = 3;
    file.write(reinterpret_cast<char*>(&vertexCount), sizeof(unsigned char));

    // Write the vertex indices for the face
    int vertex1 = face_indices[i];
    int vertex2 = face_indices[i + 1];
    int vertex3 = face_indices[i + 2];

    file.write(reinterpret_cast<char*>(&vertex1), sizeof(int));
    file.write(reinterpret_cast<char*>(&vertex2), sizeof(int));
    file.write(reinterpret_cast<char*>(&vertex3), sizeof(int));
  }

  // Close the file
  file.close();
}

//' @title Write OBJ
//'
//' @description Export a QSM cylinder mesh to .obj
//'
//' @param vertices NumericMatrix
//' @param normals NumericMatrix
//' @param filename string
//' @return obj
//'
//' @noRd
//'
// [[Rcpp::export]]
void write_obj(
    Rcpp::NumericMatrix vertices,
    Rcpp::Nullable<Rcpp::NumericMatrix> normals,
    std::string filename) {

  // Open the OBJ file
  std::ofstream file(filename);

  // Write vertices
  int n_vertices = vertices.nrow();
  for (int i = 0; i < n_vertices; ++i) {
    Rcpp::NumericVector vertex = vertices(i, Rcpp::_);
    file << "v " << vertex[0] << " " << vertex[1] << " " << vertex[2] << "\n";
  }

  // If normals are provided, cast to a NumericMatrix
  Rcpp::NumericMatrix normal_matrix;
  if (normals.isNotNull()) {
    normal_matrix = Rcpp::as<Rcpp::NumericMatrix>(normals);
  }

  // Write normals
  int n_normals = normal_matrix.nrow();
  for (int i = 0; i < n_normals; ++i) {
    Rcpp::NumericVector normal = normal_matrix(i, Rcpp::_);
    file << "vn " << normal[0] << " " << normal[1] << " " << normal[2] << "\n";
  }

  // Write faces using 1-based indexing
  for (int i = 0; i < n_vertices; i += 3) {
    file << "f " << (i + 1) << "//" << (i + 1) << " "
         << (i + 2) << "//" << (i + 2) << " "
         << (i + 3) << "//" << (i + 3) << "\n";
  }

  // Close the file
  file.close();
}

//' @title Write STL
//'
//' @description Export a QSM cylinder mesh to .stl
//'
//' @param vertices NumericMatrix
//' @param normals NumericMatrix
//' @param filename string
//' @return stl
//'
//' @noRd
//'
// [[Rcpp::export]]
void write_stl(
    NumericMatrix vertices,
    NumericMatrix normals,
    std::string filename) {

  // Open the STL file in binary write mode
  std::ofstream file(filename, std::ios::binary);

  // Write the 80-byte header (set to empty or custom)
  char header[80] = {0};
  file.write(header, 80);

  // Number of triangles (3 vertices and normals per triangle)
  unsigned int num_triangles = vertices.nrow() / 3;

  // Write the number of triangles (4 bytes)
  file.write(reinterpret_cast<char*>(&num_triangles), sizeof(num_triangles));

  // Write triangle normals and verticess
  for (size_t i = 0; i < num_triangles; i++) {
    // Each triangle has 3 rows for vertices and normals
    // Normal vector (one normal for the whole triangle)
    float normal[3] = {static_cast<float>(normals(i, 0)),
                       static_cast<float>(normals(i, 1)),
                       static_cast<float>(normals(i, 2))};
    file.write(reinterpret_cast<char*>(normal), 3 * sizeof(float));

    // Write the triangle vertices
    for (size_t j = 0; j < 3; j++) {
      float vertex[3] = {static_cast<float>(vertices(3 * i + j, 0)),
                         static_cast<float>(vertices(3 * i + j, 1)),
                         static_cast<float>(vertices(3 * i + j, 2))};
      file.write(reinterpret_cast<char*>(vertex), 3 * sizeof(float));
    }

    // Write the attribute byte count (2 bytes, usually 0)
    unsigned short attribute = 0;
    file.write(reinterpret_cast<char*>(&attribute), sizeof(attribute));
  }

  // Close the file
  file.close();
}

//' @title Read OBJ
//'
//' @description Import leaf meshes from QSM-FaNNI
//'
//' @param filename string
//' @param format string
//' @return ply
//'
//' @noRd
//'
// [[Rcpp::export]]
NumericMatrix read_obj(std::string filename, std::string format) {
  if (format == "obj") {
    std::ifstream file(filename);

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
  } else if (format == "obj_ext") {
    std::ifstream file(filename);
    std::string line;
    std::vector<std::vector<double>> base_vertices;
    std::vector<std::vector<double>> transformedVertices;
    std::vector<std::vector<int>> faces;
    int vertex_offset = 0;

    // Read the file line by line
    while (std::getline(file, line)) {
      std::istringstream iss(line);
      std::string type;
      iss >> type;

      if (type == "v") {
        // Base vertex geometry
        double x, y, z;
        iss >> x >> y >> z;
        base_vertices.push_back({x, y, z});
      } else if (type == "L") {
        // Leaf transformation parameters
        std::vector<double> L_params(15);
        for (int i = 0; i < 15; ++i) {
          iss >> L_params[i];
        }

        // Extract transformation parameters
        NumericVector leaf_start = NumericVector::create(L_params[3], L_params[4], L_params[5]);
        NumericVector leaf_direction = NumericVector::create(L_params[6], L_params[7], L_params[8]);
        NumericVector leaf_normal = NumericVector::create(L_params[9], L_params[10], L_params[11]);
        NumericVector leaf_scale = NumericVector::create(L_params[12], L_params[13], L_params[14]);

        // Normalize direction and normal vectors
        double dir_length = norm(leaf_direction);
        leaf_direction = NumericVector::create(
          leaf_direction[0] / dir_length,
          leaf_direction[1] / dir_length,
          leaf_direction[2] / dir_length
        );

        double normal_length = norm(leaf_normal);
        leaf_normal = NumericVector::create(
          leaf_normal[0] / normal_length,
          leaf_normal[1] / normal_length,
          leaf_normal[2] / normal_length
        );

        // Compute the coordinate system
        NumericVector E1 = cross_product(leaf_normal, leaf_direction);
        NumericVector E2 = leaf_direction;
        NumericVector E3 = leaf_normal;

        // Transform each base vertex
        for (const auto& vertex : base_vertices) {
          // Scale
          NumericVector scaled_vertex = NumericVector::create(
            vertex[0] * leaf_scale[0],
            vertex[1] * leaf_scale[1],
            vertex[2] * leaf_scale[2]
          );

          // Rotate
          double x_new = scaled_vertex[0] * E1[0] + scaled_vertex[1] * E1[1] + scaled_vertex[2] * E1[2];
          double y_new = scaled_vertex[0] * E2[0] + scaled_vertex[1] * E2[1] + scaled_vertex[2] * E2[2];
          double z_new = scaled_vertex[0] * E3[0] + scaled_vertex[1] * E3[1] + scaled_vertex[2] * E3[2];

          // Translate
          x_new += leaf_start[0];
          y_new += leaf_start[1];
          z_new += leaf_start[2];

          // Add to the result
          transformedVertices.push_back({x_new, y_new, z_new});
        }

        // Create triangular faces for this leaf
        // Assuming the base vertices form a triangle fan or strip
        for (size_t i = 1; i < base_vertices.size() - 1; ++i) {
          faces.push_back({
            static_cast<int>(vertex_offset),
            static_cast<int>(vertex_offset + i),
            static_cast<int>(vertex_offset + i + 1)
          });
        }

        // Update vertex offset
        vertex_offset += static_cast<int>(base_vertices.size());
      }
    }

    file.close();

    // Create triangles matrix
    size_t num_faces = faces.size();
    NumericMatrix triangles(3 * num_faces, 3);

    // Fill triangles matrix
    for (size_t i = 0; i < num_faces; ++i) {
      int v1 = faces[i][0];
      int v2 = faces[i][1];
      int v3 = faces[i][2];

      triangles(3 * i, 0) = transformedVertices[v1][0];
      triangles(3 * i, 1) = transformedVertices[v1][1];
      triangles(3 * i, 2) = transformedVertices[v1][2];

      triangles(3 * i + 1, 0) = transformedVertices[v2][0];
      triangles(3 * i + 1, 1) = transformedVertices[v2][1];
      triangles(3 * i + 1, 2) = transformedVertices[v2][2];

      triangles(3 * i + 2, 0) = transformedVertices[v3][0];
      triangles(3 * i + 2, 1) = transformedVertices[v3][1];
      triangles(3 * i + 2, 2) = transformedVertices[v3][2];
    }

    return triangles;
  } else {
    return NumericMatrix(0, 3);
  }
}
