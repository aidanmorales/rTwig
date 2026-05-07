#include <Rcpp.h>
using namespace Rcpp;

//' @title Generate Mesh
//'
//' @description Generate cylinder mesh for rgl::tmesh3d
//'
//' @param start cylinder starts
//' @param axis cylinder axes
//' @param length cylinder lengths
//' @param radius cylinder radii
//' @param facets number of cylinder facets
//' @param caps should individual cylinder ends be capped?
//' @param color hex color per cylinder
//' @return List with vertices, indices, and face_colors
//'
//' @noRd
//'
// [[Rcpp::export]]
List generate_cylinder_mesh(
   NumericMatrix start,
   NumericMatrix axis,
   NumericVector length,
   NumericVector radius,
   int facets,
   bool caps = false,
   CharacterVector color = CharacterVector()
) {
 int n = start.nrow();

 bool use_color = color.size() == n;

 int verts_per_cyl = caps ? facets * 2 + 2 : facets * 2;
 int faces_per_cyl = caps ? facets * 4 : facets * 2;

 NumericMatrix vertices(3, n * verts_per_cyl);
 IntegerMatrix indices(3, n * faces_per_cyl);
 IntegerMatrix face_colors(n * faces_per_cyl, 3);

 std::vector<double> ct(facets);
 std::vector<double> st(facets);

 for (int j = 0; j < facets; j++) {
   double theta = 2.0 * M_PI * j / facets;
   ct[j] = std::cos(theta);
   st[j] = std::sin(theta);
 }

 int v_out = 0;
 int f_out = 0;

 for (int i = 0; i < n; i++) {
   double sx = start(i, 0);
   double sy = start(i, 1);
   double sz = start(i, 2);

   double ax = axis(i, 0);
   double ay = axis(i, 1);
   double az = axis(i, 2);

   double an = std::sqrt(ax * ax + ay * ay + az * az);

   if (an < 1e-12) {
     ax = 0.0;
     ay = 0.0;
     az = 1.0;
     an = 1.0;
   }

   ax /= an;
   ay /= an;
   az /= an;

   double rx;
   double ry;
   double rz;

   if (std::abs(az) < 0.9) {
     rx = -ay;
     ry = ax;
     rz = 0.0;
   } else {
     rx = 0.0;
     ry = -az;
     rz = ay;
   }

   double rn = std::sqrt(rx * rx + ry * ry + rz * rz);

   if (rn < 1e-12) {
     rx = 1.0;
     ry = 0.0;
     rz = 0.0;
     rn = 1.0;
   }

   rx /= rn;
   ry /= rn;
   rz /= rn;

   double bx = ay * rz - az * ry;
   double by = az * rx - ax * rz;
   double bz = ax * ry - ay * rx;

   double tx = sx + ax * length[i];
   double ty = sy + ay * length[i];
   double tz = sz + az * length[i];

   int rr = 255;
   int gg = 255;
   int bb = 255;

   if (use_color && !CharacterVector::is_na(color[i])) {
     std::string col = as<std::string>(color[i]);

     if (col.size() >= 7 && col[0] == '#') {
       auto hv = [](char c) {
         if (c >= '0' && c <= '9') return c - '0';
         if (c >= 'a' && c <= 'f') return 10 + c - 'a';
         if (c >= 'A' && c <= 'F') return 10 + c - 'A';
         return 0;
       };

       rr = 16 * hv(col[1]) + hv(col[2]);
       gg = 16 * hv(col[3]) + hv(col[4]);
       bb = 16 * hv(col[5]) + hv(col[6]);
     }
   }

   int base_start = v_out + 1;

   for (int j = 0; j < facets; j++) {
     double ox = radius[i] * (ct[j] * rx + st[j] * bx);
     double oy = radius[i] * (ct[j] * ry + st[j] * by);
     double oz = radius[i] * (ct[j] * rz + st[j] * bz);

     vertices(0, v_out) = sx + ox;
     vertices(1, v_out) = sy + oy;
     vertices(2, v_out) = sz + oz;
     v_out++;

     vertices(0, v_out) = tx + ox;
     vertices(1, v_out) = ty + oy;
     vertices(2, v_out) = tz + oz;
     v_out++;
   }

   for (int j = 0; j < facets; j++) {
     int j2 = (j + 1) % facets;

     int b0 = base_start + j * 2;
     int t0 = b0 + 1;
     int b1 = base_start + j2 * 2;
     int t1 = b1 + 1;

     indices(0, f_out) = b0;
     indices(1, f_out) = t0;
     indices(2, f_out) = t1;
     face_colors(f_out, 0) = rr;
     face_colors(f_out, 1) = gg;
     face_colors(f_out, 2) = bb;
     f_out++;

     indices(0, f_out) = b0;
     indices(1, f_out) = t1;
     indices(2, f_out) = b1;
     face_colors(f_out, 0) = rr;
     face_colors(f_out, 1) = gg;
     face_colors(f_out, 2) = bb;
     f_out++;
   }

   if (caps) {
     int base_center = v_out + 1;

     vertices(0, v_out) = sx;
     vertices(1, v_out) = sy;
     vertices(2, v_out) = sz;
     v_out++;

     int top_center = v_out + 1;

     vertices(0, v_out) = tx;
     vertices(1, v_out) = ty;
     vertices(2, v_out) = tz;
     v_out++;

     for (int j = 0; j < facets; j++) {
       int j2 = (j + 1) % facets;

       int b0 = base_start + j * 2;
       int b1 = base_start + j2 * 2;

       int t0 = b0 + 1;
       int t1 = b1 + 1;

       indices(0, f_out) = base_center;
       indices(1, f_out) = b1;
       indices(2, f_out) = b0;
       face_colors(f_out, 0) = rr;
       face_colors(f_out, 1) = gg;
       face_colors(f_out, 2) = bb;
       f_out++;

       indices(0, f_out) = top_center;
       indices(1, f_out) = t0;
       indices(2, f_out) = t1;
       face_colors(f_out, 0) = rr;
       face_colors(f_out, 1) = gg;
       face_colors(f_out, 2) = bb;
       f_out++;
     }
   }
 }

 return List::create(
   Named("vertices") = vertices,
   Named("indices") = indices,
   Named("face_colors") = face_colors
 );
}
