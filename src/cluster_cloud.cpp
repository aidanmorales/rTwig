#include <RcppArmadillo.h>
#include "nanoflann.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace nanoflann;

// nanoflann struct
struct PointCloud {
  arma::mat points;  // This will hold the cloud_ref (x, y, z coordinates)

  inline size_t kdtree_get_point_count() const { return points.n_rows; }

  // Returns the distance between the i-th and j-th points
  inline double kdtree_distance(const double *p1, const size_t idx_p2, size_t /*size*/) const {
    const arma::rowvec p2 = points.row(idx_p2);
    return std::pow(p1[0] - p2[0], 2) + std::pow(p1[1] - p2[1], 2) + std::pow(p1[2] - p2[2], 2);
  }

  // Returns the dim'th component of the idx'th point in the point cloud
  inline double kdtree_get_pt(const size_t idx, int dim) const { return points(idx, dim); }

  // Optional bounding-box computation: return false to default to a standard bbox computation loop.
  template <class BBOX> bool kdtree_get_bbox(BBOX & /*bb*/) const { return false; }
};

// [[Rcpp::export]]
arma::vec assign_cloud_ids(const arma::mat& cloud_ref, const arma::mat& cloud) {
  PointCloud pc1;
  pc1.points = cloud_ref.head_cols(3);

  // Build KDTree
  typedef KDTreeSingleIndexAdaptor<L2_Simple_Adaptor<double, PointCloud>, PointCloud, 3, unsigned int> KDTree;
  KDTree tree(3, pc1, KDTreeSingleIndexAdaptorParams(10));  // leaf size of 10
  tree.buildIndex();

  arma::vec ids(cloud.n_rows);

  for (arma::uword i = 0; i < cloud.n_rows; ++i) {
    arma::rowvec p2 = cloud.row(i);
    unsigned int closest_index;
    double out_dist_sqr;

    // Perform nearest neighbor search
    tree.knnSearch(p2.memptr(), 1, &closest_index, &out_dist_sqr);

    ids(i) = cloud_ref(closest_index, 3);
  }

  return ids;
}
