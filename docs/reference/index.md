# Package index

## ‘tdarec’ package

A ‘recipes’ and ‘dials’ extension for persistent homology and its
vectorizations.

- [`tdarec-package`](tdarec.md) [`tdarec`](tdarec.md) :

  A *recipes* and *dials* Extension for Persistent Homology

## Data sets

Data sets provided to illustrate pre-processing steps.

- [`mnist_train`](mnist.md) [`mnist_test`](mnist.md) : MNIST handwritten
  digits

## Recipe steps to precede persistent homology

Functions that specify pre-processing steps to prepare data sets for
persistent homology.

- [`step_blur()`](step_blur.md) : Blur raster data

## Recipe steps to compute persistent homology

Functions that specify pre-processing steps to compute persistent
homology of data sets.

- [`step_pd_degree()`](step_pd_degree.md) : Separate persistent pairs by
  homological degree
- [`step_pd_point_cloud()`](step_pd_point_cloud.md) : Persistent
  homology of point clouds
- [`step_pd_raster()`](step_pd_raster.md) : Persistent homology of
  raster data (images)

## Recipe steps to vectorize persistent homology

Functions that specify pre-processing steps to vectorize persistent
homology.

- [`step_vpd_algebraic_functions()`](step_vpd_algebraic_functions.md) :
  Algebraic Functions Vectorization of Persistent Homology
- [`step_vpd_betti_curve()`](step_vpd_betti_curve.md) : Betti Curve
  Vectorization of Persistent Homology
- [`step_vpd_complex_polynomial()`](step_vpd_complex_polynomial.md) :
  Complex Polynomial Vectorization of Persistent Homology
- [`step_vpd_descriptive_statistics()`](step_vpd_descriptive_statistics.md)
  : Descriptive Statistics Vectorization of Persistent Homology
- [`step_vpd_euler_characteristic_curve()`](step_vpd_euler_characteristic_curve.md)
  : Euler Characteristic Curve Vectorization of Persistent Homology
- [`step_vpd_normalized_life_curve()`](step_vpd_normalized_life_curve.md)
  : Normalized Life Curve Vectorization of Persistent Homology
- [`step_vpd_persistence_block()`](step_vpd_persistence_block.md) :
  Persistence Block Vectorization of Persistent Homology
- [`step_vpd_persistence_image()`](step_vpd_persistence_image.md) :
  Persistence Image Vectorization of Persistent Homology
- [`step_vpd_persistence_landscape()`](step_vpd_persistence_landscape.md)
  : Persistence Landscape Vectorization of Persistent Homology
- [`step_vpd_persistence_silhouette()`](step_vpd_persistence_silhouette.md)
  : Persistence Silhouette Vectorization of Persistent Homology
- [`step_vpd_persistent_entropy_summary()`](step_vpd_persistent_entropy_summary.md)
  : Persistent Entropy Summary Vectorization of Persistent Homology
- [`step_vpd_tent_template_functions()`](step_vpd_tent_template_functions.md)
  : Tent Template Functions Vectorization of Persistent Homology
- [`step_vpd_tropical_coordinates()`](step_vpd_tropical_coordinates.md)
  : Tropical Coordinates Vectorization of Persistent Homology

## Dials for the recipe steps

Dials to control the tunable parameters of pre-processing steps.

- [`blur_sigmas()`](blur_sigmas.md) : Standard deviation of Gaussian
  blur
- [`num_coef()`](vpd-dials.md) [`poly_type()`](vpd-dials.md)
  [`img_sigma()`](vpd-dials.md) [`num_levels()`](vpd-dials.md)
  [`weight_func_pl()`](vpd-dials.md) [`bandwidth()`](vpd-dials.md)
  [`weight_power()`](vpd-dials.md) [`num_bars()`](vpd-dials.md)
  [`num_bins()`](vpd-dials.md) [`tent_shift()`](vpd-dials.md) : Tune
  Vectorizations of Persistent Homology

## Summarize data and finalize parameters

Helper functions to summarize topological data and finalizers for
topological transformation parameters.

- [`blur()`](blur.md) : Gaussian blur of an array
- [`ph_dim()`](vpd-summarizers.md) [`pairs_min()`](vpd-summarizers.md)
  [`pairs_max()`](vpd-summarizers.md)
  [`birth_range()`](vpd-summarizers.md)
  [`pers_max()`](vpd-summarizers.md) [`pers_min()`](vpd-summarizers.md)
  [`pers_range()`](vpd-summarizers.md)
  [`life_support()`](vpd-summarizers.md) : Summarize topological data
- [`get_blur_range()`](hom_degree.md) [`hom_degree()`](hom_degree.md)
  [`max_hom_degree()`](hom_degree.md) [`get_hom_range()`](hom_degree.md)
  : (Maximum) topological dimension or homological degree
- [`get_pairs_max()`](vpd-finalizers.md)
  [`get_pers_max_frac()`](vpd-finalizers.md)
  [`get_pers_min_mult()`](vpd-finalizers.md) : Finalizers for persistent
  homology vectorizations
