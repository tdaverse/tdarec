
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tdarec

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/tdarec)](https://CRAN.R-project.org/package=tdarec)
<!-- badges: end -->

The goal of {tdarec} is to provide
[{recipes}](https://cran.r-project.org/package=recipes)-style
preprocessing steps to compute persistent homology (PH) and calculate
vectorizations of persistence diagrams (PDs), and to provide
[{dials}](https://cran.r-project.org/package=dials)-style hyperparameter
tuners to optimize these steps in ML workflows.

You can install the development version of tdarec from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("tdaverse/tdarec")
```

## Design

### Recipe steps

The current version provides two engines to compute PH (more will be
implemented; see [this
issue](https://github.com/tdaverse/tdarec/issues/2) for plans):

- **Vietoris–Rips** filtrations of point clouds (distance matrices or
  coordinate matrices) using
  [{ripserr}](https://github.com/tdaverse/ripserr)
- **cubical** filtrations of rasters (pixelated or voxelated data) using
  {ripserr}

Also included are a pre-processing step to introduce **Gaussian blur**
to rasters and a post-processing step to select PDs for **specific
homological degrees**.

Finally, this version provides steps that deploy the highly efficient
**vectorizations** implemented in
[{TDAvec}](https://github.com/uislambekov/TDAvec). These were written
with {Rcpp} specifically for ML applications.

### Tunable parameters

Most steps come with new tunable parameters, for example the maximum
homological degree of the VR filtration and the number of levels in
persistence landscapes.

One set of parameters that are conspicuously untunable are the “scale
sequences”—the values at (or intervals over) which each transformed PD
is vectorized. An implementation is underway.

### Data formats and sets

While the most common {recipes} are designed for structured tabular
data, i.e. columns with numeric or categorical entries, almost all data
subjected to machine learning with persistent homology has been in forms
like point clouds or greyscale images that must be stored in
list-columns. All {tdarec} examples use data in this form, and the data
installed with the package is pre-processed for such use.

## Example

This example uses existing engines to optimize a simple classification
model for point clouds sampled from different embeddings of the Klein
bottle. Note also that
[{glmnet}](https://cran.r-project.org/package=glmnet) and
[{tdaunif}](https://cran.r-project.org/package=tdaunif) must be
installed.

### Setup

While not required, we attach Tidyverse and Tidymodels for convenience
(with messages suppressed):

``` r
# prepare a Tidymodels session and attach {tdarec}
library(tidyverse)
library(tidymodels)
library(tdarec)
```

The points are sampled uniformly from one of two Klein bottle
embeddings, determined by a coin toss between the flat and the tube.

``` r
# generate samples from two embeddings
set.seed(20024L)
tibble(embedding = sample(c("flat", "tube"), size = 48, replace = TRUE)) |> 
  mutate(sample = lapply(embedding, function(emb) {
    switch(
      emb,
      flat = tdaunif::sample_klein_flat(60, sd = .5),
      tube = tdaunif::sample_klein_tube(60, sd = .5)
    )
  })) |> 
  mutate(embedding = factor(embedding)) |>
  print() -> klein_data
#> # A tibble: 48 × 2
#>    embedding sample        
#>    <fct>     <list>        
#>  1 tube      <dbl [60 × 4]>
#>  2 tube      <dbl [60 × 4]>
#>  3 flat      <dbl [60 × 4]>
#>  4 flat      <dbl [60 × 4]>
#>  5 flat      <dbl [60 × 4]>
#>  6 flat      <dbl [60 × 4]>
#>  7 tube      <dbl [60 × 4]>
#>  8 tube      <dbl [60 × 4]>
#>  9 tube      <dbl [60 × 4]>
#> 10 flat      <dbl [60 × 4]>
#> # ℹ 38 more rows
```

We apply a classical partition into 80% training and 20% testing sets
and prepare to perform 3-fold cross-validation on the training set.

``` r
# partition the data
klein_split <- initial_split(klein_data, prop = .8)
klein_train <- training(klein_split)
klein_test <- testing(klein_split)
klein_folds <- vfold_cv(klein_train, v = 3L)
```

In this example, we adopt a common transformation of persistence
diagrams, Euler characteristic curves. For their vectorization, we need
a scale sequence that spans the birth and death times of any persistent
features, and for this we choose a round number larger than the
diameters of both point clouds (based on the sampler documentation) as
an upper bound. Rather than choose *a priori* to use homology up to
degree 0, 1, 2, or 3, we prepare to tune the maximum degree during
optimization.

### Specifications

To prevent the model from using the data set column as a predictor, we
assign it a new role, which is preserved by the persistent homology step
and ignored by the vectorization step (which outputs new predictor
columns).

``` r
# specify a pre-processing recipe
scale_seq <- seq(0, 3, by = .05)
recipe(embedding ~ sample, data = klein_train) |> 
  update_role(sample, new_role = "data set") |> 
  step_pd_point_cloud(sample, max_hom_degree = tune("vr_degree")) |> 
  step_vpd_euler_characteristic_curve(sample, xseq = scale_seq) |> 
  print() -> klein_rec
#> 
#> ── Recipe ──────────────────────────────────────────────────────────────────────
#> 
#> ── Inputs
#> Number of variables by role
#> outcome:  1
#> data set: 1
#> 
#> ── Operations
#> • persistent features from a Rips filtration of: sample
#> • Euler characteristic curve of: sample
```

For simplicity, we choose a common model for ML classification,
penalized logistic regression. We fix the mixture coefficient to use
LASSO rather than ridge regression but prepare the penalty parameter for
tuning.

``` r
# specify a classification model
logistic_reg(penalty = tune(), mixture = 1) |> 
  set_mode("classification") |> 
  set_engine("glmnet") |> 
  print() -> klein_lm
#> Logistic Regression Model Specification (classification)
#> 
#> Main Arguments:
#>   penalty = tune()
#>   mixture = 1
#> 
#> Computational engine: glmnet
```

We then generate a complete hyperparameter tuning grid by crossing the
grids generated for the two unspecified parameters:

``` r
# generate a hyperparameter tuning grid
klein_rec_grid <- grid_regular(
  extract_parameter_set_dials(klein_rec), levels = 3,
  filter = c(vr_degree > 0)
)
klein_lm_grid <- grid_regular(
  extract_parameter_set_dials(klein_lm), levels = 5
)
klein_grid <- merge(klein_rec_grid, klein_lm_grid)
```

### Optimization

We evaluate the model across the hyperparameter grid using
cross-validation, using the area under the sensitivity–specificity (ROC)
curve:

``` r
# optimize the model performance
klein_res <- tune_grid(
  klein_lm,
  preprocessor = klein_rec,
  resamples = klein_folds,
  grid = klein_grid,
  metrics = metric_set(roc_auc)
)
```

From the results, we obtain the best-performing parameter setting:

``` r
klein_res |> 
  select_best(metric = "roc_auc") |> 
  print() -> klein_best
#> # A tibble: 1 × 3
#>        penalty vr_degree .config             
#>          <dbl>     <int> <chr>               
#> 1 0.0000000001         1 Preprocessor1_Model1
```

### Evaluation

This optimal setting includes both the VR homology degree and the GLM
penalty, so both the pre-processing recipe and the predictive model must
be finalized in order to fit the final model to the full training set:

``` r
klein_rec_fin <- klein_rec |> finalize_recipe(klein_best) |> prep()
klein_lm_fin <- klein_lm |> finalize_model(klein_best)
klein_fit <- fit(
  klein_lm_fin,
  formula(klein_rec_fin),
  data = bake(klein_rec_fin, new_data = klein_train)
)
```

Finally, we evaluate the fitted model on the testing set:

``` r
klein_fit |> 
  predict(
    new_data = bake(klein_rec_fin, new_data = klein_test),
    type = "prob"
  ) |> 
  bind_cols(select(klein_test, embedding)) |> 
  roc_auc(truth = embedding, .pred_flat)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc binary          0.92
```

## Contributions

Please note that the tdarec project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

### Generated code

Much of the code exposing {TDAvec} tools to Tidymodels is generated by
elaborate scripts rather than written manually. While maintenance of
these scripts takes effort, it prevents (or at least flags) errors
arising from cascading implications of changes to the original
functions, and it allows simple and rapid package-wide adjustments. If
you see an issue with generated code, please raise an issue to discuss
it before submitting a pull request.

### Acknowledgments

This project was funded by [an ISC grant from the R
Consortium](https://r-consortium.org/all-projects/2024-group-1.html#modular-interoperable-and-extensible-topological-data-analysis-in-r)
and done in coordination with Aymeric Stamm and with guidance from
Bertrand Michel and Paul Rosen. It builds upon the work of and
conversations with Umar Islambekov and Aleksei Luchinsky, authors of
[{TDAvec}](https://github.com/uislambekov/TDAvec). Package development
also benefitted from the support of colleagues in [the Laboratory for
Systems Medicine](https://systemsmedicine.pulmonary.medicine.ufl.edu/)
and [the TDA Seminar](https://tda.math.ufl.edu/) and the use of
equipment at [the University of Florida](https://www.ufl.edu/).
