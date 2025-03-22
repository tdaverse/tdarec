
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tdarec

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of {tdarec} is to provide additional preprocessing steps to
[{recipes}](https://github.com/tidymodels/recipes) to compute persistent
homology (PH) and calculate vectorizations of persistence data
(diagrams; PDs).

The current prototype provides one engine to compute PH:

- Vietoris–Rips filtrations of point clouds using
  [{ripserr}](https://github.com/tdaverse/ripserr)

and one engine to vectorize PDs:

- Euler characteristic curves using
  [{TDAvec}](https://github.com/uislambekov/TDAvec).

The goal is to provide all PH and PD vectorization engines published on
CRAN.

## Installation

You can install the development version of tdarec from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("corybrunson/tdarec")
```

## Example

This example uses existing engines in a full Tidyverse workflow to
optimize a simple classification model for point clouds sampled from
different embeddings of the Klein bottle:

``` r
# prepare a Tidymodels session and attach {tdarec}
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.4     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
library(tidymodels)
#> ── Attaching packages ────────────────────────────────────── tidymodels 1.2.0 ──
#> ✔ broom        1.0.7     ✔ rsample      1.2.1
#> ✔ dials        1.4.0     ✔ tune         1.2.1
#> ✔ infer        1.0.7     ✔ workflows    1.1.4
#> ✔ modeldata    1.4.0     ✔ workflowsets 1.1.0
#> ✔ parsnip      1.3.1     ✔ yardstick    1.3.2
#> ✔ recipes      1.1.0     
#> ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
#> ✖ scales::discard() masks purrr::discard()
#> ✖ dplyr::filter()   masks stats::filter()
#> ✖ recipes::fixed()  masks stringr::fixed()
#> ✖ dplyr::lag()      masks stats::lag()
#> ✖ yardstick::spec() masks readr::spec()
#> ✖ recipes::step()   masks stats::step()
#> • Learn how to get started at https://www.tidymodels.org/start/
library(tdarec)

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
  # mutate(embedding = factor(embedding)) |> 
  print() -> klein_data
#> # A tibble: 48 × 2
#>    embedding sample        
#>    <chr>     <list>        
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

# partition the data
klein_split <- initial_split(klein_data, prop = .8)
klein_train <- training(klein_split)
klein_test <- testing(klein_split)
klein_folds <- vfold_cv(klein_train, v = 3L)

# specify a pre-processing recipe
scale_seq <- seq(0, 3, by = .05)
recipe(embedding ~ sample, data = klein_train) |> 
  step_phom_point_cloud(
    sample, max_hom_degree = tune("vr_degree"),
    keep_original_cols = FALSE
  ) |> 
  step_vpd_euler_characteristic_curve(
    sample_phom, xseq = scale_seq,
    keep_original_cols = FALSE
  ) |> 
  print() -> klein_rec
#> 
#> ── Recipe ──────────────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> outcome:   1
#> predictor: 1
#> 
#> ── Operations
#> Persistent features from a Rips filtration of sample
#> • Euler characteristic curve of: sample_phom

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

# generate a hyperparameter tuning grid
klein_rec_grid <- grid_regular(
  extract_parameter_set_dials(klein_rec), levels = 3,
  filter = c(vr_degree > 0)
)
klein_lm_grid <- grid_regular(
  extract_parameter_set_dials(klein_lm), levels = 5
)
klein_grid <- merge(klein_rec_grid, klein_lm_grid)

# optimize the model performance
klein_res <- tune_grid(
  klein_lm,
  preprocessor = klein_rec,
  resamples = klein_folds,
  grid = klein_grid,
  metrics = metric_set(roc_auc, pr_auc)
)
klein_res |> 
  collect_metrics()
#> # A tibble: 20 × 8
#>         penalty vr_degree .metric .estimator  mean     n std_err .config        
#>           <dbl>     <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>          
#>  1 0.0000000001         1 pr_auc  binary     0.918     3  0.0817 Preprocessor1_…
#>  2 0.0000000001         1 roc_auc binary     0.944     3  0.0556 Preprocessor1_…
#>  3 0.0000000316         1 pr_auc  binary     0.918     3  0.0817 Preprocessor1_…
#>  4 0.0000000316         1 roc_auc binary     0.944     3  0.0556 Preprocessor1_…
#>  5 0.00001              1 pr_auc  binary     0.918     3  0.0817 Preprocessor1_…
#>  6 0.00001              1 roc_auc binary     0.944     3  0.0556 Preprocessor1_…
#>  7 0.00316              1 pr_auc  binary     0.918     3  0.0817 Preprocessor1_…
#>  8 0.00316              1 roc_auc binary     0.944     3  0.0556 Preprocessor1_…
#>  9 1                    1 pr_auc  binary     0.686     3  0.0548 Preprocessor1_…
#> 10 1                    1 roc_auc binary     0.5       3  0      Preprocessor1_…
#> 11 0.0000000001         3 pr_auc  binary     0.777     3  0.0614 Preprocessor2_…
#> 12 0.0000000001         3 roc_auc binary     0.852     3  0.0359 Preprocessor2_…
#> 13 0.0000000316         3 pr_auc  binary     0.777     3  0.0614 Preprocessor2_…
#> 14 0.0000000316         3 roc_auc binary     0.852     3  0.0359 Preprocessor2_…
#> 15 0.00001              3 pr_auc  binary     0.777     3  0.0614 Preprocessor2_…
#> 16 0.00001              3 roc_auc binary     0.852     3  0.0359 Preprocessor2_…
#> 17 0.00316              3 pr_auc  binary     0.777     3  0.0614 Preprocessor2_…
#> 18 0.00316              3 roc_auc binary     0.852     3  0.0359 Preprocessor2_…
#> 19 1                    3 pr_auc  binary     0.686     3  0.0548 Preprocessor2_…
#> 20 1                    3 roc_auc binary     0.5       3  0      Preprocessor2_…
klein_res |> 
  select_best(metric = "roc_auc")
#> # A tibble: 1 × 3
#>        penalty vr_degree .config             
#>          <dbl>     <int> <chr>               
#> 1 0.0000000001         1 Preprocessor1_Model1
```

## Code of Conduct

Please note that the tdarec project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
