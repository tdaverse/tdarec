# Persistence Image Vectorization of Persistent Homology

The function `step_vpd_persistence_image()` creates a *specification* of
a recipe step that will convert a list-column of 3-column matrices of
persistence data to a list-column of 1-row matrices of vectorizations.

## Usage

``` r
step_vpd_persistence_image(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  hom_degree = 0L,
  xseq = NULL,
  xmin = NULL,
  xmax = NULL,
  xlen = NULL,
  xby = NULL,
  yseq = NULL,
  ymin = NULL,
  ymax = NULL,
  ylen = NULL,
  yby = NULL,
  img_sigma = 1,
  columns = NULL,
  keep_original_cols = TRUE,
  skip = FALSE,
  id = rand_id("vpd_persistence_image")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this step. See
  [`selections()`](https://recipes.tidymodels.org/reference/selections.html)
  for more details.

- role:

  For model terms created by this step, what analysis role should they
  be assigned? By default, the new columns created by this step from the
  original variables will be used as *predictors* in a model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- hom_degree:

  The homological degree of the features to be transformed.

- xseq:

  A discretization grid, as an increasing numeric vector. `xseq`
  overrides the other `x*` parameters with a warning.

- xmin, xmax, xlen, xby:

  Limits and resolution of a discretization grid; specify only one of
  `xlen` and `xby`.

- yseq:

  Combined with `xseq` to form a 2-dimensional discretization grid.

- ymin, ymax, ylen, yby:

  Limits and resolution of a discretization grid; specify only one of
  `ylen` and `yby`.

- img_sigma:

  The standard deviation of the gaussian distribution convolved with
  persistence diagrams to obtain persistence images.

- columns:

  A character string of the selected variable names. This field is a
  placeholder and will be populated once
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) is
  used.

- keep_original_cols:

  A logical to keep the original variables in the output. Defaults to
  `FALSE`.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`bake()`](https://recipes.tidymodels.org/reference/bake.html)? While
  all operations are baked when
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) is run,
  some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = TRUE` as it may affect the computations for subsequent
  operations.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Details

Persistent homology is usually encoded as birth–death pairs (barcodes or
diagrams), but the space of persistence data sets does not satisfy
convenient statistical properties. Such applications as hypothesis
testing and machine learning benefit from transformations of persistence
data, often to Hilbert spaces (vector spaces with inner products and
induced metrics).

## Engine

The persistence image vectorization deploys
[`TDAvec::computePersistenceImage()`](https://rdrr.io/pkg/TDAvec/man/computePersistenceImage.html).
See there for definitions and references.

## Tuning Parameters

This step has 2 tuning parameters:

- `hom_degree`: Homological degree (type: integer, default: `0L`)

- `img_sigma`: Convolved Gaussian standard deviation (type: double,
  default: `1`)

## Examples

``` r
library(recipes)

# inspect vectorized features
volc_dat <- data.frame(image = I(list(volcano / 10)))
recipe(~ image, data = volc_dat) %>% 
  step_pd_raster(image, method = "link_join") %>% 
  step_vpd_persistence_image(image, hom_degree = 1, img_sigma = 1) %>% 
  print() -> volc_rec
#> 
#> ── Recipe ──────────────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> predictor: 1
#> 
#> ── Operations 
#> • persistent features from a cubical filtration of: image
#> • persistence image of: image
print(volc_rec)
#> 
#> ── Recipe ──────────────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> predictor: 1
#> 
#> ── Operations 
#> • persistent features from a cubical filtration of: image
#> • persistence image of: image
volc_rec %>% 
  prep(training = volc_dat) %>% 
  bake(new_data = volc_dat)
#> # A tibble: 1 × 9,802
#>   image  image_pi_1 image_pi_2 image_pi_3 image_pi_4 image_pi_5 image_pi_6
#>   <list>      <dbl>      <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
#> 1 <PHom>   2.58e-52   2.03e-51   1.54e-50   1.12e-49   7.84e-49   5.29e-48
#> # ℹ 9,795 more variables: image_pi_7 <dbl>, image_pi_8 <dbl>, image_pi_9 <dbl>,
#> #   image_pi_10 <dbl>, image_pi_11 <dbl>, image_pi_12 <dbl>, image_pi_13 <dbl>,
#> #   image_pi_14 <dbl>, image_pi_15 <dbl>, image_pi_16 <dbl>, image_pi_17 <dbl>,
#> #   image_pi_18 <dbl>, image_pi_19 <dbl>, image_pi_20 <dbl>, image_pi_21 <dbl>,
#> #   image_pi_22 <dbl>, image_pi_23 <dbl>, image_pi_24 <dbl>, image_pi_25 <dbl>,
#> #   image_pi_26 <dbl>, image_pi_27 <dbl>, image_pi_28 <dbl>, image_pi_29 <dbl>,
#> #   image_pi_30 <dbl>, image_pi_31 <dbl>, image_pi_32 <dbl>, …

# dimension-reduce using vectorized features
data(permeability_qsar, package = "modeldata")
permeability_qsar %>% 
  transform(perm_cut = cut(permeability, breaks = seq(0, 60, 10))) %>% 
  subset(select = -permeability) %>% 
  tidyr::nest(chem_fp = -perm_cut) %>% 
  print() -> perm_dat
#> # A tibble: 6 × 2
#>   perm_cut chem_fp               
#>   <fct>    <list>                
#> 1 (10,20]  <tibble [20 × 1,107]> 
#> 2 (0,10]   <tibble [110 × 1,107]>
#> 3 (20,30]  <tibble [7 × 1,107]>  
#> 4 (30,40]  <tibble [8 × 1,107]>  
#> 5 (40,50]  <tibble [16 × 1,107]> 
#> 6 (50,60]  <tibble [4 × 1,107]>  
recipe(perm_cut ~ chem_fp, data = perm_dat) %>% 
  step_pd_point_cloud(chem_fp, max_hom_degree = 2) %>% 
  step_vpd_persistence_image(chem_fp, hom_degree = 1, img_sigma = 1) %>% 
  step_pca(starts_with("chem_fp_"), num_comp = 2) %>%
  print() -> perm_rec
#> 
#> ── Recipe ──────────────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> outcome:   1
#> predictor: 1
#> 
#> ── Operations 
#> • persistent features from a Rips filtration of: chem_fp
#> • persistence image of: chem_fp
#> • PCA extraction with: starts_with("chem_fp_")
perm_est <- prep(perm_rec, training = perm_dat)
perm_res <- bake(perm_est, new_data = perm_dat)
# inspect results
tidy(perm_rec)
#> # A tibble: 3 × 6
#>   number operation type                  trained skip  id                       
#>    <int> <chr>     <chr>                 <lgl>   <lgl> <chr>                    
#> 1      1 step      pd_point_cloud        FALSE   FALSE pd_point_cloud_QKjPT     
#> 2      2 step      vpd_persistence_image FALSE   FALSE vpd_persistence_image_Pt…
#> 3      3 step      pca                   FALSE   FALSE pca_4ozuV                
tidy(perm_rec, number = 2)
#> # A tibble: 1 × 3
#>   terms   value id                         
#>   <chr>   <dbl> <chr>                      
#> 1 chem_fp    NA vpd_persistence_image_Pt6Ax
tidy(perm_est, number = 2)
#> # A tibble: 1 × 3
#>   terms   value id                         
#>   <chr>   <dbl> <chr>                      
#> 1 chem_fp    NA vpd_persistence_image_Pt6Ax
# visualize results
with(perm_res, {
  plot(PC1, PC2, type = "n", asp = 1)
  text(PC1, PC2, labels = perm_cut)
})
```
