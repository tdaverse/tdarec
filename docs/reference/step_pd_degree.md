# Separate persistent pairs by homological degree

The function `step_pd_degree()` creates a *specification* of a recipe
step that will separate data sets of persistent pairs by homological
degree. The input and output must be list-columns.

## Usage

``` r
step_pd_degree(
  recipe,
  ...,
  role = NA_character_,
  trained = FALSE,
  hom_degrees = NULL,
  columns = NULL,
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("pd_degree")
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

- hom_degrees:

  Integer vector of homological degrees.

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

The `hom_degrees` argument sets the homological degrees for which to
return new list-columns. If not `NULL` (the default), it is intersected
with the degrees found in any specified columns of the training data;
otherwise all found degrees are used. This parameter cannot be tuned.

## See also

Other topological feature extraction via persistent homology:
[`step_pd_point_cloud()`](step_pd_point_cloud.md),
[`step_pd_raster()`](step_pd_raster.md)

## Examples

``` r
dat <- data.frame(
  roads = I(list(eurodist, UScitiesD * 1.6)),
  topos = I(list(volcano, 255 - volcano))
)

ph_rec <- recipe(~ ., data = dat) %>% 
  step_pd_point_cloud(roads) %>% 
  step_pd_raster(topos) %>% 
  step_pd_degree(roads, topos)
ph_prep <- prep(ph_rec, training = dat)
(ph_res <- bake(ph_prep, dat))
#> # A tibble: 2 × 4
#>   roads_0         roads_1        topos_0         topos_1       
#>   <list>          <list>         <list>          <list>        
#> 1 <PHom [21 × 3]> <PHom [3 × 3]> <PHom [13 × 3]> <PHom [5 × 3]>
#> 2 <PHom [10 × 3]> <PHom [1 × 3]> <PHom [7 × 3]>  <PHom [1 × 3]>

tidy(ph_rec, number = 3)
#> # A tibble: 2 × 3
#>   terms value id             
#>   <chr> <dbl> <chr>          
#> 1 roads    NA pd_degree_xb20B
#> 2 topos    NA pd_degree_xb20B
tidy(ph_prep, number = 3)
#> # A tibble: 2 × 3
#>   terms value id             
#>   <chr> <dbl> <chr>          
#> 1 roads    NA pd_degree_xb20B
#> 2 topos    NA pd_degree_xb20B

with_degs <- recipe(~ ., data = dat) %>% 
  step_pd_point_cloud(roads) %>% 
  step_pd_raster(topos) %>% 
  step_pd_degree(roads, topos, hom_degrees = c(1, 2))
with_degs <- prep(with_degs, training = dat)
bake(with_degs, dat)
#> # A tibble: 2 × 2
#>   roads_1        topos_1       
#>   <list>         <list>        
#> 1 <PHom [3 × 3]> <PHom [5 × 3]>
#> 2 <PHom [1 × 3]> <PHom [1 × 3]>
```
