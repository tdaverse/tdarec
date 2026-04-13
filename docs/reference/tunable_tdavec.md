# `tunable()` methods for tdavec package

These functions define what parameters *can* be tuned for specific
steps. They also define the recommended objects from
[dials](https://dials.tidymodels.org/reference/dials-package.html) that
can be used to generate new parameter values and other characteristics.

## Usage

``` r
# S3 method for class 'step_vpd_algebraic_functions'
tunable(x, ...)

# S3 method for class 'step_vpd_betti_curve'
tunable(x, ...)

# S3 method for class 'step_vpd_complex_polynomial'
tunable(x, ...)

# S3 method for class 'step_vpd_descriptive_statistics'
tunable(x, ...)

# S3 method for class 'step_vpd_euler_characteristic_curve'
tunable(x, ...)

# S3 method for class 'step_vpd_normalized_life_curve'
tunable(x, ...)

# S3 method for class 'step_vpd_persistence_block'
tunable(x, ...)

# S3 method for class 'step_vpd_persistence_image'
tunable(x, ...)

# S3 method for class 'step_vpd_persistence_landscape'
tunable(x, ...)

# S3 method for class 'step_vpd_persistence_silhouette'
tunable(x, ...)

# S3 method for class 'step_vpd_persistent_entropy_summary'
tunable(x, ...)

# S3 method for class 'step_vpd_tent_template_functions'
tunable(x, ...)

# S3 method for class 'step_vpd_tropical_coordinates'
tunable(x, ...)
```

## Arguments

- x:

  A recipe step object

- ...:

  Not used.

## Value

A tibble (class `tbl_df`).
