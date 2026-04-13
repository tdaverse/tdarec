# S3 methods for tracking which additional packages are needed for steps.

Recipe-adjacent packages always list themselves as a required package so
that the steps can function properly within parallel processing schemes.

## Usage

``` r
# S3 method for class 'step_blur'
required_pkgs(x, ...)

# S3 method for class 'step_pd_degree'
required_pkgs(x, ...)

# S3 method for class 'step_pd_point_cloud'
required_pkgs(x, ...)

# S3 method for class 'step_pd_raster'
required_pkgs(x, ...)

# S3 method for class 'step_tdarec'
required_pkgs(x, ...)

# S3 method for class 'step_vpd_algebraic_functions'
required_pkgs(x, ...)

# S3 method for class 'step_vpd_betti_curve'
required_pkgs(x, ...)

# S3 method for class 'step_vpd_complex_polynomial'
required_pkgs(x, ...)

# S3 method for class 'step_vpd_descriptive_statistics'
required_pkgs(x, ...)

# S3 method for class 'step_vpd_euler_characteristic_curve'
required_pkgs(x, ...)

# S3 method for class 'step_vpd_normalized_life_curve'
required_pkgs(x, ...)

# S3 method for class 'step_vpd_persistence_block'
required_pkgs(x, ...)

# S3 method for class 'step_vpd_persistence_image'
required_pkgs(x, ...)

# S3 method for class 'step_vpd_persistence_landscape'
required_pkgs(x, ...)

# S3 method for class 'step_vpd_persistence_silhouette'
required_pkgs(x, ...)

# S3 method for class 'step_vpd_persistent_entropy_summary'
required_pkgs(x, ...)

# S3 method for class 'step_vpd_tent_template_functions'
required_pkgs(x, ...)

# S3 method for class 'step_vpd_tropical_coordinates'
required_pkgs(x, ...)
```

## Arguments

- x:

  A recipe step.

## Value

A character vector.
