# Standard deviation of Gaussian blur

The standard deviation of the noise function convolved with array values
to induce blur in raster data.

## Usage

``` r
blur_sigmas(range = c(unknown(), unknown()), trans = transform_log1p())
```

## Arguments

- range:

  A two-element vector holding the *defaults* for the smallest and
  largest possible values, respectively. If a transformation is
  specified, these values should be in the *transformed units*.

- trans:

  A `trans` object from the `scales` package, such as
  [`scales::transform_log10()`](https://scales.r-lib.org/reference/transform_log.html)
  or
  [`scales::transform_reciprocal()`](https://scales.r-lib.org/reference/transform_reciprocal.html).
  If not provided, the default is used which matches the units used in
  `range`. If no transformation, `NULL`.

## Value

A `param` object or list of `param` objects.

## Details

The gaussian blur step deploys [`blur()`](blur.md). See there for
definitions and references.

[`get_blur_range()`](hom_degree.md) varies the parameter logarithmically
from 0 to an order of magnitude greater than the [`blur()`](blur.md)
default.

## Examples

``` r
img_dat <- data.frame(img = I(list(volcano)))

(blur_man <- blur_sigmas(range = c(0, 3)))
#> Gaussian Blur std. dev.s (quantitative)
#> Transformer: log1p [-1, Inf]
#> Range (transformed scale): [0, 3]
grid_regular(blur_man)
#> # A tibble: 3 × 1
#>   blur_sigmas
#>         <dbl>
#> 1        0   
#> 2        3.48
#> 3       19.1 

(blur_fin <- blur_sigmas() %>% get_blur_range(x = img_dat))
#> Gaussian Blur std. dev.s (quantitative)
#> Transformer: log1p [-1, Inf]
#> Range (transformed scale): [1.47, 3.47]
grid_regular(blur_fin)
#> # A tibble: 3 × 1
#>   blur_sigmas
#>         <dbl>
#> 1        3.37
#> 2       10.9 
#> 3       31.3 
```
