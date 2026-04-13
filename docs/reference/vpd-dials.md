# Tune Vectorizations of Persistent Homology

These tuning functions govern the parameters of vectorizations
implemented in **TDAvec**.

## Usage

``` r
num_coef(range = c(1L, unknown()), trans = NULL)

poly_type(values = c("R", "S", "T"), trans = NULL)

img_sigma(range = c(unknown(), unknown()), trans = transform_log10())

num_levels(range = c(1L, unknown()), trans = NULL)

weight_func_pl(
  values = c("triangle", "epanechnikov", "tricubic"),
  trans = NULL
)

bandwidth(range = c(unknown(), unknown()), trans = transform_log10())

weight_power(range = c(1, 2), trans = NULL)

num_bars(range = c(1L, unknown()), trans = NULL)

num_bins(range = c(2L, 20L), trans = NULL)

tent_shift(range = c(unknown(), unknown()), trans = transform_log10())
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

- values:

  A character string of possible values.

## Value

A `param` object or list of `param` objects.

## Details

The parameter `num_coef` is passed to `m` in
[`TDAvec::computeComplexPolynomial()`](https://rdrr.io/pkg/TDAvec/man/computeComplexPolynomial.html).

The parameter `poly_type` is passed to `polyType` in
[`TDAvec::computeComplexPolynomial()`](https://rdrr.io/pkg/TDAvec/man/computeComplexPolynomial.html).

The parameter `img_sigma` is passed to `sigma` in
[`TDAvec::computePersistenceImage()`](https://rdrr.io/pkg/TDAvec/man/computePersistenceImage.html).

The parameter `num_levels` is passed to `k` in
[`TDAvec::computePersistenceLandscape()`](https://rdrr.io/pkg/TDAvec/man/computePersistenceLandscape.html).

The parameter `weight_func_pl` is passed to `kernel` in
[`TDAvec::computePersistenceLandscape()`](https://rdrr.io/pkg/TDAvec/man/computePersistenceLandscape.html).

The parameter `bandwidth` is passed to `h` in
[`TDAvec::computePersistenceLandscape()`](https://rdrr.io/pkg/TDAvec/man/computePersistenceLandscape.html).

The parameter `weight_power` is passed to `p` in
[`TDAvec::computePersistenceSilhouette()`](https://rdrr.io/pkg/TDAvec/man/computePersistenceSilhouette.html).

The parameter `num_bars` is passed to `r` in
[`TDAvec::computeTropicalCoordinates()`](https://rdrr.io/pkg/TDAvec/man/computeTropicalCoordinates.html).

The parameter `num_bins` is passed to `d` in
[`TDAvec::computeTemplateFunction()`](https://rdrr.io/pkg/TDAvec/man/computeTemplateFunction.html).

The parameter `tent_shift` is passed to `epsilon` in
[`TDAvec::computeTemplateFunction()`](https://rdrr.io/pkg/TDAvec/man/computeTemplateFunction.html).

## Examples

``` r
data.frame(dist = I(list(eurodist, UScitiesD * 1.6))) %>%
  transform(pd = I(lapply(dist, ripserr::vietoris_rips))) %>%
  subset(select = c(pd)) %>%
  print() -> pd_data
#>             pd
#> 1 c(0, 0, ....
#> 2 c(0, 0, ....

# `num_coef` for `step_vpn_complex_polynomial()`

(nc_man <- num_coef(range = c(1L, 3L)))
#> # Polynomial coefficients (quantitative)
#> Range: [1, 3]
grid_regular(nc_man)
#> # A tibble: 3 × 1
#>   num_coef
#>      <int>
#> 1        1
#> 2        2
#> 3        3

# `poly_type` for `step_vpn_complex_polynomial()`

(pt_man <- poly_type(values = c("R", "S")))
#> Type of polynomial (qualitative)
#> 2 possible values include:
#> 'R' and 'S'
grid_regular(pt_man)
#> # A tibble: 2 × 1
#>   poly_type
#>   <chr>    
#> 1 R        
#> 2 S        

# `img_sigma` for `step_vpn_persistence_image()`

(is_man <- img_sigma(range = c(100, 400), trans = NULL))
#> Convolved Gaussian standard deviation (quantitative)
#> Range: [100, 400]
grid_regular(is_man)
#> # A tibble: 3 × 1
#>   img_sigma
#>       <dbl>
#> 1       100
#> 2       250
#> 3       400

(is_dat <- img_sigma() %>% get_pers_max_frac(x = pd_data))
#> Convolved Gaussian standard deviation (quantitative)
#> Transformer: log-10 [1e-100, Inf]
#> Range (transformed scale): [1.15, 3.15]
grid_regular(is_dat)
#> # A tibble: 3 × 1
#>   img_sigma
#>       <dbl>
#> 1      14.1
#> 2     141. 
#> 3    1406. 

(is_hom <- img_sigma() %>% get_pers_max_frac(x = pd_data, hom_degrees = seq(2L)))
#> Convolved Gaussian standard deviation (quantitative)
#> Transformer: log-10 [1e-100, Inf]
#> Range (transformed scale): [1.15, 3.15]
grid_regular(is_hom)
#> # A tibble: 3 × 1
#>   img_sigma
#>       <dbl>
#> 1      14.1
#> 2     141. 
#> 3    1406. 

# `num_levels` for `step_vpn_persistence_landscape()`

(nl_man <- num_levels(range = c(1L, 6L)))
#> # Levels or envelopes (quantitative)
#> Range: [1, 6]
grid_regular(nl_man)
#> # A tibble: 3 × 1
#>   num_levels
#>        <int>
#> 1          1
#> 2          3
#> 3          6

# `weight_func_pl` for `step_vpn_persistence_landscape()`

(wfp_man <- weight_func_pl(values = c("triangle", "tricubic")))
#> Kernel distance weight function (qualitative)
#> 2 possible values include:
#> 'triangle' and 'tricubic'
grid_regular(wfp_man)
#> # A tibble: 2 × 1
#>   weight_func_pl
#>   <chr>         
#> 1 triangle      
#> 2 tricubic      

# `bandwidth` for `step_vpn_persistence_landscape()`

(b_man <- bandwidth(range = c(500, 1500), trans = NULL))
#> Kernel bandwidth (quantitative)
#> Range: [500, 1500]
grid_regular(b_man)
#> # A tibble: 3 × 1
#>   bandwidth
#>       <dbl>
#> 1       500
#> 2      1000
#> 3      1500

(b_dat <- bandwidth() %>% get_pers_max_frac(x = pd_data))
#> Kernel bandwidth (quantitative)
#> Transformer: log-10 [1e-100, Inf]
#> Range (transformed scale): [1.15, 3.15]
grid_regular(b_dat)
#> # A tibble: 3 × 1
#>   bandwidth
#>       <dbl>
#> 1      14.1
#> 2     141. 
#> 3    1406. 

(b_hom <- bandwidth() %>% get_pers_max_frac(x = pd_data, hom_degrees = seq(2L)))
#> Kernel bandwidth (quantitative)
#> Transformer: log-10 [1e-100, Inf]
#> Range (transformed scale): [1.15, 3.15]
grid_regular(b_hom)
#> # A tibble: 3 × 1
#>   bandwidth
#>       <dbl>
#> 1      14.1
#> 2     141. 
#> 3    1406. 

# `weight_power` for `step_vpn_persistence_silhouette()`

(wp_man <- weight_power(range = c(1, 3)))
#> Exponent weight (quantitative)
#> Range: [1, 3]
grid_regular(wp_man)
#> # A tibble: 3 × 1
#>   weight_power
#>          <dbl>
#> 1            1
#> 2            2
#> 3            3

# `num_bars` for `step_vpn_tropical_coordinates()`

(nb_man <- num_bars(range = c(1L, 3L)))
#> # Bars (persistence pairs) (quantitative)
#> Range: [1, 3]
grid_regular(nb_man)
#> # A tibble: 3 × 1
#>   num_bars
#>      <int>
#> 1        1
#> 2        2
#> 3        3

# `num_bins` for `step_vpn_tent_template_functions()`

(nb_man <- num_bins(range = c(5L, 10L)))
#> Discretization grid bins (quantitative)
#> Range: [5, 10]
grid_regular(nb_man)
#> # A tibble: 3 × 1
#>   num_bins
#>      <int>
#> 1        5
#> 2        7
#> 3       10

# `tent_shift` for `step_vpn_tent_template_functions()`

(ts_man <- tent_shift(range = c(100, 200), trans = NULL))
#> Discretization grid shift (quantitative)
#> Range: [100, 200]
grid_regular(ts_man)
#> # A tibble: 3 × 1
#>   tent_shift
#>        <dbl>
#> 1        100
#> 2        150
#> 3        200

(ts_dat <- tent_shift() %>% get_pers_min_mult(x = pd_data))
#> Discretization grid shift (quantitative)
#> Transformer: log-10 [1e-100, Inf]
#> Range (transformed scale): [1.51, 3.51]
grid_regular(ts_dat)
#> # A tibble: 3 × 1
#>   tent_shift
#>        <dbl>
#> 1         32
#> 2        320
#> 3       3200

(ts_hom <- tent_shift() %>% get_pers_min_mult(x = pd_data, hom_degrees = seq(2L)))
#> Discretization grid shift (quantitative)
#> Transformer: log-10 [1e-100, Inf]
#> Range (transformed scale): [1.51, 3.51]
grid_regular(ts_hom)
#> # A tibble: 3 × 1
#>   tent_shift
#>        <dbl>
#> 1         32
#> 2        320
#> 3       3200
```
