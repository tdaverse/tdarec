# (Maximum) topological dimension or homological degree

The degree of the homology group to vectorize, or the degree at which to
stop vectorizing.

## Usage

``` r
get_blur_range(object, x, ...)

hom_degree(range = c(0L, unknown()), trans = NULL)

max_hom_degree(range = c(0L, unknown()), trans = NULL)

get_hom_range(object, x, max_dim = 2L, ...)
```

## Arguments

- object:

  A `param` object or a list of `param` objects.

- x:

  The predictor data. In some cases (see below) this should only include
  numeric data.

- ...:

  Other arguments to pass to the underlying parameter finalizer
  functions. For example, for
  [`get_rbf_range()`](https://dials.tidymodels.org/reference/finalize.html),
  the dots are passed along to
  [`kernlab::sigest()`](https://rdrr.io/pkg/kernlab/man/sigest.html).

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

- max_dim:

  Bound on the maximum dimension determined from the data.

## Value

A `param` object or list of `param` objects.

## Details

Topological features have whole number dimensions that determine the
degrees of homology that encode them. Any finite point cloud will have
finite topological dimension, but most practical applications exploit
features of degree at most 3.

Steps may vectorize features of a single degree (`hom_degree()`) or of
degrees zero through some maximum (`max_hom_degree()`).

In case the (maximum) degree is not provided, `get_hom_range()` queries
each list-column for the maximum dimension of its point cloud and
returns the smaller of this maximum and `max_dim` (which defaults to
`2L`, the highest homological degree of interest in most practical
applications).

## Examples

``` r
# toy data set
klein_sampler <- function(n, prob = .5) {
  if (rbinom(1, 1, prob) == 0) {
    tdaunif::sample_klein_flat(n)
  } else {
    tdaunif::sample_klein_tube(n)
  }
}
sample_data <- data.frame(
  id = LETTERS[seq(4L)],
  sample = I(c(replicate(4L, klein_sampler(60), simplify = FALSE)))
)

# options to calibrate homological degree
hom_degree(range = c(2, 5))
#> Homological Degree (quantitative)
#> Range: [2, 5]
hom_degree() %>% get_hom_range(x = sample_data[, 2, drop = FALSE])
#> Homological Degree (quantitative)
#> Range: [0, 1]
hom_degree() %>% get_hom_range(x = sample_data[, 2, drop = FALSE], max_dim = 5)
#> Homological Degree (quantitative)
#> Range: [0, 3]

# heterogeneous data types
hetero_data <- tibble(dataset = list(mtcars, nhtemp, eurodist, HairEyeColor))
hetero_data %>% 
  mutate(class = vapply(dataset, function(x) class(x)[[1L]], ""))
#> # A tibble: 4 × 2
#>   dataset             class     
#>   <list>              <chr>     
#> 1 <df [32 × 11]>      data.frame
#> 2 <ts [60]>           ts        
#> 3 <dist [210]>        dist      
#> 4 <table [4 × 4 × 2]> table     
get_hom_range(
  hom_degree(),
  hetero_data,
  max_dim = 60
)
#> Homological Degree (quantitative)
#> Range: [0, 20]
```
