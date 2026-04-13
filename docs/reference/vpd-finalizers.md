# Finalizers for persistent homology vectorizations

These functions take a persistent homology vectorization parameter
object and modify the
[`dials::unknown()`](https://dials.tidymodels.org/reference/unknown.html)
parts of `ranges` based on a data set and heuristics used in inaugural
studies.

## Usage

``` r
get_pairs_max(object, x, hom_degrees = NULL, ...)

get_pers_max_frac(
  object,
  x,
  hom_degree = NULL,
  log_vals = TRUE,
  frac = 1/100,
  ...
)

get_pers_min_mult(
  object,
  x,
  hom_degree = NULL,
  log_vals = TRUE,
  mult = 100,
  ...
)
```

## Arguments

- object:

  A `param` object or a list of `param` objects.

- x:

  Persistence data in a recognizable format.

- ...:

  Other arguments to pass to the underlying parameter finalizer
  functions.

- hom_degree, hom_degrees:

  Integer (vector) of homological degree(s).

- log_vals:

  A logical: should the ranges be set on the log10 scale?

- frac:

  A double for the fraction of the data to be used for the upper bound.
  For
  [`get_n_frac_range()`](https://dials.tidymodels.org/reference/finalize.html)
  and
  [`get_batch_sizes()`](https://dials.tidymodels.org/reference/get_batch_sizes.html),
  a vector of two fractional values are required.

- mult:

  A double for the multiple of the data to be used for the lower bound.

## Value

An updated `param` object or a list of updated `param` objects depending
on what is provided in `object`.

## Details

`get_pairs_max()` sets the upper bound to the maximum number of
persistent pairs.

`get_pers_max_frac()` sets both bounds to fractions of the maximum
finite persistence (lifespan). A single number is used as the lower
bound fraction and takes the upper bound fraction to be 1.

`get_pers_min_mult()` sets both bounds to multiples of the minimum
positive persistence (lifespan). A single number is used as the upper
bound multiple and takes the lower bound multiple to be 1.
