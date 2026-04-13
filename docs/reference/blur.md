# Gaussian blur of an array

This function takes a numeric array of any dimension as input and
returns a blurred array of the same dimensions as output.

## Usage

``` r
blur(
  x,
  xmin = 0,
  xmax = 2^ceiling(log(max(x + 1), 2)) - 1,
  sigma = max(dim(x))/2^(length(dim(x)) + 1)
)
```

## Arguments

- x:

  a numerical 'array' (including 'matrix')

- xmin:

  the smallest possible value in `x`; defaults to 0

- xmax:

  the largest possible value in `x`; defaults to the smallest integer
  \\(2^k - 1 \leq \code{max(x)}\\)

- sigma:

  the standard deviation of the gaussian distribution with which to
  convolve `x`; defaults to \\(\code{max(dim(x))} / 2^{D+1}\\), where
  \\(D\\) is the dimensionality of `x`

## Value

An array of the same dimensions as `x`.

## Details

This function is adapted from `spatstat.explore::blur()`, part of the
[spatstat package collection](https://spatstat.org/).

The procedure takes the following steps:

1.  Rescale the value range from \\(\[\code{xmin},\code{xmax}\]\\) to
    \\(\[0,1\]\\).

2.  Convolve `x` with \\(N(0,\code{sigma}^2)\\).

3.  Rescale the result back to the original value range.

## Examples

``` r
square <- matrix(byrow = TRUE, nrow = 6L, c(
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 1, 1, 1, 0, 0, 0,
  0, 1, 0, 0, 1, 1, 1, 0,
  0, 1, 0, 0, 1, 0, 1, 0,
  0, 1, 1, 1, 1, 1, 1, 0,
  0, 0, 0, 0, 0, 0, 0, 0
))
square_blur <- blur(square)
image(t(square))

image(t(square_blur))
```
