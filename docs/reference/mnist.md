# MNIST handwritten digits

This is a 1% stratified random sample from the MNIST handwritten digit
data set.

## Usage

``` r
mnist_train; mnist_test
```

## Format

Two data frames of 600 and 100 rows, respectively, and 2 variables:

- `digit`:

  list column of 28 × 28 numeric matrices

- `label`:

  integer digit

## Source

<http://yann.lecun.com/exdb/mnist/>
