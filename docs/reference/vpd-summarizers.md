# Summarize topological data

These miscellaneous functions are used by various `get_*_range()`
functions to
[finalize](https://dials.tidymodels.org/reference/finalize.html)
hyperparameter ranges.

## Usage

``` r
ph_dim(x)

# Default S3 method
ph_dim(x)

# S3 method for class 'matrix'
ph_dim(x)

# S3 method for class 'array'
ph_dim(x)

# S3 method for class 'data.frame'
ph_dim(x)

# S3 method for class 'dist'
ph_dim(x)

# S3 method for class 'ts'
ph_dim(x)

pairs_min(x, hom_degrees)

# Default S3 method
pairs_min(x, hom_degrees)

# S3 method for class 'matrix'
pairs_min(x, hom_degrees)

# S3 method for class 'data.frame'
pairs_min(x, hom_degrees)

# S3 method for class 'diagram'
pairs_min(x, hom_degrees)

# S3 method for class 'PHom'
pairs_min(x, hom_degrees)

# S3 method for class 'persistence'
pairs_min(x, hom_degrees)

pairs_max(x, hom_degrees)

# Default S3 method
pairs_max(x, hom_degrees)

# S3 method for class 'matrix'
pairs_max(x, hom_degrees)

# S3 method for class 'data.frame'
pairs_max(x, hom_degrees)

# S3 method for class 'diagram'
pairs_max(x, hom_degrees)

# S3 method for class 'PHom'
pairs_max(x, hom_degrees)

# S3 method for class 'persistence'
pairs_max(x, hom_degrees)

birth_range(x, hom_degree)

# Default S3 method
birth_range(x, hom_degree)

# S3 method for class 'matrix'
birth_range(x, hom_degree)

# S3 method for class 'data.frame'
birth_range(x, hom_degree)

# S3 method for class 'diagram'
birth_range(x, hom_degree)

# S3 method for class 'PHom'
birth_range(x, hom_degree)

# S3 method for class 'persistence'
birth_range(x, hom_degree)

pers_max(x, hom_degree)

# Default S3 method
pers_max(x, hom_degree)

# S3 method for class 'matrix'
pers_max(x, hom_degree)

# S3 method for class 'data.frame'
pers_max(x, hom_degree)

# S3 method for class 'diagram'
pers_max(x, hom_degree)

# S3 method for class 'PHom'
pers_max(x, hom_degree)

# S3 method for class 'persistence'
pers_max(x, hom_degree)

pers_min(x, hom_degree)

# Default S3 method
pers_min(x, hom_degree)

# S3 method for class 'matrix'
pers_min(x, hom_degree)

# S3 method for class 'data.frame'
pers_min(x, hom_degree)

# S3 method for class 'diagram'
pers_min(x, hom_degree)

# S3 method for class 'PHom'
pers_min(x, hom_degree)

# S3 method for class 'persistence'
pers_min(x, hom_degree)

pers_range(x, hom_degree)

# Default S3 method
pers_range(x, hom_degree)

# S3 method for class 'matrix'
pers_range(x, hom_degree)

# S3 method for class 'data.frame'
pers_range(x, hom_degree)

# S3 method for class 'diagram'
pers_range(x, hom_degree)

# S3 method for class 'PHom'
pers_range(x, hom_degree)

# S3 method for class 'persistence'
pers_range(x, hom_degree)

life_support(x, hom_degree)

# Default S3 method
life_support(x, hom_degree)

# S3 method for class 'matrix'
life_support(x, hom_degree)

# S3 method for class 'data.frame'
life_support(x, hom_degree)

# S3 method for class 'diagram'
life_support(x, hom_degree)

# S3 method for class 'PHom'
life_support(x, hom_degree)

# S3 method for class 'persistence'
life_support(x, hom_degree)
```

## Arguments

- x:

  Persistence data in a recognizable format.

- hom_degree, hom_degrees:

  Integer (vector) of homological degree(s).

## Value

A vector of one or two numeric values.

## Details

The functions compute the following summaries:

- `ph_dim()`: Dimension of a data set for purposes of PH

- `pairs_min()`: Minimum number of persistent pairs of any degree

- `pairs_max()`: Maximum number of persistent pairs of any degree

- `birth_range()`: Range of finite birth values for a given degree

- `pers_max()`: Maximum positive finite persistence for a given degree

- `pers_min()`: Minimum positive finite persistence for a given degree

- `pers_range()`: Range of positive finite persistence for a given
  degree

- `life_support()`: Range of union of birth–death ranges for a given
  degree
