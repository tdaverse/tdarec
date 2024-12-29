# Read, sample, and save MNIST handwritten digit training & testing data

library(tidyverse)

# Adapted from the following gists:
# https://gist.github.com/brendano/39760
# https://gist.github.com/daviddalpiaz/ae62ae5ccd0bada4b9acd6dbc9008706

# read image files
read_image_file <- function(filename) {
  ret <- list()
  f <- file(filename, 'rb')
  readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  n <- readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  nrow <- readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  ncol <- readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  x <- readBin(f, 'integer', n = n * nrow * ncol, size = 1, signed = FALSE)
  close(f)
  matrix(x, ncol = nrow * ncol, byrow = TRUE)
}

# read label files
read_label_file <- function(filename) {
  f <- file(filename, 'rb')
  readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  n <- readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  y <- readBin(f, 'integer', n = n, size = 1, signed = FALSE)
  close(f)
  matrix(y)
}

# read images
train_x <- read_image_file("ignore/mnist/emnist-mnist-train-images-idx3-ubyte")
test_x  <- read_image_file("ignore/mnist/emnist-mnist-test-images-idx3-ubyte")

# read labels
train_y <- read_label_file("ignore/mnist/emnist-mnist-train-labels-idx1-ubyte")
test_y  <- read_label_file("ignore/mnist/emnist-mnist-test-labels-idx1-ubyte")

# helper function for matricization
extract_digit <- function(arr784) {
  matrix(as.matrix(arr784), nrow = 28)
}

# helper function for visualization
plot_digit <- function(arr784, col = gray(12:1 / 12), ...) {
  image(extract_digit(arr784), col = col, asp = 1, ...)
}

# view test image
plot_digit(train_x[5, ])

# stratified random sample (indices) of digits
strat_sample_digits <- function(x, y, prop) {
  freq_y <- table(as.vector(y))
  uniq_y <- as.integer(names(freq_y))
  count_y <- as.vector(freq_y)
  samps <- lapply(uniq_y, function(value) {
    which_y <- which(as.vector(y) == value)
    which_y[sample(length(which_y), length(which_y) * prop)]
  })
  sort(unlist(samps))
}

# training and testing data with image matrices and labels
set.seed(984520L)
train_sample <- strat_sample_digits(train_x, train_y, prop = .01)
mnist_train <- tibble(
  digit = lapply(train_sample, \(s) extract_digit(train_x[s, ])),
  label = as.vector(train_y[train_sample, ])
)
test_sample <- strat_sample_digits(test_x, test_y, prop = .01)
mnist_test <- tibble(
  digit = lapply(test_sample, \(s) extract_digit(test_x[s, ])),
  label = as.vector(train_y[test_sample, ])
)

save(
  mnist_train, mnist_test,
  file = here::here("data/mnist.rda"),
  compression_level = 9
)
