#' @title Gaussian blur of an array
#'
#' @description This function takes a numeric array of any dimension as input
#'   and returns a blurred array of the same dimensions as output.
#'
#' @param x a numerical 'array' (including 'matrix')
#' @param xmin the smallest possible value in `x`; defaults to 0
#' @param xmax the largest possible value in `x`; defaults to the smallest
#'   integer \eqn{2^k - 1 \leq \code{max(x)}}
#' @param sigma the standard deviation of the gaussian distribution with which
#'   to convolve `x`; defaults to \eqn{\code{max(dim(x))} / 2^{D+1}}, where
#'   \eqn{D} is the dimensionality of `x`
#'
#' @details This function is adapted from `spatstat.explore::blur()`, part of
#'   the [spatstat package collection](https://spatstat.org/).
#'
#'   The procedure takes the following steps:
#' \enumerate{
#' \item Rescale the value range
#'       from \eqn{[\code{xmin},\code{xmax}]} to \eqn{[0,1]}.
#' \item Convolve `x` with \eqn{N(0,\code{sigma}^2)}.
#' \item Rescale the result back to the original value range.
#' }
#'
#' @examples
#' square <- matrix(byrow = TRUE, nrow = 6L, c(
#'   0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 1, 1, 1, 1, 0, 0, 0,
#'   0, 1, 0, 0, 1, 1, 1, 0,
#'   0, 1, 0, 0, 1, 0, 1, 0,
#'   0, 1, 1, 1, 1, 1, 1, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0
#' ))
#' square_blur <- blur(square)
#' image(t(square))
#' image(t(square_blur))
#' @export
blur <- function(
    x,
    xmin = 0,
    xmax = 2 ^ ceiling(log(max(x + 1), 2)) - 1,
    sigma = max(dim(x)) / 2 ^ (length(dim(x)) + 1)
) {
  
  # ensure that `x` is an array
  x <- as.array(x)
  # get dimensions of `x`
  d <- dim(x)
  
  # scale `x`
  x[] <- (x[] - xmin) / (xmax - xmin)
  
  # compute the gaussian kernel for the dimensionality and dimensions of `x`
  ker <- 
    stats::dnorm(c(seq(0, d[1] / 2 - 1), floor(seq(d[1]/2, 1/2))), sd = sigma)
  if (length(d) > 1) for (i in seq(2, length(d))) {
    ker <- outer(
      ker,
      stats::dnorm(c(seq(0, d[i]/2 - 1), floor(seq(d[i]/2, 1/2))), sd = sigma),
      "*"
    )
  }
  # normalize the kernel
  ker <- ker / sum(ker)
  
  # compute fast fourier transforms of `x` and of `ker`
  x_fft <- stats::fft(x)
  ker_fft <- stats::fft(ker)
  # invert the product of the fourier transforms
  x_blur <- Re(stats::fft(x_fft * ker_fft, inverse = TRUE) / length(x))
  
  # de-scale `x_blur`
  x_blur[] <- x_blur[] * (xmax - xmin) + xmin
  # return `x_blur`
  x_blur
}
