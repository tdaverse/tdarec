#' @title Persistence Landscape parameters
#'
#' @description Parameters related to vectorized persistence landscapes.
#'
#' @details
#' The persistence surface vectorization deploys
#' [TDAvec::computePersistenceLandscape()].
#' See there for definitions and references.
#'
#' @inheritParams dials::Laplace
#' @inheritParams dials::finalize
#' @inheritParams step_phom_degree
#' @example inst/examples/ex-param-num-level.R
#' @export
num_level <- function(
    range = c(1L, unknown()), trans = NULL
) {
  new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(num_level = "# Persistence Landscape levels"),
    finalize = get_pairs_max
  )
}
