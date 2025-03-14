#' @title Persistence Surface parameters
#'
#' @description Parameters related to vectorized persistence surfaces (images).
#'
#' @details
#' The persistence surface vectorization deploys
#' [TDAvec::computePersistenceImage()].
#' See there for definitions and references.
#'
#' @importFrom scales transform_log10
#' @inheritParams dials::Laplace
#' @inheritParams dials::finalize
#' @inheritParams step_phom_degree
#' @example inst/examples/ex-param-img-sigma.R
#' @export
img_sigma <- function(
    range = c(unknown(), unknown()), trans = transform_log10()
) {
  new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(img_sigma = "Persistence Surface blur"),
    finalize = get_pers_frac_range
  )
}
