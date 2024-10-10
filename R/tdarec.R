#' S3 methods for tracking which additional packages are needed for steps.
#'
#' Recipe-adjacent packages always list themselves as a required package so that
#' the steps can function properly within parallel processing schemes.
#' @param x A recipe step.
#' @return A character vector.
#' @rdname required_pkgs.tdarec
#' @keywords internal
#' @export
required_pkgs.step_tdarec <- function(x, ...) {
  c("tdarec")
}
