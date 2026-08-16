#' get_increment: finalize function for the xby dial
#'
#' @description Uses the data to make sure the grid resolution(named as xby here) 
#'   won't end up too large or too small. pers_min is used as a lower bound so
#'   the grid can resolve the closest pair of features, and the overall span 
#'   was divided by 2^3 to act as an upper bound.
#'
#' @param object the xby dial object being finalized
#' @param x a persistence diagram
#' @param hom_degree which homological degree
#' @export
get_increment <- function(object, x, hom_degree) {
  r <- life_support(x, hom_degree)
  rg <- r[2] - r[1]
  dials::range_set(object, log2(sort(c(pers_min(x, hom_degree), rg / 2^3))))
}

#' xby: dial for grid resolution
#'
#' @description We define the dial xby with unknown limits at this moment, with 
#'   candidate increments set as powers of 2 (trans handles the log2 conversion)
#'   get_increment() works as the finalize function, using the data to make sure 
#'   xby won't end up too large or too small leading to trouble
#'
#' @param range smallest/largest allowed xby, unknown by default since it depends 
#'   on the data
#' @param trans log2 transform, so candidates are searched as powers of 2
#' @export
xby <- function(range = c(unknown(), unknown()), trans = scales::transform_log(base = 2)) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(xbyp = "Grid resolution (as power of 2)"),
    finalize = get_increment
  )
}

#' tunable.step_vpd_persistence_landscape: adds xby as tunable
#'
#' @description Since xby is already a native parameter of
#'   step_vpd_persistence_landscape(), the process is relatively simple tha the 
#'   opearation of t. We just add its dial to the step's overall tunable method.
#'
#' @param x a step_vpd_persistence_landscape object
#' @export
tunable.step_vpd_persistence_landscape <- function(x, ...) {
  tibble::tibble(
    name = c("hom_degree", "num_levels", "weight_func_pl", "bandwidth", "xby"),
    call_info = list(
      list(pkg = "tdarec", fun = "hom_degree", range = c(0L, unknown())),
      list(pkg = "tdarec", fun = "num_levels", range = c(1L, unknown())),
      list(pkg = "tdarec", fun = "weight_func_pl", values = c("triangle", "epanechnikov", "tricubic")),
      list(pkg = "tdarec", fun = "bandwidth", range = c(unknown(), unknown())),
      list(pkg = "tdarec", fun = "xby", range = c(unknown(), unknown()))
    ),
    source = "recipe",
    component = "step_vpd_persistence_landscape",
    component_id = x$id
  )
}
