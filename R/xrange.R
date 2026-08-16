#' xrange: dial for the range coefficient t
#'
#' @description Instead of tuning xmin and xmax directly, we here choose to tune
#'   a new coefficient t, controlling a buffer applied to the original range.
#'   t = 0 keeps the original range unchanged, t = 1 doubles it. The benefit is 
#'   that it's easier to represent in code, since we can just define its limits 
#'   as 0 and 1, with no need for a finalize step
#'
#' @param range smallest and largest allowed t. Default c(0, 1).
#' @export
xrange <- function(range = c(0, 1)) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    label = c(xrangek = "Range coefficient"),
    finalize = NULL
  )
}

#' get_range: turns t into actual xmin/xmax
#'
#' @description Computes xmin/xmax for a persistence diagram with a given t by
#'   expanding the data discussed above. 
#'   step_vpd_persistence_landscape_t() uses in prep() to turn t into the grid limits.
#'
#' @param x a persistence diagram
#' @param hom_degree which homological degree
#' @param t the buffering coefficient, see xrange()
#' @param allow_negative if FALSE (default) shifts the lower bound to be 
#' nonnegative instead of cutting it off, so the width stays the same
#' @export
get_range <- function(x, hom_degree, t, allow_negative = FALSE) {
  r <- life_support(x, hom_degree)
  M <- (r[1] + r[2]) / 2
  width <- (r[2] - r[1]) / 2 * (1 + t)
  lower <- M - width
  upper <- M + width
  if (allow_negative) {
    return(c(lower, upper))
  }
  c(max(0, lower), max(upper, upper - lower))
}
#' Persistence landscape vectorization with a tunable range coefficient t
#'
#' @description This is a new step, separate from and work as the same function 
#'   as step_vpd_persistence_landscape() with its own prep/bake. 
#'   prep() uses t to compute xmin/xmax from the training data, 
#'   bake() uses those results to compute the vectorization
#'
#' @param recipe,role,trained,skip,id,keep_original_cols,columns 
#'   standard recipe step args, see step_vpd_persistence_landscape()
#' @param t the buffering coefficient for range, defaults to tune()
#' @param hom_degree,num_levels passed to TDAvec::computePersistenceLandscape()
#' @export
step_vpd_persistence_landscape_t <- function(
    recipe, ..., role = "predictor", trained = FALSE,
    t = tune(), hom_degree = 0L, num_levels = 6L,
    columns = NULL, keep_original_cols = TRUE, skip = FALSE,
    id = rand_id("vpd_pl_t")
) {
  add_step(
    recipe,
    step_vpd_persistence_landscape_t_new(
      terms = rlang::enquos(...),
      role = role, trained = trained,
      t = t, hom_degree = hom_degree, num_levels = num_levels,
      columns = columns, keep_original_cols = keep_original_cols,
      skip = skip, id = id
    )
  )
}

step_vpd_persistence_landscape_t_new <- function(
    terms, role, trained, t, hom_degree, num_levels,
    columns, keep_original_cols, skip, id
) {
  step(
    subclass = "vpd_persistence_landscape_t",
    terms = terms, role = role, trained = trained,
    t = t, hom_degree = hom_degree, num_levels = num_levels,
    columns = columns, keep_original_cols = keep_original_cols,
    skip = skip, id = id
  )
}

#' @rdname step_vpd_persistence_landscape_t
#' @param x a step_vpd_persistence_landscape_t object
#' @export
tunable.step_vpd_persistence_landscape_t <- function(x, ...) {
  tibble::tibble(
    name = "t",
    call_info = list(list(pkg = "tdarec", fun = "xrange", range = c(0, 1))),
    source = "recipe",
    component = "step_vpd_persistence_landscape_t",
    component_id = x$id
  )
}

#' @rdname step_vpd_persistence_landscape_t
#' @param training training data
#' @param info step info, default NULL
#' @export
prep.step_vpd_persistence_landscape_t <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_pd_list(training[, col_names, drop = FALSE])
  for (col_name in col_names) class(training[[col_name]]) <- "list"

  # this is where t actually turns into xmin/xmax
  pd_list <- training[[col_names[1]]]
  ranges <- purrr::map(pd_list, ~ get_range(.x, x$hom_degree, x$t, allow_negative = TRUE))
  xmin <- min(purrr::map_dbl(ranges, 1))
  xmax <- min(purrr::map_dbl(ranges, 2))
  xseq <- seq(xmin, xmax, length.out = 100)

  step_vpd_persistence_landscape_t_new(
    terms = col_names, role = x$role, trained = TRUE,
    t = x$t, hom_degree = x$hom_degree, num_levels = x$num_levels,
    columns = list(names = col_names, xseq = xseq),
    keep_original_cols = x$keep_original_cols, skip = x$skip, id = x$id
  )
}

#' @rdname step_vpd_persistence_landscape_t
#' @param object a trained step_vpd_persistence_landscape_t object
#' @param new_data data to bake
#' @export
bake.step_vpd_persistence_landscape_t <- function(object, new_data, ...) {
  col_names <- object$columns$names
  check_new_data(col_names, object, new_data)
  for (col_name in col_names) class(new_data[[col_name]]) <- "list"

  if (nrow(new_data) == 0L || length(col_names) == 0L) return(new_data)

  vph_data <- tibble::tibble(.rows = nrow(new_data))
  for (col_name in col_names) {
    col_vpd <- purrr::map(new_data[[col_name]], function(d) {
      v <- TDAvec::computePersistenceLandscape(
        as.matrix(d),
        homDim = object$hom_degree,
        scaleSeq = object$columns$xseq,
        k = object$num_levels
      )
      vn <- vpd_suffix(v)
      v <- as.vector(v)
      names(v) <- vn
      v
    })
    col_vpd <- purrr::map(
      col_vpd,
      function(v) as.data.frame(matrix(v, nrow = 1L, dimnames = list(NULL, names(v))))
    )
    vph_data[[paste(col_name, "pl", sep = "_")]] <- col_vpd
  }
  vph_col_names <- paste(col_names, "pl", sep = "_")
  vph_data <- tidyr::unnest(vph_data, cols = tidyr::all_of(vph_col_names), names_sep = "_")

  check_name(vph_data, new_data, object)
  new_data <- vctrs::vec_cbind(new_data, vph_data)
  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

#' @rdname step_vpd_persistence_landscape_t
#' @export
required_pkgs.step_vpd_persistence_landscape_t <- function(x, ...) {
  c("TDAvec", "tdarec")
}
