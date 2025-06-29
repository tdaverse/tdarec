# ------------------------------------------------------------------------------
# Generated by 'pre-generate/generate-steps.R': do not edit by hand.
# ------------------------------------------------------------------------------

#' @title Persistence Image Vectorization of Persistent Homology
#' 
#' @description The function `step_vpd_persistence_image()` creates
#'   a _specification_ of a recipe step that will convert
#'   a list-column of 3-column matrices of persistence data
#'   to a list-column of 1-row matrices of vectorizations.
#' 

#' 
#' @template step-vpd-details
#' 
#' @section Engine:
#' 
#' The persistence image vectorization deploys
#' [TDAvec::computePersistenceImage()].
#' See there for definitions and references.
#' 
#' @section Tuning Parameters:
#' 
#' This step has 2 tuning parameters:
#' \itemize{
#'   \item `hom_degree`: Homological degree (type: integer, default: `0L`)
#'   \item `img_sigma`: Convolved Gaussian standard deviation (type: double, default: `1`)
#' }
#' 
#' @param hom_degree
#'   The homological degree of the features to be transformed.
#' @param xseq
#'   A discretization grid, as an increasing numeric vector.
#'   `xseq` overrides the other `x*` parameters with a warning.
#' @param xmin,xmax,xlen,xby
#'   Limits and resolution of a discretization grid;
#'   specify only one of `xlen` and `xby`.
#' @param yseq
#'   Combined with `xseq` to form a 2-dimensional discretization grid.
#' @param ymin,ymax,ylen,yby
#'   Limits and resolution of a discretization grid;
#'   specify only one of `ylen` and `yby`.
#' @param img_sigma
#'   The standard deviation of the gaussian distribution
#'   convolved with persistence diagrams to obtain persistence images.

#' @import recipes
#' @inheritParams recipes::step_pca
#' @inherit recipes::step_pca return
#' @example inst/examples/zzz-ex-step-vpd-persistence-image.R

#' @export
step_vpd_persistence_image <- function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    hom_degree = 0L,
    xseq = NULL, xmin = NULL, xmax = NULL, xlen = NULL, xby = NULL,
    yseq = NULL, ymin = NULL, ymax = NULL, ylen = NULL, yby = NULL,
    img_sigma = 1,
    columns = NULL,
    keep_original_cols = TRUE,
    skip = FALSE,
    id = rand_id("vpd_persistence_image")
) {
  recipes_pkg_check(required_pkgs.step_vpd_persistence_image())
  
  add_step(
    recipe,
    step_vpd_persistence_image_new(
      terms = rlang::enquos(...),
      trained = trained,
      role = role,
      hom_degree = hom_degree,
      xseq = xseq, xmin = xmin, xmax = xmax, xlen = xlen, xby = xby,
      yseq = yseq, ymin = ymin, ymax = ymax, ylen = ylen, yby = yby,
      img_sigma = img_sigma,
      columns = columns,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  )
}

step_vpd_persistence_image_new <- function(
    terms,
    role, trained,
    hom_degree,
    xseq, xmin, xmax, xlen, xby,
    yseq, ymin, ymax, ylen, yby,
    img_sigma,
    columns, keep_original_cols,
    skip, id
) {
  step(
    subclass = "vpd_persistence_image",
    terms = terms,
    role = role,
    trained = trained,
    hom_degree = hom_degree,
    xseq = xseq, xmin = xmin, xmax = xmax, xlen = xlen, xby = xby,
    yseq = yseq, ymin = ymin, ymax = ymax, ylen = ylen, yby = yby,
    img_sigma = img_sigma,
    columns = columns,
    keep_original_cols = keep_original_cols,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_vpd_persistence_image <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_pd_list(training[, col_names, drop = FALSE])
  for (col_name in col_names) class(training[[col_name]]) <- "list"
  
  x[paste0("x", c("seq", "min", "max", "len", "by"))] <- 
    reconcile_scale_seq(x, training[, col_names, drop = FALSE], "x")
  
  x[paste0("y", c("seq", "min", "max", "len", "by"))] <- 
    reconcile_scale_seq(x, training[, col_names, drop = FALSE], "y")
  
  step_vpd_persistence_image_new(
    terms = col_names,
    role = x$role,
    trained = TRUE,
    hom_degree = x$hom_degree,
    xseq = x$xseq, xmin = x$xmin, xmax = x$xmax, xlen = x$xlen, xby = x$xby,
    yseq = x$yseq, ymin = x$ymin, ymax = x$ymax, ylen = x$ylen, yby = x$yby,
    img_sigma = x$img_sigma,
    columns = col_names,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_vpd_persistence_image <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)
  for (col_name in col_names) class(new_data[[col_name]]) <- "list"
  
  if (nrow(new_data) == 0L || length(col_names) == 0L) return(new_data)
  
  vph_data <- tibble::tibble(.rows = nrow(new_data))
  for (col_name in col_names) {
    col_vpd <- purrr::map(
      new_data[[col_name]],
      function(d) {
        v <- TDAvec::computePersistenceImage(
          as.matrix(d),
          homDim = object$hom_degree,
          xSeq = object$xseq,
          ySeq = object$yseq,
          sigma = object$img_sigma
        )
        vn <- vpd_suffix(v)
        v <- as.vector(v)
        names(v) <- vn
        v
      }
    )
    col_vpd <- purrr::map(
      col_vpd,
      function(v) as.data.frame(matrix(
        v, nrow = 1L, dimnames = list(NULL, names(v))
      ))
    )
    vph_data[[paste(col_name, "pi", sep = "_")]] <- col_vpd
  }
  vph_col_names <- if (length(col_names) == 0L) col_names else
    paste(col_names, "pi", sep = "_")
  vph_data <- tidyr::unnest(
    vph_data,
    cols = tidyr::all_of(vph_col_names),
    names_sep = "_"
  )
  
  check_name(vph_data, new_data, object)
  new_data <- vctrs::vec_cbind(new_data, vph_data)
  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

#' @export
print.step_vpd_persistence_image <- function(
    x, width = max(20, options()$width - 35), ...
) {
  title <- "persistence image of "
  
  print_step(
    untr_obj = x$terms,
    tr_obj = NULL,
    trained = x$trained,
    title = title,
    width = width
  )
  invisible(x)
}

#' @rdname required_pkgs.tdarec
#' @export
required_pkgs.step_vpd_persistence_image <- function(x, ...) {
  c("TDAvec", "tdarec")
}

#' @rdname step_vpd_persistence_image
#' @usage NULL
#' @export
tidy.step_vpd_persistence_image <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble::tibble(
      terms = unname(x$columns),
      value = rep(NA_real_, length(x$columns))
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble::tibble(
      terms = term_names,
      value = rep(NA_real_, length(term_names))
    )
  }
  res$id <- x$id
  res
}

#' @rdname tunable_tdavec
#' @export
tunable.step_vpd_persistence_image <- function(x, ...) {
  tibble::tibble(
    name = c("hom_degree", "img_sigma"),
    call_info = list(
      list(pkg = "tdarec", fun = "hom_degree", range = c(0L, unknown())),
      list(pkg = "tdarec", fun = "img_sigma", range = c(unknown(), unknown()))
    ),
    source = "recipe",
    component = "step_vpd_persistence_image",
    component_id = x$id
  )
}

