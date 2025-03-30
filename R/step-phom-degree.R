#' @title Separate persistent pairs by homological degree
#'
#' @description The function `step_phom_degree()` creates a _specification_ of a
#'   recipe step that will separate data sets of persistent pairs by homological
#'   degree. The input and output must be list-columns.
#'
#' @details Additional details...
#'
#'   The `hom_degrees` argument sets the homological degrees for which to return
#'   new list-columns. If not `NULL` (the default), it is intersected with the
#'   degrees found in any specified columns of the training data; otherwise all
#'   found degrees are used. This parameter cannot be tuned.
#'

#' @import recipes
#' @importFrom dials new_quant_param unknown
#' @inheritParams recipes::step_pca
#' @inherit recipes::step_pca return
#' @param hom_degrees Integer vector of homological degrees.
#' @family topological feature extraction via persistent homology
#' @example inst/examples/ex-step-phom-degree.R

#' @export
step_phom_degree <- function(
    recipe,
    ...,
    # standard inputs
    role = "persistence diagram",
    trained = FALSE,
    # custom parameters
    hom_degrees = NULL,
    # standard parameters
    columns = NULL,
    keep_original_cols = FALSE,
    skip = FALSE,
    id = rand_id("phom_degree")
) {
  recipes_pkg_check(required_pkgs.step_phom_degree())
  
  # output the step
  add_step(
    recipe,
    step_phom_degree_new(
      terms = rlang::enquos(...),
      trained = trained,
      role = role,
      hom_degrees = hom_degrees,
      columns = columns,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  )
}

step_phom_degree_new <- function(
    terms,
    role, trained,
    hom_degrees,
    columns, keep_original_cols,
    skip, id
) {
  step(
    subclass = "phom_degree",
    terms = terms,
    role = role,
    trained = trained,
    hom_degrees = hom_degrees,
    columns = columns,
    keep_original_cols = keep_original_cols,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_phom_degree <- function(x, training, info = NULL, ...) {
  # save(x, training, info, file = here::here("step-phom-degree-prep.rda"))
  # load(here::here("step-phom-degree-prep.rda"))
  
  col_names <- recipes_eval_select(x$terms, training, info)
  check_phom_list(training[, col_names, drop = FALSE])
  for (col_name in col_names) class(training[[col_name]]) <- "list"
  
  # intersection of `hom_degrees` (if passed) and dimensions in data
  x_hom_degrees <- get_hom_degrees(training[, col_names, drop = FALSE])
  if (is.null(x$hom_degrees))
    x$hom_degrees <- x_hom_degrees
  else
    x$hom_degrees <- sort(intersect(as.integer(x$hom_degrees), x_hom_degrees))
  
  # output prepped step
  step_phom_degree_new(
    terms = col_names,
    role = x$role,
    trained = TRUE,
    hom_degrees = x$hom_degrees,
    columns = col_names,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_phom_degree <- function(object, new_data, ...) {
  # save(object, new_data, file = here::here("step-phom-degree-bake.rda"))
  # load(here::here("step-phom-degree-bake.rda"))
  
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)
  for (col_name in col_names) class(new_data[[col_name]]) <- "list"
  
  # iterate all columns over the same degrees
  phom_data <- tibble::tibble(.rows = nrow(new_data))
  for (term in object$terms) for (deg in object$hom_degrees) {
    
    # NB: This works for the 'PHom' class but may not for other formats.
    term_deg_phom <- lapply(new_data[[term]], \(d) d[d[, 1L] == deg, ])
    
    phom_data[[paste(term, deg, sep = "_")]] <- term_deg_phom
  }
  
  check_name(phom_data, new_data, object)
  new_data <- vctrs::vec_cbind(new_data, phom_data)
  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

#' @export
print.step_phom_degree <- function(
    x, width = max(20, options()$width - 35), ...
) {
  # save(x, width, file = here::here("step-phom-degree-print.rda"))
  # load(here::here("step-phom-degree-print.rda"))
  
  title <- paste0(
    paste0(x$hom_degrees, collapse = ", "),
    "-degree features from "
  )
  
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
required_pkgs.step_phom_degree <- function(x, ...) {
  c("tdarec")
}

#' @rdname step_phom_degree
#' @usage NULL
#' @export
tidy.step_phom_degree <- function(x, ...) {
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
