#' @title Topological feature extraction via persistent homology
#'
#' @description The function `step_phom()` creates a _specification_ of a recipe
#'   step that will convert compatible data formats (distance matrices,
#'   coordinate matrices, or time series) to 3-column matrix representations of
#'   persistence diagram data.
#'
#' @details
#'
#' (PH)
#'
#' (Guidelines / good practice?)
#'
#' (Describe arguments in detail.)

#' @import recipes
#' @inheritParams recipes::step_pca
#' @inherit recipes::step_pca return
#' @param engine Character; package to use to calculate persistent homology.
#' @param method Character; type of filtration to construct (must be compatible
#'   with data).
#' @param dim_max,radius_max,diameter_max,field_order,cubical_method Parameters
#'   passed to persistence engines.
#' @example inst/examples/ex-step-PHom.R

#' @export
step_phom <- function(
    recipe,
    ...,
    # standard inputs
    role = "predictor",
    trained = FALSE,
    # custom parameters
    engine = "ripserr", method = "vietoris_rips",
    dim_max = 1L,
    radius_max = NULL, diameter_max = NULL,
    field_order = 2L,
    cubical_method = "lj",
    # standard parameters
    columns = NULL,
    keep_original_cols = TRUE,
    skip = FALSE,
    id = rand_id("phom")
) {
  recipes_pkg_check(required_pkgs.step_phom())
  
  # output the step
  add_step(
    recipe,
    step_phom_new(
      terms = rlang::enquos(...),
      trained = trained,
      role = role,
      engine = engine, method = method,
      dim_max = dim_max,
      radius_max = radius_max, diameter_max = diameter_max,
      field_order = field_order,
      cubical_method = cubical_method,
      columns = columns,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  )
}

step_phom_new <- function(
    terms,
    role, trained,
    engine, method,
    dim_max, radius_max, diameter_max, field_order, cubical_method,
    columns, keep_original_cols,
    skip, id
) {
  step(
    subclass = "phom",
    terms = terms,
    role = role,
    trained = trained,
    engine = engine, method = method,
    dim_max = dim_max,
    radius_max = radius_max, diameter_max = diameter_max,
    field_order = field_order,
    cubical_method = cubical_method,
    columns = columns,
    keep_original_cols = keep_original_cols,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_phom <- function(x, training, info = NULL, ...) {
  # save(x, training, info, file = here::here("step-phom-prep.rda"))
  # load(here::here("step-phom-prep.rda"))
  
  # extract columns and ensure they are lists of 3-column numeric tables
  col_names <- recipes_eval_select(x$terms, training, info)
  # check that all columns are list columns
  # TODO: Check other existing steps for handling of list columns.
  if (! all(vapply(training[, col_names, drop = FALSE], typeof, "") == "list"))
    rlang::abort("The `phom` step can only transform list columns.")
  # remove troublesome 'AsIs' class (and any other non-'list' classes)
  for (col_name in col_names) class(training[[col_name]]) <- "list"
  
  # check that engine is installed
  if (system.file(package = x$engine) == "") {
    rlang::abort(paste0("Package {", x$engine, "} is not installed."))
  }
  # match function
  engine_functions <- switch(
    x$engine,
    ripserr = c("vietoris_rips", "cubical")
  )
  x$method <- match.arg(x$method, engine_functions)
  # get function methods
  function_methods <- switch(
    x$engine,
    ripserr = switch(
      x$method,
      vietoris_rips = utils::methods(ripserr::vietoris_rips),
      cubical = utils::methods(ripserr::cubical)
    )
  )
  function_methods <-
    gsub("vietoris_rips\\.", "", as.character(function_methods))
  function_methods <- setdiff(function_methods, "default")
  # check that each list element is of a class with a 'method' method
  col_errs <- character(0L)
  for (col_name in col_names) {
    inheritance <-
      purrr::map_lgl(training[[col_name]], \(x) inherits(x, function_methods))
    if (! all(inheritance)) col_errs <- c(col_errs, col_name)
  }
  if (length(col_errs) > 0L)
    rlang::abort(paste0(
      "The `phom` step requires elements passable to `",
      x$method, "()` in package {", x$engine, "}."
    ))
  
  # if needed, select threshold
  if (is.null(x$diameter_max)) {
    if (is.null(x$radius_max)) {
      x$diameter_max <- -1L
    } else {
      x$diameter_max <- 2 * x$radius_max
      x$radius_max <- NULL
    }
  } else {
    if (x$diameter_max == Inf) {
      x$diameter_max <- -1L
    }
    if (! is.null(x$radius_max)) {
      warning(
        "Both `radius_max` and `diameter_max` were passed; ",
        "only `diameter_max` value will be used."
      )
      x$radius_max <- NULL
    }
  }
  
  # output prepped step
  step_phom_new(
    terms = col_names,
    role = x$role,
    trained = TRUE,
    engine = x$engine, method = x$method,
    dim_max = x$dim_max,
    radius_max = x$radius_max, diameter_max = x$diameter_max,
    field_order = x$field_order,
    cubical_method = x$cubical_method,
    columns = col_names,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_phom <- function(object, new_data, ...) {
  # save(object, new_data, file = here::here("step-phom-bake.rda"))
  # load(here::here("step-phom-bake.rda"))
  
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)
  # remove troublesome 'AsIs' class (and any other non-'list' classes)
  for (term in object$terms) class(new_data[[term]]) <- "list"
  
  # TODO: move {ggtda} procedure for passing data to engine to helper package
  # TODO: include `.ripserr_version` in helper package
  # tabulate persistent homology from each data column
  phom_data <- tibble::tibble(.rows = nrow(new_data))
  ripserr_version <- utils::packageVersion("ripserr")
  for (term in object$terms) {
    term_phom <- if (ripserr_version == "0.1.1") {
      purrr::map(
        new_data[[term]],
        \(d) ripserr::vietoris_rips(
          d,
          threshold = object$diameter_max,
          dim = object$dim_max,
          p = object$field_order,
          return_format = "df"
        )
      )
    } else if (ripserr_version >= "0.2.0") {
      purrr::map(
        new_data[[term]],
        \(d) ripserr::vietoris_rips(
          d,
          threshold = object$diameter_max,
          dim = object$dim_max,
          p = object$field_order
        )
      )
    }
    phom_data[[paste(term, "phom", sep = "_")]] <- term_phom
  }
  
  check_name(phom_data, new_data, object)
  new_data <- vctrs::vec_cbind(new_data, phom_data)
  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

#' @export
print.step_phom <- function(
    x, width = max(20, options()$width - 35), ...
) {
  # TODO: match method (`vietoris_rips`) to elegant name to use here
  cat("Persistent features from a Vietoris-Rips filtration of ", sep = "")
  printer(
    untr_obj = x$terms,
    tr_obj = NULL,
    trained = x$trained,
    width = width
  )
  invisible(x)
}

#' @rdname required_pkgs.tdarec
#' @export
required_pkgs.step_phom <- function(x, ...) {
  # REVIEW: {ggplot2} encourages this syntax, but {recipes} might not tolerate
  # it.
  c("ripserr|TDA", "tdarec")
}
