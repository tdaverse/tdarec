#' @title Persistent homology of point clouds
#'
#' @description The function `step_pd_point_cloud()` creates a _specification_
#'   of a recipe step that will convert compatible data formats (distance
#'   matrices, coordinate matrices, or time series) to 3-column matrix
#'   representations of persistence diagram data. The input and output must be
#'   list-columns.
#'
#' @template step-pd-details
#'
#' @section PH of Point Clouds:
#'
#'   The PH of a point cloud arises from a simplicial filtration (usually
#'   Vietoris--Rips, Čech, or alpha) along an increasing distance threshold.
#'
#'   Ripser is a highly efficient implementation of PH on a point cloud (a
#'   finite metric space) via the Vietoris--Rips filtration and is ported to R
#'   through [`ripserr`][ripserr::ripserr-package]. [`TDA`][TDA::TDA-package]
#'   calls the Dionysus, PHAT, and GUDHI libraries to compute PH via
#'   Vietoris--Rips and alpha filtrations. The `filtration` parameter controls
#'   the choice of filtration while the `engine` parameter allows the user to
#'   manually select an implementation.
#'
#'   Both engines accept data sets in distance matrix, coordinate matrix, and
#'   data frame formats. While **ripserr** computes PH for time series data,
#'   this is not currently supported in **tdarec**.
#'
#'   The `max_hom_degree` argument determines the highest-dimensional features
#'   to be calculated. Either `diameter_max` (preferred) or `radius_max` can be
#'   used to bound the distance threshold along which PH is computed. The
#'   `field_order` argument should be prime and will be the order of the field
#'   of coefficients used in the computation. In most applications, only
#'   `max_hom_degree` will be tuned, and to at most `3L`.
#'
#' @section Tuning Parameters:
#'
#' ```{r, echo=FALSE, results="asis"}
#' step <- "step_pd_point_cloud"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#' 

#' @import recipes
#' @importFrom dials new_quant_param unknown
#' @inheritParams recipes::step_pca
#' @inherit recipes::step_pca return
#' @param filtration The type of filtration from which to compute persistent
#'   homology; one of `"Rips"`, `"Vietoris"` (equivalent), or `"alpha"`.
#' @param max_hom_degree,radius_max,diameter_max,field_order
#'   Parameters passed to persistence engines.
#' @param engine The computational engine to use (see 'Details'). Reasonable
#'   defaults are chosen based on `filtration`.
#' @family topological feature extraction via persistent homology
#' @example inst/examples/ex-step-pd-point-cloud.R

#' @export
step_pd_point_cloud <- function(
    recipe,
    ...,
    # standard inputs
    role = "persistence diagram",
    trained = FALSE,
    # custom parameters
    filtration = "Rips",
    max_hom_degree = 1L,
    radius_max = NULL, diameter_max = NULL,
    field_order = 2L,
    engine = NULL,
    # standard parameters
    columns = NULL,
    keep_original_cols = TRUE,
    skip = FALSE,
    id = rand_id("pd_point_cloud")
) {
  recipes_pkg_check(required_pkgs.step_pd_point_cloud())
  
  # output the step
  add_step(
    recipe,
    step_pd_point_cloud_new(
      terms = rlang::enquos(...),
      trained = trained,
      role = role,
      filtration = filtration,
      max_hom_degree = max_hom_degree,
      radius_max = radius_max, diameter_max = diameter_max,
      field_order = field_order,
      engine = engine,
      columns = columns,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  )
}

step_pd_point_cloud_new <- function(
    terms,
    role, trained,
    filtration,
    max_hom_degree, radius_max, diameter_max, field_order,
    engine,
    columns, keep_original_cols,
    skip, id
) {
  step(
    subclass = "pd_point_cloud",
    terms = terms,
    role = role,
    trained = trained,
    filtration = filtration,
    max_hom_degree = max_hom_degree,
    radius_max = radius_max, diameter_max = diameter_max,
    field_order = field_order,
    engine = engine,
    columns = columns,
    keep_original_cols = keep_original_cols,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_pd_point_cloud <- function(x, training, info = NULL, ...) {
  # save(x, training, info, file = here::here("step-pd-point-cloud-prep.rda"))
  # load(here::here("step-pd-point-cloud-prep.rda"))
  
  # extract columns and ensure they are lists of 3-column numeric tables
  col_names <- recipes_eval_select(x$terms, training, info)
  # check that all columns are list-columns
  # TODO: Check other existing steps for handling of list-columns.
  if (! all(vapply(training[, col_names, drop = FALSE], typeof, "") == "list"))
    rlang::abort("The `pd_point_cloud` step can only transform list-columns.")
  # remove troublesome 'AsIs' class (and any other non-'list' classes)
  for (col_name in col_names) class(training[[col_name]]) <- "list"
  
  # TODO: Make these into a tools for a {ggtda}/{tdarec} helper package.

  # pre-process filtration parameters

  # logic to deduce reasonable values of engine
  # + issue warnings when choices are incompatible
  x$filtration <-
    match.arg(x$filtration, c("Vietoris", "Rips", "alpha"))
  if (x$filtration %in% c("Vietoris", "Rips")) x$filtration <- "Vietoris-Rips"
  if (is.null(x$engine)) {
    x$engine <- "ripserr"
  } else {
    x$engine <-
      match.arg(x$engine, c("TDA", "GUDHI", "Dionysus", "ripserr"))
  }
  # TODO: Incorporate assignment helper functions into helper package.
  # x$engine <-
  #   assign_filtration_engine(x$filtration, x$engine)
  if (x$filtration == "alpha")
    rlang::abort("Alpha filtrations are not deployable yet.")
  if (x$engine != "ripserr")
    rlang::abort("Only the {ripserr} engine can be deployed as yet.")

  # # reconcile thresholds
  # if (is.null(x$radius_max) && is.null(x$diameter_max)) {
  #   x$diameter_max <- Inf
  # }
  # if (! is.null(x$radius_max)) {
  #   if (! is.null(x$diameter_max)) {
  #     warning("Both `radius_max` and `diameter_max` were passed; ",
  #             "only `diameter_max` value will be used.")
  #   } else {
  #     x$diameter_max <- x$radius_max * 2
  #   }
  # }

  # TODO: End helper package section. Begin temporary section.
  
  # check that engine is installed
  if (system.file(package = x$engine) == "") {
    rlang::abort(paste0("Package {", x$engine, "} is not installed."))
  }
  # check that each list element can be passed to the function
  col_errs <- character(0L)
  for (col_name in col_names) {
    inheritance <- purrr::map_lgl(
      training[[col_name]],
      function(x) inherits(x, .ripserr_vietoris_rips_classes)
    )
    if (! all(inheritance)) col_errs <- c(col_errs, col_name)
  }
  if (length(col_errs) > 0L)
    rlang::abort(paste0(
      "Some columns contain elements impassable to ",
      "`ripserr::vietoris_rips()`: '",
      paste0(col_errs, collapse = "', '"), "'."
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
  step_pd_point_cloud_new(
    terms = col_names,
    role = x$role,
    trained = TRUE,
    filtration = x$filtration,
    max_hom_degree = x$max_hom_degree,
    radius_max = x$radius_max, diameter_max = x$diameter_max,
    field_order = x$field_order,
    engine = x$engine,
    columns = col_names,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_pd_point_cloud <- function(object, new_data, ...) {
  # save(object, new_data, file = here::here("step-pd-point-cloud-bake.rda"))
  # load(here::here("step-pd-point-cloud-bake.rda"))
  
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)
  # remove troublesome 'AsIs' class (and any other non-'list' classes)
  for (term in object$terms) class(new_data[[term]]) <- "list"
  
  # TODO: Move {ggtda} procedure for passing data to engine to helper package.
  # TODO: include `.ripserr_version` in helper package
  # tabulate persistent homology from each data column
  pd_data <- tibble::tibble(.rows = nrow(new_data))
  for (term in object$terms) {
    term_pd <- if (.ripserr_version == "0.1.1") {
      purrr::map(
        new_data[[term]],
        function(d) ripserr::vietoris_rips(
          d,
          threshold = object$diameter_max,
          dim = object$max_hom_degree,
          p = object$field_order,
          return_format = "df"
        )
      )
    } else if (.ripserr_version >= "0.2.0") {
      purrr::map(
        new_data[[term]],
        function(d) ripserr::vietoris_rips(
          d,
          threshold = object$diameter_max,
          max_dim = object$max_hom_degree,
          p = object$field_order
        )
      )
    }
    pd_data[[paste(term, "pd", sep = "_")]] <- term_pd
  }
  
  check_name(pd_data, new_data, object)
  new_data <- vctrs::vec_cbind(new_data, pd_data)
  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

#' @export
print.step_pd_point_cloud <- function(
    x, width = max(20, options()$width - 35), ...
) {
  # save(x, width, file = here::here("step-pd-point-cloud-print.rda"))
  # load(here::here("step-pd-point-cloud-print.rda"))
  
  title <- paste0(
    "persistent features from a ",
    x$filtration,
    " filtration of "
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
required_pkgs.step_pd_point_cloud <- function(x, ...) {
  c("ripserr", "tdarec")
}

#' @rdname step_pd_point_cloud
#' @usage NULL
#' @export
tidy.step_pd_point_cloud <- function(x, ...) {
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

#' @export
tunable.step_pd_point_cloud <- function(x, ...) {
  tibble::tibble(
    name = c("max_hom_degree"),
    call_info = list(
      list(pkg = "tdarec", fun = "max_hom_degree", range = c(0L, 3L))
    ),
    source = "recipe",
    component = "step_pd_point_cloud",
    component_id = x$id
  )
}
