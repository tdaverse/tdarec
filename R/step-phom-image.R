#' @title Persistent homology of images
#'
#' @description The function `step_phom_image()` creates a _specification_ of a
#'   recipe step that will convert compatible data formats (numerical arrays,
#'   including matrices, of 2, 3, or 4 dimensions) to 3-column matrix
#'   representations of persistence diagram data. The input and output must be
#'   list-columns.
#'
#' @template step-phom-details
#'
#' @section PH of Images:
#'
#'   The PH of (greyscale) digital images is computed from the cubical
#'   filtration of the pixel or voxel array, treated as a function from a
#'   cubical mesh to a finite value range.
#'
#'   Cubical Ripser is an efficient implementation of cubical PH and is ported
#'   to R through the {ripserr} package. It accepts numerical arrays.
#'
#'   The `value_max` argument bounds the value range along which PH is computed.
#'   Cubical Ripser is implemented using both of two methods, link-join and
#'   compute-pairs, controlled by the `method` parameter.
#'
#' @section Tuning Parameters:
#'
#' ```{r, echo=FALSE, results="asis"}
#' step <- "step_phom_image"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#' 

#' @import recipes
#' @importFrom dials new_quant_param unknown
#' @inheritParams recipes::step_pca
#' @inherit recipes::step_pca return
#' @param filtration The type of filtration from which to compute persistent
#'   homology; currently only `"cubical"`.
#' @param value_max,method Parameters passed to persistence engines.
#' @param engine The computational engine to use (see 'Details'). Reasonable
#'   defaults are chosen based on `filtration`.
#' @family topological feature extraction via persistent homology
#' @example inst/examples/ex-step-phom-image.R

#' @export
step_phom_image <- function(
    recipe,
    ...,
    # standard inputs
    role = "predictor",
    trained = FALSE,
    # custom parameters
    filtration = "cubical",
    value_max = 9999L,
    method = c("link_join", "compute_pairs"),
    engine = NULL,
    # standard parameters
    columns = NULL,
    keep_original_cols = TRUE,
    skip = FALSE,
    id = rand_id("phom_image")
) {
  recipes_pkg_check(required_pkgs.step_phom_image())
  
  # output the step
  add_step(
    recipe,
    step_phom_image_new(
      terms = rlang::enquos(...),
      trained = trained,
      role = role,
      filtration = filtration,
      value_max = value_max,
      method = method,
      engine = engine,
      columns = columns,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  )
}

step_phom_image_new <- function(
    terms,
    role, trained,
    filtration,
    value_max, method,
    engine,
    columns, keep_original_cols,
    skip, id
) {
  step(
    subclass = "phom_image",
    terms = terms,
    role = role,
    trained = trained,
    filtration = filtration,
    value_max = value_max,
    method = method,
    engine = engine,
    columns = columns,
    keep_original_cols = keep_original_cols,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_phom_image <- function(x, training, info = NULL, ...) {
  # save(x, training, info, file = here::here("step-phom-image-prep.rda"))
  # load(here::here("step-phom-image-prep.rda"))
  
  # extract columns and ensure they are lists of 3-column numeric tables
  col_names <- recipes_eval_select(x$terms, training, info)
  # check that all columns are list-columns
  # TODO: Check other existing steps for handling of list-columns.
  if (! all(vapply(training[, col_names, drop = FALSE], typeof, "") == "list"))
    rlang::abort("The `phom_image` step can only transform list-columns.")
  # remove troublesome 'AsIs' class (and any other non-'list' classes)
  for (col_name in col_names) class(training[[col_name]]) <- "list"
  
  # TODO: Make these into a tools for a {ggtda}/{tdarec} helper package.

  # pre-process filtration parameters

  # logic to deduce reasonable values of engine
  # + issue warnings when choices are incompatible
  x$filtration <-
    match.arg(x$filtration, c("cubical"))
  if (is.null(x$engine)) {
    x$engine <- "ripserr"
  } else {
    x$engine <-
      match.arg(x$engine, c("ripserr"))
  }
  # TODO: Incorporate assignment helper functions into helper package.
  # x$engine <-
  #   assign_filtration_engine(x$filtration, x$engine)
  if (x$engine != "ripserr")
    rlang::abort("Only the {ripserr} engine can be deployed as yet.")

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
      \(x) inherits(x, .ripserr_cubical_classes)
    )
    if (! all(inheritance)) col_errs <- c(col_errs, col_name)
  }
  if (length(col_errs) > 0L)
    rlang::abort(paste0(
      "Some list-column elements are not passable to ",
      "`ripserr::cubical()`."
    ))
  
  # if needed, select threshold
  # TODO: Make this threshold at least the largest finite value in the data.
  if (is.null(x$value_max)) {
    x$value_max <- 9999L
  }
  # match method
  x$method <- match.arg(x$method, c("link_join", "compute_pairs"))
  
  # output prepped step
  step_phom_image_new(
    terms = col_names,
    role = x$role,
    trained = TRUE,
    filtration = x$filtration,
    value_max = x$value_max,
    method = x$method,
    engine = x$engine,
    columns = col_names,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_phom_image <- function(object, new_data, ...) {
  # save(object, new_data, file = here::here("step-phom-image-bake.rda"))
  # load(here::here("step-phom-image-bake.rda"))
  
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)
  # remove troublesome 'AsIs' class (and any other non-'list' classes)
  for (term in object$terms) class(new_data[[term]]) <- "list"
  
  # TODO: Move {ggtda} procedure for passing data to engine to helper package.
  # TODO: include `.ripserr_version` in helper package
  # tabulate persistent homology from each data column
  phom_data <- tibble::tibble(.rows = nrow(new_data))
  for (term in object$terms) {
    term_phom <- if (.ripserr_version == "0.1.1") {
      purrr::map(
        new_data[[term]],
        \(d) ripserr::cubical(
          d,
          threshold = object$value_max,
          method = switch(object$method, link_join = 0, compute_pairs = 1),
          return_format = "df"
        )
      )
    } else if (.ripserr_version >= "0.2.0") {
      purrr::map(
        new_data[[term]],
        \(d) ripserr::cubical(
          d,
          threshold = object$value_max,
          method = switch(object$method, link_join = "lj", compute_pairs = "cp")
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
print.step_phom_image <- function(
    x, width = max(20, options()$width - 35), ...
) {
  # save(x, width, file = here::here("step-phom-image-print.rda"))
  # load(here::here("step-phom-image-print.rda"))
  
  cat(
    "Persistent features from a ",
    x$filtration,
    " filtration of ",
    sep = ""
  )
  printer(
    tr_obj = NULL,
    untr_obj = x$terms,
    trained = x$trained,
    width = width
  )
  invisible(x)
}

#' @rdname required_pkgs.tdarec
#' @export
required_pkgs.step_phom_image <- function(x, ...) {
  c("ripserr", "tdarec")
}

#' @rdname step_phom_image
#' @usage NULL
#' @export
tidy.step_phom_image <- function(x, ...) {
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
tunable.step_phom_image <- function(x, ...) {
  tibble::tibble(
    name = c("max_hom_degree"),
    call_info = list(
      list(pkg = "tdarec", fun = "max_hom_degree", range = c(0L, 3L))
    ),
    source = "recipe",
    component = "step_phom_image",
    component_id = x$id
  )
}
