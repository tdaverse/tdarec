#' @title Vectorize persistence data as Euler characteristic curves
#'
#' @description The functions `step_vpd_*()` create _specifications_ of recipe
#'   steps that will convert 3-column matrix representations of persistence
#'   diagrams to 1-row matrices of vectorizations.
#'
#' @details
#'
#' (What are ECCs?)
#'
#' (Guidelines / good practice?)
#'
#' (Describe arguments in detail.)

#' @import recipes
#' @inheritParams recipes::step_pca
#' @importFrom TDAvec computeECC

#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step.
#' @param role For model terms created by this step, what analysis role should
#'   they be assigned? By default, the function assumes that the new columns
#'   created by the original variables will be used as predictors in a model.
#' @param trained A logical value indicating whether the values used for
#'   binarization have been checked.
#' @param dim_max,scale_seq Parameters used by {TDAvec}.
#' @param skip A logical value indicating whether the step should be skipped
#'   when the recipe is baked by `bake.recipe()`.
#' @param id A character string that is unique to this step, used to identify
#'   it.
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of existing steps (if any).
#' @example inst/examples/ex-step-VPD-ECC.R

#' @export
step_vpd_ecc <- function(
    recipe,
    # custom inputs
    ...,
    # standard inputs
    role = "predictor",
    trained = FALSE,
    # custom parameters
    dim_max = Inf,
    scale_seq = NULL,
    # standard parameters
    columns = NULL,
    keep_original_cols = TRUE,
    skip = FALSE,
    id = rand_id("vpd_ecc")
) {
  
  # check any criteria here
  
  # output the step
  add_step(
    recipe,
    step_vpd_ecc_new(
      terms = rlang::enquos(...),
      trained = trained,
      role = role,
      dim_max = dim_max,
      scale_seq = scale_seq,
      columns = columns,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  )
}

step_vpd_ecc_new <- function(
    terms,
    role, trained,
    dim_max, scale_seq,
    columns, keep_original_cols,
    skip, id
) {
  step(
    subclass = "vpd_ecc",
    terms = terms,
    role = role,
    trained = trained,
    dim_max = dim_max,
    scale_seq = scale_seq,
    columns = columns,
    keep_original_cols = keep_original_cols,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_vpd_ecc <- function(x, training, info = NULL, ...) {
  # save(x, training, info, file = here::here("step-ecc-prep.rda"))
  # load(here::here("step-ecc-prep.rda"))
  
  # extract columns and ensure they are lists of 3-column numeric tables
  col_names <- recipes_eval_select(x$terms, training, info)
  # check_type(training[, col_names, drop = FALSE], types = c("list"))
  # check that all columns are list columns
  if (! all(vapply(training[, col_names, drop = FALSE], typeof, "") == "list"))
    rlang::abort("The `vpd_ecc` step can only transform list columns.")
  # remove troublesome 'AsIs' class (and any other non-'list' classes)
  for (col_name in col_names) class(training[[col_name]]) <- "list"
  # check that each list element is a 3-column matrix or data frame
  # TODO: encode using persistence data class
  are_phom <- function(l) all(vapply(
    l,
    \(x) ncol(x) == 3L && (
      (inherits(x, "data.frame") && 
        all(vapply(x, typeof, "") %in% c("double", "integer"))) ||
        (inherits(x, "matrix") && typeof(x) %in% c("double", "integer"))
    ),
    FALSE
  ))
  if (! all(vapply(training[, col_names, drop = FALSE], are_phom, FALSE))) {
    rlang::abort("The `vpd_ecc` step requires 3-column persistence data.")
  }
  
  # if needed, select maximum dimension
  if (x$dim_max == Inf) {
    train_dim_maxs <- purrr::map(
      training[, col_names, drop = FALSE],
      \(x) purrr::map_dbl(x, \(m) max(m[, 1L]))
    )
    x$dim_max <- max(unlist(train_dim_maxs))
  }
  # if needed, select scale sequence
  if (is.null(x$scale_seq)) {
    train_birth_min <- purrr::map(
      training[, col_names, drop = FALSE],
      \(x) purrr::map_dbl(x, \(m) min(m[, 2L]))
    )
    train_death_max <- purrr::map(
      training[, col_names, drop = FALSE],
      \(x) purrr::map_dbl(x, \(m) max(m[, 3L]))
    )
    # TODO: user should control the following:
    # * whether to start at zero
    # * how much buffer to expand from the maximum death (here 2)
    # * which of `by` or `length.out` to use and what value to pass
    x$scale_seq <- seq(
      min(unlist(train_birth_min)),
      max(unlist(train_death_max)) * 2,
      length.out = 101L# user should control `seq()` params
    )
  }
  
  # output prepped step
  step_vpd_ecc_new(
    terms = col_names,
    role = x$role,
    trained = TRUE,
    dim_max = x$dim_max,
    scale_seq = x$scale_seq,
    columns = col_names,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_vpd_ecc <- function(object, new_data, ...) {
  # save(object, new_data, file = here::here("step-ecc-bake.rda"))
  # load(here::here("step-ecc-bake.rda"))
  
  # remove troublesome 'AsIs' class (and any other non-'list' classes)
  for (term in object$terms) class(new_data[[term]]) <- "list"
  
  # apply vectorization to each persistence data column
  # TODO: compare with existing recipes and decide to store vectorizations as
  # * matrices in list columns (with or without names)
  # * data frames in list columns (require names)
  # * numerous additional columns (require names)
  for (term in object$terms) {
    term_ecc <- purrr::map(
      new_data[[term]],
      \(d) TDAvec::computeECC(
        as.matrix(d),
        maxhomDim = object$dim_max, scaleSeq = object$scale_seq
      )
    )
    term_ecc <- lapply(term_ecc, matrix, nrow = 1L)
    new_data[[term]] <- term_ecc
  }
  # rename transformed columns
  names(new_data) <- ifelse(
    names(new_data) %in% object$terms,
    paste(names(new_data), "ecc", sep = "_"),
    names(new_data)
  )
  
  new_data
}

#' @export
print.step_vpd_ecc <- function(
    x, width = max(20, options()$width - 35), ...
) {
  cat("Euler characteristic curves of ", sep = "")
  printer(
    untr_obj = x$terms,
    tr_obj = NULL,
    trained = x$trained,
    width = width
  )
  invisible(x)
}
