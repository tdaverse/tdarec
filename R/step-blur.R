#' @title Blur lattice (image) data
#'
#' @description The function `step_blur()` creates a _specification_ of a recipe
#'   step that will induce Gaussian blur in numerical arrays. The input and
#'   output must be list-columns.
#'
#' @details
#' Additional details...
#' 
#' **TODO:** Explain the importance of blur for PH of image data.
#' 
#' @section Tuning Parameters:
#'
#' ```{r, echo=FALSE, results="asis"}
#' step <- "step_blur"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#' 

#' @import recipes
#' @importFrom dials new_quant_param unknown
#' @param xmin,xmax,sigma Parameters passed to [blur()].
#' @inheritParams recipes::step_pca
#' @inherit recipes::step_pca return
#' @example inst/examples/ex-step-blur.R

#' @export
step_blur <- function(
    recipe,
    ...,
    # standard inputs
    role = "predictor",
    trained = FALSE,
    # custom parameters
    xmin = 0, xmax = 1,
    sigma = 1,
    # standard parameters
    columns = NULL,
    keep_original_cols = FALSE,
    skip = FALSE,
    id = rand_id("blur")
) {
  recipes_pkg_check(required_pkgs.step_blur())
  
  # output the step
  add_step(
    recipe,
    step_blur_new(
      terms = rlang::enquos(...),
      trained = trained,
      role = role,
      xmin = xmin, xmax = xmax,
      sigma = sigma,
      columns = columns,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  )
}

step_blur_new <- function(
    terms,
    role, trained,
    xmin, xmax,
    sigma,
    columns, keep_original_cols,
    skip, id
) {
  step(
    subclass = "blur",
    terms = terms,
    role = role,
    trained = trained,
    xmin = xmin, xmax = xmax,
    sigma = sigma,
    columns = columns,
    keep_original_cols = keep_original_cols,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_blur <- function(x, training, info = NULL, ...) {
  # save(x, training, info, file = here::here("step-blur-prep.rda"))
  # load(here::here("step-blur-prep.rda"))
  
  # extract columns and ensure they are lists of 3-column numeric tables
  col_names <- recipes_eval_select(x$terms, training, info)
  # check that all columns are list-columns
  # TODO: Check other existing steps for handling of list-columns.
  if (! all(vapply(training[, col_names, drop = FALSE], typeof, "") == "list"))
    rlang::abort("The `blur` step can only transform list-columns.")
  if (! all(vapply(
    training[, col_names, drop = FALSE],
    \(l) all(vapply(l, \(m) is.array(m) && is.numeric(m), FALSE)),
    FALSE
  )))
    rlang::abort("The `blur` step can only transform lists of numeric arrays.")
  # remove troublesome 'AsIs' class (and any other non-'list' classes)
  for (col_name in col_names) class(training[[col_name]]) <- "list"
  
  # adjust value bounds
  x_range <- training[, col_names, drop = FALSE] |> 
    unlist() |> 
    range(na.rm = TRUE, finite = TRUE)
  x_bound <- vapply(
    x_range,
    \(m) if (m == 0) m else sign(m) * 2 ^ ceiling(log(max(abs(m) + 1), 2)) - 1,
    0
  )
  if (x$xmin > x_range[[1L]]) x$xmin <- x_bound[[1L]]
  if (x$xmax > x_range[[2L]]) x$xmax <- x_bound[[2L]]
  
  # output prepped step
  step_blur_new(
    terms = col_names,
    role = x$role,
    trained = TRUE,
    xmin = x$xmin, xmax = x$xmax,
    sigma = x$sigma,
    columns = col_names,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_blur <- function(object, new_data, ...) {
  # save(object, new_data, file = here::here("step-blur-bake.rda"))
  # load(here::here("step-blur-bake.rda"))
  
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)
  # remove troublesome 'AsIs' class (and any other non-'list' classes)
  for (term in object$terms) class(new_data[[term]]) <- "list"
  
  # blur each image in each column
  blur_data <- tibble::tibble(.rows = nrow(new_data))
  for (term in object$terms) {
    term_blur <- lapply(
      new_data[[term]],
      \(d) blur(
        x = d,
        xmin = object$xmin,
        xmax = object$xmax,
        sigma = object$sigma
      )
    )
    blur_data[[paste(term, "blur", sep = "_")]] <- term_blur
  }
  
  check_name(blur_data, new_data, object)
  new_data <- vctrs::vec_cbind(new_data, blur_data)
  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

#' @export
print.step_blur <- function(
    x, width = max(20, options()$width - 35), ...
) {
  # save(x, width, file = here::here("step-blur-print.rda"))
  # load(here::here("step-blur-print.rda"))
  
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
required_pkgs.step_blur <- function(x, ...) {
  c("ripserr", "tdarec")
}

#' @rdname step_blur
#' @usage NULL
#' @export
tidy.step_blur <- function(x, ...) {
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
tunable.step_blur <- function(x, ...) {
  tibble::tibble(
    name = c("sigma"),
    call_info = list(
      list(pkg = "tdarec", fun = "blur_sigma", range = c(0, unknown()))
    ),
    source = "recipe",
    component = "step_blur",
    component_id = x$id
  )
}
