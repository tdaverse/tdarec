#' @title Blur raster data
#'
#' @description The function `step_blur()` creates a _specification_ of a recipe
#'   step that will induce Gaussian blur in numerical arrays. The input and
#'   output must be list-columns.
#'
#' @details The gaussian blur step deploys [blur()]. See there for definitions
#'   and references.
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
#' @param xmin,xmax,blur_sigmas Parameters passed to [blur()].
#' @inheritParams recipes::step_pca
#' @inherit recipes::step_pca return
#' @example inst/examples/ex-step-blur.R

#' @export
step_blur <- function(
    recipe,
    ...,
    # standard inputs
    role = NA_character_,
    trained = FALSE,
    # custom parameters
    xmin = 0, xmax = 1,
    blur_sigmas = NULL,
    # standard parameters
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
      blur_sigmas = blur_sigmas,
      skip = skip,
      id = id
    )
  )
}

step_blur_new <- function(
    terms,
    role, trained,
    xmin, xmax,
    blur_sigmas,
    skip, id
) {
  step(
    subclass = "blur",
    terms = terms,
    role = role,
    trained = trained,
    xmin = xmin, xmax = xmax,
    blur_sigmas = blur_sigmas,
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
    function(l) all(vapply(l, function(m) is.array(m) && is.numeric(m), FALSE)),
    FALSE
  )))
    rlang::abort("The `blur` step can only transform lists of numeric arrays.")
  # remove troublesome 'AsIs' class (and any other non-'list' classes)
  for (col_name in col_names) class(training[[col_name]]) <- "list"
  
  # adjust value bounds
  x_range <- training[, col_names, drop = FALSE] %>% 
    unlist() %>% 
    range(na.rm = TRUE, finite = TRUE)
  x_bound <- vapply(
    x_range,
    function(m) if (m == 0) m else sign(m) * 2 ^ ceiling(log(max(abs(m) + 1), 2)) - 1,
    0
  )
  if (x$xmin > x_range[[1L]]) x$xmin <- x_bound[[1L]]
  if (x$xmax > x_range[[2L]]) x$xmax <- x_bound[[2L]]
  
  # default to median default blur within each column
  if (length(x$blur_sigmas) == 1L) {
    x$blur_sigmas <- rep(x$blur_sigmas, length(col_names))
    names(x$blur_sigmas) <- col_names
  } else if (is.null(x$blur_sigmas)) {
    blur_default <- function(m) max(dim(m)) / ( 2 ^ ( length(dim(m)) + 1 ) )
    x_blur_sigmas <- training[, col_names, drop = FALSE] %>% 
      lapply(function(l) vapply(l, blur_default, 0.)) %>% 
      vapply(stats::median, 0.)
    x$blur_sigmas <- x_blur_sigmas
  }
  
  # output prepped step
  step_blur_new(
    terms = col_names,
    role = x$role,
    trained = TRUE,
    xmin = x$xmin, xmax = x$xmax,
    blur_sigmas = x$blur_sigmas,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_blur <- function(object, new_data, ...) {
  # save(object, new_data, file = here::here("step-blur-bake.rda"))
  # load(here::here("step-blur-bake.rda"))
  
  col_names <- names(object$terms)
  check_new_data(col_names, object, new_data)
  # remove troublesome 'AsIs' class (and any other non-'list' classes)
  for (term in object$terms) class(new_data[[term]]) <- "list"
  
  # blur each image in each column
  for (col_name in col_names) {
    new_data[[col_name]] <- lapply(
      new_data[[col_name]],
      function(d) blur(
        x = d,
        xmin = object$xmin,
        xmax = object$xmax,
        sigma = object$blur_sigmas[col_name]
      )
    )
  }
  
  new_data
}

#' @export
print.step_blur <- function(
    x, width = max(20, options()$width - 35), ...
) {
  # save(x, width, file = here::here("step-blur-print.rda"))
  # load(here::here("step-blur-print.rda"))
  
  title <- "Gaussian blurring of "
  
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
required_pkgs.step_blur <- function(x, ...) {
  c("ripserr", "tdarec")
}

#' @rdname step_blur
#' @usage NULL
#' @export
tidy.step_blur <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble::tibble(
      terms = unname(x$terms),
      value = rep(NA_real_, length(x$terms))
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
    name = c("blur_sigmas"),
    call_info = list(
      list(pkg = "tdarec", fun = "blur_sigmas", range = c(0, unknown()))
    ),
    source = "recipe",
    component = "step_blur",
    component_id = x$id
  )
}
