#' @title Euler characteristic curve vectorization of persistent homology
#'
#' @description The function `step_vpd_ecc()` creates a _specification_ of a
#'   recipe step that will convert a list-column of 3-column matrix
#'   representations of persistence data to a list-column of 1-row matrices of
#'   vectorizations.
#'
#' @details
#' Persistent homology is usually encoded as birth--death pairs (barcodes or
#' diagrams), but the space of persistence data sets does not satisfy convenient
#' statistical properties. Such applications as hypothesis testing and machine
#' learning benefit from transformations of persistence data, often to Hilbert
#' spaces (vector spaces with inner products and induced metrics).
#' 
#' The Euler characteristic curve vectorization deploys
#' [TDAvec::computeECC()].
#'
#' The `hom_degree()` and `max_hom_degree` arguments determine the degree, or
#' the highest degree (starting with `0`), of the features to be transformed.
#' `xseq` will specify an entire discretization grid, while `xmin`, `xmax`, and
#' one of `xlen` and `xby` will construct a grid using [base::seq()].
#'
#' ```{r, echo=FALSE, results="asis"}
#' step <- "step_vpd_ecc"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#' 

#' @import recipes
#' @inheritParams recipes::step_pca
#' @inherit recipes::step_pca return

#' @param max_hom_degree,xseq,xmin,xmax,xlen,xby Parameters transformed and
#'   passed to {TDAvec}.
#' @example inst/examples/ex-step-vpd-ecc.R

#' @export
step_vpd_ecc <- function(
    recipe,
    ...,
    # standard inputs
    role = "predictor",
    trained = FALSE,
    # custom parameters
    max_hom_degree = Inf,
    xseq = NULL,
    xmin = NULL, xmax = NULL, xlen = NULL, xby = NULL,
    # standard parameters
    columns = NULL,
    keep_original_cols = TRUE,
    skip = FALSE,
    id = rand_id("vpd_ecc")
) {
  recipes_pkg_check(required_pkgs.step_vpd_ecc())
  
  # output the step
  add_step(
    recipe,
    step_vpd_ecc_new(
      terms = rlang::enquos(...),
      trained = trained,
      role = role,
      max_hom_degree = max_hom_degree,
      xseq = xseq,
      xmin = xmin, xmax = xmax, xlen = xlen, xby = xby,
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
    max_hom_degree,
    xseq,
    xmin, xmax, xlen, xby,
    columns, keep_original_cols,
    skip, id
) {
  step(
    subclass = "vpd_ecc",
    terms = terms,
    role = role,
    trained = trained,
    max_hom_degree = max_hom_degree,
    xseq = xseq,
    xmin = xmin, xmax = xmax, xlen = xlen, xby = xby,
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
  if (x$max_hom_degree == Inf) {
    train_max_hom_degrees <- purrr::map(
      training[, col_names, drop = FALSE],
      \(x) purrr::map_dbl(x, \(m) max(m[, 1L]))
    )
    x$max_hom_degree <- max(unlist(train_max_hom_degrees))
  }
  
  # if provided, use the full sequence
  if (! is.null(x$xseq)) {
    if (! is.null(x$xmin) || ! is.null(x$xmax) ||
        ! is.null(x$xlen) || ! is.null(x$xby)) {
      warning(
        "Pass either `xseq` alone or `xmin`, `xmax`, and `xlen` or `xby`, ",
        "not both."
      )
    }
  } else {
    # if needed, determine scale sequence
    if (is.null(x$xmax)) {
      train_xmax <- purrr::map(
        training[, col_names, drop = FALSE],
        \(x) purrr::map_dbl(x, \(m) max(m[, 3L]))
      )
      train_xmax <- min(unlist(train_xmax))
      x$xmax <- max(train_xmax, 0)
    }
    if (is.null(x$xmin)) {
      train_xmin <- purrr::map(
        training[, col_names, drop = FALSE],
        \(x) purrr::map_dbl(x, \(m) min(m[, 2L]))
      )
      train_xmin <- min(unlist(train_xmin))
      # only deviate from zero if it would reduce the grid size by at least half
      if (train_xmin < x$xmax * .5) train_xmin <- 0
      x$xmin <- train_xmin
    }
    if (is.null(x$xby)) {
      if (is.null(x$xlen)) {
        x$xlen <- 100
      }
      x$xby <- (x$xmax - x$xmin) / x$xlen
      # train_scale_seq <- seq(train_xmin, train_xmax, length.out = x$xlen)
    } else if (! is.null(x$xlen)) {
      # TODO: Make a macro to warn about redundant parameters.
      warning(
        "Both `xby` and `xlen` were passed; ",
        "only `xlen` value will be used."
      )
      x$xby <- (x$xmax - x$xmin) / x$xlen
      # train_scale_seq <- seq(train_xmin, train_xmax, length.out = x$xlen)
    } else {
      x$xlen <- (x$xmax - x$xmin) / x$xby
      # train_scale_seq <- seq(train_xmin, train_xmax, by = x$xby)
    }
    # calculate `xseq`
    x$xseq <- seq(x$xmin, x$xmax, length.out = x$xlen)
    # x$xmin <- x$xmax <- x$xlen <- x$xby <- NULL
  }
  
  # output prepped step
  step_vpd_ecc_new(
    terms = col_names,
    role = x$role,
    trained = TRUE,
    max_hom_degree = x$max_hom_degree,
    # scale_seq = train_scale_seq,
    xseq = x$xseq,
    xmin = x$xmin, xmax = x$xmax, xlen = x$xlen, xby = x$xby,
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
  
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)
  # remove troublesome 'AsIs' class (and any other non-'list' classes)
  for (col_name in col_names) class(new_data[[col_name]]) <- "list"
  
  # tabulate vectorizations of each persistence data column
  vph_data <- tibble::tibble(.rows = nrow(new_data))
  # TODO: compare with existing recipes and decide to store vectorizations as
  # * matrices in list columns (with or without names)
  # * data frames in list columns (require names)
  # * numerous additional columns (require names)
  for (col_name in col_names) {
    col_ecc <- purrr::map(
      new_data[[col_name]],
      \(d) TDAvec::computeECC(
        as.matrix(d),
        maxhomDim = object$max_hom_degree,
        # scaleSeq = object$scale_seq
        scaleSeq = object$xseq
      )
    )
    # col_ecc <- lapply(col_ecc, matrix, nrow = 1L)
    col_ecc <- purrr::map(
      col_ecc,
      \(v) as.data.frame(matrix(
        v, nrow = 1L, dimnames = list(NULL, seq(length(v)))
      ))
    )
    vph_data[[paste(col_name, "ecc", sep = "_")]] <- col_ecc
  }
  # unnest data-framed matrices to ensure commensurate columns
  vph_data <- tidyr::unnest(
    vph_data,
    cols = tidyr::all_of(paste(col_names, "ecc", sep = "_")),
    names_sep = "_"
  )
  
  check_name(vph_data, new_data, object)
  new_data <- vctrs::vec_cbind(new_data, vph_data)
  new_data <- remove_original_cols(new_data, object, col_names)
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

#' @rdname required_pkgs.tdarec
#' @export
required_pkgs.step_vpd_ecc <- function(x, ...) {
  c("TDAvec", "tdarec")
}

#' @export
tunable.step_vpd_ecc <- function(x, ...) {
  tibble::tibble(
    name = c("max_hom_degree"),
    call_info = list(
      list(pkg = "tdarec", fun = "max_hom_degree", range = c(0L, 3L))
    ),
    source = "recipe",
    component = "step_vpd_ecc",
    component_id = x$id
  )
}
