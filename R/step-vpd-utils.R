
# ensure that all columns are list-columns of 3-column persistence diagrams
check_pd_list <- function(data) {
  # check that all columns are list columns
  if (! all(vapply(data, typeof, "") == "list"))
    rlang::abort("The `vpd_*` steps can only transform list columns.")
  # TODO: Delete this conversion here if not needed for the function.
  # remove troublesome 'AsIs' class (and any other non-'list' classes)
  for (col in names(data)) class(data[[col]]) <- "list"
  # check that each list element is a 3-column matrix or data frame
  # TODO: Encode persistence data using a specialized class.
  are_pd <- function(l) all(vapply(
    l,
    function(x) ncol(x) == 3L && (
      (inherits(x, "data.frame") && 
         all(vapply(x, typeof, "") %in% c("double", "integer"))) ||
        (inherits(x, "matrix") && typeof(x) %in% c("double", "integer"))
    ),
    FALSE
  ))
  if (! all(vapply(data, are_pd, FALSE))) {
    rlang::abort("The `vpd_*` steps require 3-column persistence data.")
  }
}

# calculate the maximum homological degree of a list of persistence data sets
# TODO: Store this as an integer rather than a double.
get_max_hom_degree <- function(data) {
  max_degs <- purrr::map(
    data,
    function(x) purrr::map_dbl(x, function(m) max(m[, 1L]))
  )
  max(unlist(max_degs))
}

# calculate the distinct homological degrees of a list of persistence data sets
get_hom_degrees <- function(data) {
  degs <- purrr::map(
    data,
    function(x) purrr::map(x, function(m) unique(m[, 1L]))
  )
  sort(unique(unlist(degs)))
}

# extrema excluding infinities
min_finite <- function(x) min(x[is.finite(x)])
max_finite <- function(x) max(x[is.finite(x)])

# reconcile scale sequence parameters
reconcile_scale_seq <- function(x, data, axis) {
  stopifnot(axis %in% c("x", "y"))
  xyseq <- x[[paste0(axis, "seq")]]
  xymin <- x[[paste0(axis, "min")]]
  xymax <- x[[paste0(axis, "max")]]
  xylen <- x[[paste0(axis, "len")]]
  xyby <- x[[paste0(axis, "by")]]
  
  # if provided, use the full sequence
  if (! is.null(xyseq)) {
    if (! is.null(xymin) || ! is.null(xymax) ||
        ! is.null(xylen) || ! is.null(xyby)) {
      warning(
        "Pass either `", paste0(axis, "seq"), "` alone ",
        "or `", paste0(axis, "min"), "`, `", paste0(axis, "max"), "`, ",
        "and `", paste0(axis, "len"), "` or `", paste0(axis, "by"), "`, ",
        "not both."
      )
    }
  } else {
    # if needed, determine scale sequence
    if (is.null(xymax)) {
      train_xmax <- purrr::map(
        data,
        function(x) purrr::map_dbl(x, function(m) max_finite(m[, 3L]))
      )
      train_xmax <- min(unlist(train_xmax))
      xymax <- max(train_xmax, 0)
    }
    if (is.null(xymin)) {
      train_xmin <- purrr::map(
        data,
        function(x) purrr::map_dbl(x, function(m) min_finite(m[, 2L]))
      )
      train_xmin <- min(unlist(train_xmin))
      # TODO: Consult with specialists about this convention.
      # only deviate from zero if it would reduce the grid size by at least half
      if (train_xmin < xymax * .5) train_xmin <- 0
      xymin <- train_xmin
    }
    if (is.null(xyby)) {
      if (is.null(xylen)) xylen <- 100
      xyby <- (xymax - xymin) / xylen
    } else if (! is.null(xylen)) {
      # TODO: Make a macro to warn about redundant parameters.
      warning(
        "Both `", paste0(axis, "by"), "` and `", paste0(axis, "len"),
        "` were passed; only `", paste0(axis, "len"), "` value will be used."
      )
      xyby <- (xymax - xymin) / xylen
    } else {
      xylen <- (xymax - xymin) / xyby
    }
    # calculate `xseq`
    xyseq <- seq(xymin, xymax, length.out = xylen)
    # xymin <- xymax <- xylen <- xyby <- NULL
  }
  
  list(xyseq, xymin, xymax, xylen, xyby)
}

# assign meaningful names to vectorization features
vpd_suffix <- function(x, sep = "_") {
  if (is.matrix(x)) {
    paste(
      rep(colnames(x) %||% seq(ncol(x)), each = nrow(x)),
      rep(rownames(x) %||% seq(nrow(x)), times = ncol(x)),
      sep = sep
    )
  } else {
    names(x) %||% seq(length(x))
  }
}
