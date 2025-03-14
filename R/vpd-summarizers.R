#' @title Summarize topological data
#'
#' @description These miscellaneous functions are used by various
#'   `get_*_range()` functions to [finalize][dials::finalize()] hyperparameter
#'   ranges.
#'
#' @details The functions compute the following summaries:
#'
#' \itemize{
#'   \item `ph_dim()`:
#'         Dimension of a data set for purposes of PH
#'   \item `pairs_max()`:
#'         Maximum number of persistent pairs of any degree
#'   \item `pers_range()`:
#'         Range of positive finite persistence for a given degree
#'   \item `union_range()`:
#'         Union of birth--death ranges for a given degree
#' }
#' 
#' @name vpd-summarizers
#' @param x Persistence data in a recognizable format.
#' @param hom_degree,hom_degrees Integer (vector) of homological degree(s).
NULL

check_param <- getFromNamespace("check_param", "dials")

# FIXME: should be informed by engine & method, e.g. `ripserr::vietoris_rips()`
# versus `ripserr::cubical()` treat a 2-column matrix as a coordinate matrix and
# as a 2D image, respectively

# FIXME: This is designed for input data but should also apply to persistence
# data.

#' @rdname vpd-summarizers
#' @export
ph_dim <- function(x) {
  UseMethod("ph_dim")
}

#' @rdname vpd-summarizers
#' @export
ph_dim.default <- function(x) ncol(as.matrix(x))

#' @rdname vpd-summarizers
#' @export
ph_dim.matrix <- function(x) ncol(x)

#' @rdname vpd-summarizers
#' @export
ph_dim.array <- 
  function(x) if (is.matrix(x)) ph_dim.default(x) else length(dim(x))

#' @rdname vpd-summarizers
#' @export
ph_dim.data.frame <- function(x) ncol(x)

#' @rdname vpd-summarizers
#' @export
ph_dim.dist <- function(x) as.integer(attr(x, "Size"))

#' @rdname vpd-summarizers
#' @export
ph_dim.ts <- ph_dim.default

# TODO: Get maximum number of pairs for any single degree (not totaled).

#' @rdname vpd-summarizers
#' @export
pairs_max <- function(x, hom_degrees) {
  UseMethod("pairs_max")
}

#' @rdname vpd-summarizers
#' @export
pairs_max.default <- function(x, hom_degrees) {
  stop("Unrecognized persistent homology class.")
}

#' @rdname vpd-summarizers
#' @export
pairs_max.matrix <- function(x, hom_degrees) {
  if (is.null(hom_degrees)) {
    max(unname(table(x[, 1L])))
  } else {
    max(unname(table(x[x[, 1L] %in% hom_degrees, 1L])))
  }
}

#' @rdname vpd-summarizers
#' @export
pairs_max.data.frame <- 
  function(x, hom_degrees) pairs_max.matrix(x, hom_degrees)

#' @rdname vpd-summarizers
#' @export
pairs_max.diagram <- 
  function(x, hom_degrees) pairs_max.matrix(unclass(x), hom_degrees)

#' @rdname vpd-summarizers
#' @export
pairs_max.PHom <- 
  function(x, hom_degrees) pairs_max.data.frame(x, hom_degrees)

#' @rdname vpd-summarizers
#' @export
pairs_max.persistence <- function(x, hom_degrees) {
  if (is.null(hom_degrees)) {
    max(vapply(x$pairs, nrow, 0L))
  } else {
    max(vapply(x$pairs[hom_degrees + 1L], nrow, 0L))
  }
}

#' @rdname vpd-summarizers
#' @export
pers_range <- function(x, hom_degree) {
  UseMethod("pers_range")
}

#' @rdname vpd-summarizers
#' @export
pers_range.default <- function(x, hom_degree) {
  stop("Unrecognized persistent homology class.")
}

#' @rdname vpd-summarizers
#' @export
pers_range.matrix <- function(x, hom_degree) {
  if (is.null(hom_degree)) {
    val <- abs(x[, 3L] - x[, 2L])
    range(val[is.finite(val)])
  } else {
    deg <- x[, 1L] == hom_degree
    val <- abs(x[deg, 3L] - x[deg, 2L])
    range(val[is.finite(val)])
  }
}

#' @rdname vpd-summarizers
#' @export
pers_range.data.frame <- function(x, hom_degree) {
  if (is.null(hom_degree)) {
    val <- abs(x[[3L]] - x[[2L]])
    range(val[is.finite(val)])
  } else {
    deg <- x[[1L]] == hom_degree
    val <- abs(x[[3L]][deg] - x[[2L]][deg])
    range(val[is.finite(val)])
  }
}

#' @rdname vpd-summarizers
#' @export
pers_range.diagram <- 
  function(x, hom_degree) pers_range.matrix(unclass(x), hom_degree)

#' @rdname vpd-summarizers
#' @export
pers_range.PHom <- 
  function(x, hom_degree) pers_range.data.frame(x, hom_degree)

#' @rdname vpd-summarizers
#' @export
pers_range.persistence <- function(x, hom_degree) {
  if (is.null(hom_degree)) {
    range(sapply(x$pairs, function(y) {
      val <- abs(y[, 2L] - y[, 1L])
      range(val[is.finite(val)])
    }, simplify = TRUE))
  } else {
    val <-
      abs(x$pairs[[hom_degree + 1L]][, 2L] - x$pairs[[hom_degree + 1L]][, 1L])
    range(val[is.finite(val)])
  }
}

#' @rdname vpd-summarizers
#' @export
union_range <- function(x, hom_degree) {
  UseMethod("union_range")
}

#' @rdname vpd-summarizers
#' @export
union_range.default <- function(x, hom_degree) {
  stop("Unrecognized persistent homology class.")
}

#' @rdname vpd-summarizers
#' @export
union_range.matrix <- function(x, hom_degree) {
  if (is.null(hom_degree)) {
    range(as.vector(x[, c(2L, 3L)]))
  } else {
    range(as.vector(x[x[, 1L] == hom_degree, c(2L, 3L)]))
  }
}

#' @rdname vpd-summarizers
#' @export
union_range.data.frame <- function(x, hom_degree) {
  if (is.null(hom_degree)) {
    range(c(x[[2L]], x[[3L]]))
  } else {
    r <- x[[1L]] == hom_degree
    range(c(x[[2L]][r], x[[3L]][r]))
  }
}

#' @rdname vpd-summarizers
#' @export
union_range.diagram <- 
  function(x, hom_degree) union_range.matrix(unclass(x), hom_degree)

#' @rdname vpd-summarizers
#' @export
union_range.PHom <- 
  function(x, hom_degree) union_range.data.frame(x, hom_degree)

#' @rdname vpd-summarizers
#' @export
union_range.persistence <- function(x, hom_degree) {
  if (is.null(hom_degree)) {
    range(do.call(c, x$pairs))
  } else {
    range(x$pairs[[hom_degree + 1L]])
  }
}
