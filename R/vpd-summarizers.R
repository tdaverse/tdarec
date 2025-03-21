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
#'   \item `birth_range()`:
#'         Range of finite birth values for a given degree
#'   \item `pers_max()`:
#'         Maximum positive finite persistence for a given degree
#'   \item `pers_min()`:
#'         Minimum positive finite persistence for a given degree
#'   \item `pers_range()`:
#'         Range of positive finite persistence for a given degree
#'   \item `life_support()`:
#'         Union of birth--death ranges for a given degree
#' }
#' 
#' @name vpd-summarizers
#' @param x Persistence data in a recognizable format.
#' @param hom_degree,hom_degrees Integer (vector) of homological degree(s).
NULL

check_param <- getFromNamespace("check_param", "dials")

subset_positive_finite <- function(x) x[is.finite(x) & x > 0]

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
birth_range <- function(x, hom_degree) {
  UseMethod("birth_range")
}

#' @rdname vpd-summarizers
#' @export
birth_range.default <- function(x, hom_degree) {
  stop("Unrecognized persistent homology class.")
}

#' @rdname vpd-summarizers
#' @export
birth_range.matrix <- function(x, hom_degree) {
  if (is.null(hom_degree)) {
    range(x[is.finite(x[, 2L]), 2L])
  } else {
    deg <- x[, 1L] == hom_degree
    range(x[deg, 2L][is.finite(x[deg, 2L])])
  }
}

#' @rdname vpd-summarizers
#' @export
birth_range.data.frame <- function(x, hom_degree) {
  if (is.null(hom_degree)) {
    range(x[[2L]][is.finite(x[[2L]])])
  } else {
    deg <- x[[1L]] == hom_degree
    range(x[[2L]][deg][is.finite(x[[2L]][deg])])
  }
}

#' @rdname vpd-summarizers
#' @export
birth_range.diagram <- 
  function(x, hom_degree) birth_range.matrix(unclass(x), hom_degree)

#' @rdname vpd-summarizers
#' @export
birth_range.PHom <- 
  function(x, hom_degree) birth_range.data.frame(x, hom_degree)

#' @rdname vpd-summarizers
#' @export
birth_range.persistence <- function(x, hom_degree) {
  if (is.null(hom_degree)) {
    range(sapply(x$pairs, function(y) {
      range(y[is.finite(y[, 1L]), 1L])
    }, simplify = TRUE))
  } else {
    val <- x$pairs[[hom_degree + 1L]][, 1L]
    range(val[is.finite(val)])
  }
}

#' @rdname vpd-summarizers
#' @export
pers_max <- function(x, hom_degree) {
  UseMethod("pers_max")
}

#' @rdname vpd-summarizers
#' @export
pers_max.default <- function(x, hom_degree) {
  stop("Unrecognized persistent homology class.")
}

#' @rdname vpd-summarizers
#' @export
pers_max.matrix <- function(x, hom_degree) {
  if (is.null(hom_degree)) {
    max(subset_positive_finite(x[, 3L] - x[, 2L]))
  } else {
    deg <- x[, 1L] == hom_degree
    max(subset_positive_finite(x[deg, 3L] - x[deg, 2L]))
  }
}

#' @rdname vpd-summarizers
#' @export
pers_max.data.frame <- function(x, hom_degree) {
  if (is.null(hom_degree)) {
    max(subset_positive_finite(x[[3L]] - x[[2L]]))
  } else {
    deg <- x[[1L]] == hom_degree
    max(subset_positive_finite(x[[3L]][deg] - x[[2L]][deg]))
  }
}

#' @rdname vpd-summarizers
#' @export
pers_max.diagram <- 
  function(x, hom_degree) pers_max.matrix(unclass(x), hom_degree)

#' @rdname vpd-summarizers
#' @export
pers_max.PHom <- 
  function(x, hom_degree) pers_max.data.frame(x, hom_degree)

#' @rdname vpd-summarizers
#' @export
pers_max.persistence <- function(x, hom_degree) {
  if (is.null(hom_degree)) {
    max(sapply(x$pairs, function(y) {
      max(subset_positive_finite(y[, 2L] - y[, 1L]))
    }, simplify = TRUE))
  } else {
    max(subset_positive_finite(x$pairs[[hom_degree + 1L]][, 2L] -
                                 x$pairs[[hom_degree + 1L]][, 1L]))
  }
}

#' @rdname vpd-summarizers
#' @export
pers_min <- function(x, hom_degree) {
  UseMethod("pers_min")
}

#' @rdname vpd-summarizers
#' @export
pers_min.default <- function(x, hom_degree) {
  stop("Unrecognized persistent homology class.")
}

#' @rdname vpd-summarizers
#' @export
pers_min.matrix <- function(x, hom_degree) {
  if (is.null(hom_degree)) {
    min(subset_positive_finite(x[, 3L] - x[, 2L]))
  } else {
    deg <- x[, 1L] == hom_degree
    min(subset_positive_finite(x[deg, 3L] - x[deg, 2L]))
  }
}

#' @rdname vpd-summarizers
#' @export
pers_min.data.frame <- function(x, hom_degree) {
  if (is.null(hom_degree)) {
    min(subset_positive_finite(x[[3L]] - x[[2L]]))
  } else {
    deg <- x[[1L]] == hom_degree
    min(subset_positive_finite(x[[3L]][deg] - x[[2L]][deg]))
  }
}

#' @rdname vpd-summarizers
#' @export
pers_min.diagram <- 
  function(x, hom_degree) pers_min.matrix(unclass(x), hom_degree)

#' @rdname vpd-summarizers
#' @export
pers_min.PHom <- 
  function(x, hom_degree) pers_min.data.frame(x, hom_degree)

#' @rdname vpd-summarizers
#' @export
pers_min.persistence <- function(x, hom_degree) {
  if (is.null(hom_degree)) {
    min(sapply(x$pairs, function(y) {
      min(subset_positive_finite(y[, 2L] - y[, 1L]))
    }, simplify = TRUE))
  } else {
    min(subset_positive_finite(x$pairs[[hom_degree + 1L]][, 2L] -
                                 x$pairs[[hom_degree + 1L]][, 1L]))
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
    range(subset_positive_finite(abs(x[, 3L] - x[, 2L])))
  } else {
    deg <- x[, 1L] == hom_degree
    range(subset_positive_finite(abs(x[deg, 3L] - x[deg, 2L])))
  }
}

#' @rdname vpd-summarizers
#' @export
pers_range.data.frame <- function(x, hom_degree) {
  if (is.null(hom_degree)) {
    range(subset_positive_finite(abs(x[[3L]] - x[[2L]])))
  } else {
    deg <- x[[1L]] == hom_degree
    range(subset_positive_finite(abs(x[[3L]][deg] - x[[2L]][deg])))
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
      range(subset_positive_finite(abs(y[, 2L] - y[, 1L])))
    }, simplify = TRUE))
  } else {
    range(subset_positive_finite(abs(x$pairs[[hom_degree + 1L]][, 2L] -
                                       x$pairs[[hom_degree + 1L]][, 1L])))
  }
}

#' @rdname vpd-summarizers
#' @export
life_support <- function(x, hom_degree) {
  UseMethod("life_support")
}

#' @rdname vpd-summarizers
#' @export
life_support.default <- function(x, hom_degree) {
  stop("Unrecognized persistent homology class.")
}

#' @rdname vpd-summarizers
#' @export
life_support.matrix <- function(x, hom_degree) {
  if (is.null(hom_degree)) {
    range(as.vector(x[, c(2L, 3L)]))
  } else {
    range(as.vector(x[x[, 1L] == hom_degree, c(2L, 3L)]))
  }
}

#' @rdname vpd-summarizers
#' @export
life_support.data.frame <- function(x, hom_degree) {
  if (is.null(hom_degree)) {
    range(c(x[[2L]], x[[3L]]))
  } else {
    r <- x[[1L]] == hom_degree
    range(c(x[[2L]][r], x[[3L]][r]))
  }
}

#' @rdname vpd-summarizers
#' @export
life_support.diagram <- 
  function(x, hom_degree) life_support.matrix(unclass(x), hom_degree)

#' @rdname vpd-summarizers
#' @export
life_support.PHom <- 
  function(x, hom_degree) life_support.data.frame(x, hom_degree)

#' @rdname vpd-summarizers
#' @export
life_support.persistence <- function(x, hom_degree) {
  if (is.null(hom_degree)) {
    range(do.call(c, x$pairs))
  } else {
    range(x$pairs[[hom_degree + 1L]])
  }
}
