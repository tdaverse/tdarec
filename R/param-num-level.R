#' @title (Maximum) number of persistence landscape levels
#'
#' @description The number of levels of a persistence landscape to vectorize.
#'
#' @details Persistence landscapes are natural number--indexed sequences of
#'   continuous real-valued functions derived from persistence data for a single
#'   homological degree. Most machine learning workflows vectorize the first
#'   \eqn{k} levels, for some \eqn{k>0} required by `num_level()`. In case an _a
#'   priori_ choice is not provided, `get_level_range()` queries each
#'   list-column for the number of persistent pairs of each degree (among
#'   `hom_degrees`, if specified); the maximum of these is taken as the upper
#'   bound on the level.
#'
#'   Currently, [TDAvec::computePL()] specifies only one level which to
#'   vectorize. When multiple levels are allowed, this parameter will be
#'   updated (and its name changed).
#'
#' @inheritParams hom_degree
#' @param hom_degrees,d Integer vector of homological degrees whose persistence
#'   pairs to count.
#' @example inst/examples/ex-param-num-level.R
#' @export
num_level <- function(range = c(1L, unknown()), trans = NULL) {
  new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(num_level = "Landscape Level"),
    finalize = get_level_range
  )
}

#' @rdname num_level
#' @export
get_level_range <- function(object, x, hom_degrees = NULL, ...) {
  check_param(object)
  
  rngs <- dials::range_get(object, original = FALSE)
  if (! dials::is_unknown(rngs$upper)) {
    return(object)
  }
  
  # check that all columns are list-columns of objects with sizes
  if (! all(vapply(x, typeof, "") == "list")) {
    rlang::abort("The `phom` step can only transform list-columns.")
  }
  
  # calculate maximum within-dimension level
  x_max_levs <- vapply(
    x,
    \(l) max(vapply(l, max_pairs, 0L, d = hom_degrees), na.rm = FALSE),
    0L
  )
  x_max_lev <- max(x_max_levs)
  
  # set the range based on the maximum observed and the minimum required
  rngs[2L] <- x_max_lev
  
  if (object$type == "integer" & is.null(object$trans)) {
    rngs <- as.integer(rngs)
  }
  
  dials::range_set(object, rngs)
}

# determine the number of persistent pairs for purposes of persistent homology

#' @rdname num_level
#' @export
max_pairs <- function(x, d) {
  UseMethod("max_pairs")
}

#' @rdname num_level
#' @export
max_pairs.default <- function(x, d) {
  stop("Unrecognized persistent homology class.")
}

#' @rdname num_level
#' @export
max_pairs.matrix <- function(x, d) {
  if (is.null(d)) {
    max(unname(table(x[, 1L])))
  } else {
    max(unname(table(x[x[, 1L] %in% d, 1L])))
  }
}

#' @rdname num_level
#' @export
max_pairs.data.frame <- function(x, d) max_pairs.matrix(x, d)

#' @rdname num_level
#' @export
max_pairs.diagram <- function(x, d) max_pairs.matrix(unclass(x), d)

#' @rdname num_level
#' @export
max_pairs.PHom <- function(x, d) max_pairs.data.frame(x, d)

#' @rdname num_level
#' @export
max_pairs.persistence <- function(x, d) {
  if (is.null(d)) {
    max(vapply(x$pairs, nrow, 0L))
  } else {
    max(vapply(x$pairs[d + 1L], nrow, 0L))
  }
}
