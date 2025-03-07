#' @title Finalizers for persistent homology vectorizations
#'
#' @description These functions take a persistent homology vectorization
#'   parameter object and modify the [dials::unknown()] parts of `ranges` based
#'   on a data set and heuristics used in inaugural studies.
#' 
#' @name vpd-finalizers
#' @inherit dials::finalize return

#' @rdname vpd-finalizers
#' @export
get_persistence_range <- function(object, x, hom_degree = NULL, ...) {
  check_param(object)
  
  rngs <- dials::range_get(object, original = FALSE)
  # TODO: Account for partial user specifications?
  if (! dials::is_unknown(rngs$lower) && ! dials::is_unknown(rngs$upper)) {
    return(object)
  }
  
  # check that all columns are list-columns of objects with sizes
  if (! all(vapply(x, typeof, "") == "list")) {
    rlang::abort("This step can only transform list-columns.")
  }
  
  # calculate maximum within-dimension persistence
  x_persistence_ranges <- sapply(
    x,
    function(l) {
      val <- 
        sapply(l, persistence_range, hom_degree = hom_degree, simplify = TRUE)
      range(val[is.finite(val)])
    },
    simplify = TRUE
  )
  x_persistence_range <- range(x_persistence_ranges)
  
  # set the range based on the maximum observed and a fixed scale factor smaller
  # FIXME: Do this transformation in the standard way.
  if (! dials::is_unknown(rngs$lower)) {
    rngs[1L] <- log(x_persistence_range[1L])
  }
  if (! dials::is_unknown(rngs$upper)) {
    rngs[2L] <- log(x_persistence_range[2L])
  }
  
  # if (object$type == "integer" & is.null(object$trans)) {
  #   rngs <- as.integer(rngs)
  # }
  
  dials::range_set(object, rngs)
}

#' @rdname vpd-finalizers
#' @export
get_pairs_max <- function(object, x, hom_degrees = NULL, ...) {
  check_param(object)
  
  rngs <- dials::range_get(object, original = FALSE)
  if (! dials::is_unknown(rngs$upper)) {
    return(object)
  }
  
  # check that all columns are list-columns of objects with sizes
  if (! all(vapply(x, typeof, "") == "list")) {
    rlang::abort("This step can only transform list-columns.")
  }
  
  # calculate maximum within-dimension number of pairs
  x_pairs_max <- vapply(
    x,
    \(l) max(
      vapply(l, pairs_max, 0L, hom_degrees = hom_degrees),
      na.rm = FALSE
    ),
    0L
  )
  x_max_pair <- max(x_pairs_max)
  
  # set the range based on the maximum observed and the minimum required
  rngs[2L] <- x_max_pair
  
  if (object$type == "integer" & is.null(object$trans)) {
    rngs <- as.integer(rngs)
  }
  
  dials::range_set(object, rngs)
}
