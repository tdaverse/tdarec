#' @title Finalizers for persistent homology vectorizations
#'
#' @description These functions take a persistent homology vectorization
#'   parameter object and modify the [dials::unknown()] parts of `ranges` based
#'   on a data set and heuristics used in inaugural studies.
#' 
#' @name vpd-finalizers
#' @inheritParams vpd-summarizers
#' @inheritParams dials::finalize
#' @param ... Other arguments to pass to the underlying parameter finalizer
#'   functions.
#' @inherit dials::finalize return

#' @rdname vpd-finalizers
#' @export
get_pers_frac_range <- function(
    object, x, hom_degree = NULL, log_vals = TRUE, frac = c(1/100, 1/10), ...
) {
  check_param(object)
  
  rngs <- dials::range_get(object, original = FALSE)
  if (! dials::is_unknown(rngs$lower) && ! dials::is_unknown(rngs$upper)) {
    return(object)
  }
  
  # check that all columns are list-columns of objects with sizes
  if (! all(vapply(x, typeof, "") == "list")) {
    rlang::abort("This step can only transform list-columns.")
  }
  
  # calculate maximum within-dimension persistence
  x_pers_ranges <- sapply(
    x,
    function(l) {
      val <- 
        sapply(l, pers_range, hom_degree = hom_degree, simplify = TRUE)
      range(val[is.finite(val)])
    },
    simplify = TRUE
  )
  x_pers_range <- range(setdiff(x_pers_ranges, 0)) * frac
  
  # set the range based on the maximum observed and a fixed scale factor smaller
  if (dials::is_unknown(rngs$lower)) {
    rngs[1L] <- 
      if (log_vals) log10(x_pers_range[1L]) else x_pers_range[1L]
  }
  if (dials::is_unknown(rngs$upper)) {
    rngs[2L] <- 
      if (log_vals) log10(x_pers_range[2L]) else x_pers_range[2L]
  }

  if (object$type == "integer" & is.null(object$trans)) {
    rngs <- as.integer(rngs)
  }

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
