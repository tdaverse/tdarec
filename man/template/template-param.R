#' @name vpd-dials
#' @export
{dial_name} <- function(
    range = {dial_range}, trans = {dial_trans}
) {
  new_{dial_class}_param(
    type = {dial_type},
    range = range,
    inclusive = {dial_inclusive},
    trans = trans,
    label = c({dial_name} = "{dial_bullet}"),
    finalize = {dial_finalizer}
  )
}

