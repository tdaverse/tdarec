#' @name vpd-dials
#' @export
{dial_name} <- function(
    {dial_scope} = {dial_range_value}, trans = {dial_trans}
) {
  new_{dial_class}_param(
    type = {dial_type},
    {dial_scope} = {dial_scope},
    inclusive = {dial_inclusive},
    trans = trans,
    label = c({dial_name} = "{dial_bullet}"),
    finalize = {dial_finalizer}
  )
}

