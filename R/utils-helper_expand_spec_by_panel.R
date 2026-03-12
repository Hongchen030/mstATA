#' @keywords internal
#' @noRd
expand_spec_by_panel <- function(spec_single,rows_per_panel,panels) {
  do.call(rbind,lapply(panels, function(panel_id) {
    out <- spec_single
    out$Panel <- panel_id
    out$Row_Start <- out$Row_Start + (panel_id - 1L) * rows_per_panel
    out$Row_End   <- out$Row_End   + (panel_id - 1L) * rows_per_panel
    out}))
}
