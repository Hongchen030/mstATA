#' @keywords internal
#' @noRd

parse_x_indices <- function(varname_df) {

  m <- regexec(
    "^x\\[\\s*(\\d+)\\s*,\\s*(\\d+)\\s*,\\s*(\\d+)\\s*\\]$",
    varname_df$varname
  )
  out <- regmatches(varname_df$varname, m)

  keep <- lengths(out) > 0
  if (!any(keep)) {
    return(cbind(varname_df[0, , drop = FALSE],
                 module_id = integer(0), item_id = integer(0), panel_id = integer(0)))
  }

  parsed <- do.call(rbind, lapply(out[keep], function(z) as.integer(z[-1])))
  colnames(parsed) <- c("module_id", "item_id", "panel_id")

  cbind(varname_df[keep, , drop = FALSE], parsed)
}
