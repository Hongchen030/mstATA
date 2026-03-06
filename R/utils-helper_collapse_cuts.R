#' @keywords internal
#' @noRd
#'
collapse_cuts <- function(lower, upper, region_to_module) {

  if (length(lower) != length(upper) ||
      length(lower) != length(region_to_module)) {
    stop("Lengths of lower, upper, and region_to_module must match.",
         call. = FALSE)
  }

  # Identify boundaries between different modules
  change_points <- c(TRUE,
                     region_to_module[-1] != region_to_module[-length(region_to_module)])

  # Group indices by module blocks
  block_id <- cumsum(change_points)

  # Unique modules in order of appearance
  collapsed_mods <- region_to_module[change_points]

  # For each block, compute new bounds
  collapsed_lower <- tapply(lower, block_id, min)
  collapsed_upper <- tapply(upper, block_id, max)

  list(
    module = collapsed_mods,
    lower  = as.numeric(collapsed_lower),
    upper  = as.numeric(collapsed_upper)
  )
}
