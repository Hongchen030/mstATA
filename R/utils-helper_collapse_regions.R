#' @keywords internal
#' @noRd

collapse_regions <- function(global_next_mods, allowed_next_mods) {

  if (!all(allowed_next_mods %in% global_next_mods)) {
    stop("Allowed modules must be subset of global modules.", call. = FALSE)
  }

  global_idx <- seq_along(global_next_mods)
  allowed_idx <- match(allowed_next_mods, global_next_mods)

  region_to_module <- character(length(global_next_mods))

  for (i in global_idx) {

    if (global_next_mods[i] %in% allowed_next_mods) {

      region_to_module[i] <- global_next_mods[i]

    } else {

      # find nearest allowed module
      nearest <- allowed_idx[which.min(abs(allowed_idx - i))]
      region_to_module[i] <- global_next_mods[nearest]
    }
  }

  region_to_module
}
