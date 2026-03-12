#' @keywords internal
#' @noRd
check_item_module_eligibility <- function(item_module_eligibility,
                                          index_map,
                                          NumModules) {
  pool_size <- length(index_map)
  ## ---- basic checks ----
  if (is.null(item_module_eligibility)) {
    # NULL means no restriction at all
    return(
      setNames(
        replicate(NumModules, seq_len(pool_size), simplify = FALSE),
        as.character(seq_len(NumModules))
      )
    )
  }

  if (!is.list(item_module_eligibility) || is.null(names(item_module_eligibility))) {
    stop("`item_module_eligibility` must be a named list.",call. = FALSE)
  }

  ## ---- module name checks ----
  mod_names <- names(item_module_eligibility)
  if (any(is.na(suppressWarnings(as.integer(mod_names))))) {
    stop("All names in `item_module_eligibility` must be module indices.",call. = FALSE)
  }
  mod_index <- as.integer(mod_names)
  if (any(mod_index < 1 | mod_index > NumModules)) {
    stop(sprintf("Module indices must be between 1 and %d.",NumModules),call. = FALSE)
  }

  ## ---- normalize each module ----
  normalized <- vector("list", NumModules)
  names(normalized) <- as.character(seq_len(NumModules))

  for (m in seq_len(NumModules)) {
    key <- as.character(m)

    if (!key %in% names(item_module_eligibility)) {
      # default: all items
      normalized[[key]] <- seq_len(pool_size)
      next
    }

    items <- item_module_eligibility[[key]]

    if (length(items) == 0L) {
      stop(
        sprintf("Module %s has an empty eligibility set.", key),
        call. = FALSE
      )
    }

    ## numeric item indices
    if (is.numeric(items)) {
      if (any(items < 1 | items > pool_size)) {
        stop(
          sprintf("Invalid item index in module %s.", key),
          call. = FALSE
        )
      }
      normalized[[key]] <- sort(unique(as.integer(items)))
      next
    }

    ## character item IDs
    if (is.character(items)) {

      if (!all(items %in% names(index_map))) {
        bad <- setdiff(items, names(index_map))
        stop(
          sprintf(
            "Unknown item IDs in module %s: %s",
            key,
            paste(bad, collapse = ", ")
          ),
          call. = FALSE
        )
      }

      normalized[[key]] <- sort(unique(index_map[items]))
      next
    }

    stop(
      sprintf(
        "Items for module %s must be numeric indices or character IDs.",
        key
      ),
      call. = FALSE
    )
  }

  normalized
}
