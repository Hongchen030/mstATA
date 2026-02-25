#' @keywords internal
#' @noRd
check_rdp <- function(rdp, ModuleIndex, NumStages) {

  if (is.null(rdp)) {
    return(NULL)
  }

  if (!is.list(rdp)) {
    stop("'rdp' must be a list with length NumStages - 1.",call. = FALSE)
  }

  if (length(rdp) != NumStages - 1L) {
    stop(paste0("'rdp' must have length ", NumStages - 1L,
                " (one routing stage per stage transition)."),
         call. = FALSE)
  }

  for (s in seq_len(NumStages - 1L)) {
    rdp_s <- rdp[[s]]
    if (!is.numeric(rdp_s) || anyNA(rdp_s)) {
      stop(paste0("'rdp[[", s, "]]' must be a numeric vector with no missing values."),
           call. = FALSE)
    }

    if (any(!is.finite(rdp_s))) {
      stop(paste0("'rdp[[", s, "]]' must contain only finite values."),
           call. = FALSE)
    }

    ## modules at stage s + 1
    next_modules <- ModuleIndex$module_index[ModuleIndex$stage == s + 1]
    n_expected <- length(next_modules) - 1L

    if (length(rdp_s) != n_expected) {
      stop(paste0("Routing after stage ", s," requires ", n_expected,
                  " routing decision points, but rdp[[", s, "]] has length ",length(rdp_s), "."),
           call. = FALSE)
    }

    if (is.unsorted(rdp_s)) {
      warning(paste0("'rdp[[", s, "]]' is not sorted; adjacency is assumed to follow order."))
    }

  }

  rdp
}
