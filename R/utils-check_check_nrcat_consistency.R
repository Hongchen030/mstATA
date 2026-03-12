#' @keywords internal
#' @noRd
#'
check_nrCat_consistency <- function(model, params, nrCat) {
  if (is.null(nrCat)) return(invisible(TRUE))
  if (!is.numeric(nrCat) || length(nrCat) != 1L || is.na(nrCat)) {
    stop("`nrCat` must be a single non-missing numeric value.", call. = FALSE)
  }
  model <- toupper(model)

  exp_nrCat <- expected_nrCat(model  = model,params = params)

  if (nrCat != exp_nrCat) {
    stop(
      sprintf(
        "Inconsistent nrCat for model '%s': expected %d based on model parameters, but got %d.",
        model, exp_nrCat, nrCat
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
