#' @keywords internal
#' @noRd

expand_binary_dv_to_panels <- function(dv_names_single, num_panels) {

  if (!is.character(dv_names_single)) {
    stop("`dv_names_single` must be a character vector.", call. = FALSE)
  }

  if (num_panels == 1L) {
    expanded <- sub("\\]$", paste0(",", num_panels, "]"), dv_names_single)
  }else{
    expanded <- unlist(
      lapply(seq_len(num_panels), function(p) {
        sub("\\]$", paste0(",", p, "]"), dv_names_single)
      }),
      use.names = FALSE
    )
  }
  return(expanded)

}
