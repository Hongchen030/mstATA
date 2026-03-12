#' @keywords internal
#' @noRd

hard_term_index <- function(varnames) {

  # Pattern definitions
  is_x     <- grepl("^x\\[\\d+,\\d+,\\d+\\]$", varnames)
  is_s     <- grepl("^s\\[\\d+\\]$", varnames)
  is_slack <- grepl("^slack\\[\\d+\\]$", varnames)

  auxiliary <- !(is_x | is_s | is_slack)

  # Hard terms = everything except slack
  hard_term <- is_x | is_s| auxiliary

  if (!any(hard_term)) {
    stop("All variables are slack variables; hard term is empty.",
         call. = FALSE)
  }

  list(
    x_index = is_x,
    s_index = is_s,
    obj_index = auxiliary,
    slack_index = is_slack,
    hard_term_index = hard_term
  )
}
