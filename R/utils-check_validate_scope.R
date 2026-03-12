#' @title Validate Application Scope for An Objective Term
#' @description
#'
#' This internal helper function validates the appropriate application scope
#' (i.e., "Module-level", "Pathway-level","Panel-level") for an objective term.
#'
#' Specifically, this function ensures that the user-specified
#' \code{applied_level} ("Module-level", "Pathway-level", or "Panel-level")
#' is compatible with the corresponding inputs \code{which_module} and
#' \code{which_pathway}.
#'
#' @param applied_level A character string specifying the intended scope of the
#'   constraint. Must be one of:
#'   \code{"Module-level"}, \code{"Pathway-level"}, or \code{"Panel-level"}.
#' @param num_modules An integer indicating the number of modules in a panel.
#' @param which_module An integer indicating which the constraint applies.
#' @param num_pathways An integer indicating the number of allowable pathways in a MST panel.
#' @param which_pathway An integer indicating which the constraint applies.
#' @details
#' This function enforces the following rules:
#'
#' \itemize{
#'
#'   \item **Module-level:**
#'     \code{which_module} must be supplied and must be a scalar.
#'     \code{which_pathway} must be \code{NULL}.
#'
#'   \item **Pathway-level:**
#'     \code{which_pathway} must be supplied and must be a scalar.
#'     \code{which_module} must be \code{NULL}.
#'
#'   \item **Panel-level:**
#'     Neither \code{which_module} nor \code{which_pathway} are supplied.
#'
#' }
#'
#' @return
#' A list with validated fields:
#' \describe{
#'   \item{\code{which_module}}{Validated module index (if applicable).}
#'   \item{\code{which_pathway}}{Validated pathway index (if applicable).}
#' }
#'
#' @keywords internal
#' @noRd
#'
validate_scope<- function(applied_level,which_module,which_pathway,num_modules,num_pathways) {

  scope<-match.arg(applied_level,c("Module-level","Pathway-level","Panel-level"))

  ## MODULE SCOPE
  if (scope == "Module-level") {
    if (is.null(which_module)) {
      stop("scope='Module-level' requires 'which_module' to be provided.")
    }
    if (!is.null(which_module) && length(which_module) != 1L) {
      stop("'which_module' must be a scalar (length 1) if provided.")
    }
    if (!is.null(which_pathway)) {
      stop("scope='Module-level' requires 'which_pathway' = NULL.")
    }
    which_module<-validate_module_selection(which_module = which_module,num_modules = num_modules)
    return(list(which_module = which_module, which_pathway = NULL))
  }

  ## PATHWAY SCOPE
  if (scope == "Pathway-level") {
    if (is.null(which_pathway)) {
      stop("scope='Pathway-level' requires 'which_pathway' to be provided.")
    }
    if (!is.null(which_pathway) && length(which_pathway) != 1L) {
      stop("'which_pathway' must be a scalar (length 1) if provided.")
    }
    if (!is.null(which_module)) {
      stop("scope='Pathway-level' requires 'which_module' = NULL.")
    }
    which_pathway<-validate_pathway_selection(which_pathway = which_pathway,num_pathways = num_pathways)
    return(list(which_module = NULL, which_pathway = which_pathway))
  }

  ## PANEL SCOPE
  if (scope == "Panel-level") {
    if (!is.null(which_module) || !is.null(which_pathway)) {
      stop("scope='Panel-level' does not accept which_module or which_pathway.")
    }
    return(list(which_module = NULL, which_pathway = NULL))
  }
}
