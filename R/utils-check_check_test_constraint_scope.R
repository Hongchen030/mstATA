#' @title Infer Application Scope for Test-Level Constraints
#'
#' @description
#'
#' This internal helper function determines the appropriate application scope
#' (i.e., "Module-level", "Pathway-level") for test-level categorical/quantitative
#' constraints in multistage test (MST) assembly.
#'
#' @param num_modules An integer indicating the number of modules in a panel.
#' @param which_module Integer vector of module indices to which the constraint applies.
#' @param num_pathways An integer indicating the number of allowable pathways in a MST panel.
#' @param which_pathway Integer vector of pathway indices to which the constraint applies.
#'
#' @details
#' If \code{which_module} and \code{which_pathway} both are NULL, then the constraint is applied to each module.
#'
#' @return A list with five elements:
#' \describe{
#'   \item{\code{application_level}}{A character string indicating the inferred application level: "Module-level", "Pathway-level".}
#'   \item{\code{which_module}}{The validated \code{which_module}, if applicable.}
#'   \item{\code{which_pathway}}{The validated \code{which_pathway}, if applicable.}
#'   \item{\code{total_rows}}{A scalar: either \code{x$NumModules} or \code{x$NumPathways}}
#'   \item{\code{row_ids}}{A numeric vector: either \code{which_module} or \code{which_pathway}}
#' }
#' @keywords internal
#' @noRd
#'


check_test_constraint_scope <- function(num_modules,num_pathways, which_module = NULL, which_pathway =NULL) {
  if (!is.null(which_module) && !is.null(which_pathway)) {
    stop("Specify either 'whicmodule' or 'which_pathway', not both.")
  }

  if(!is.null(which_module) && is.null(which_pathway)){
    which_module<-validate_module_selection(which_module = which_module,num_modules = num_modules)
    return(list(application_level = "Module-level", which_module = which_module, which_pathway = NULL,
                total_rows = num_modules,row_ids = which_module))
  }

  if(!is.null(which_pathway) && is.null(which_module)){
    which_pathway<-validate_pathway_selection(which_pathway = which_pathway,num_pathways = num_pathways)
    return(list(application_level = "Pathway-level", which_module = NULL, which_pathway = which_pathway,
                total_rows = num_pathways,row_ids = which_pathway))
  }

  if(is.null(which_module) && is.null(which_pathway)){
    which_module<-validate_module_selection(which_module = which_module,num_modules = num_modules)
    return(list(application_level = "Module-level", which_module = which_module, which_pathway = NULL,
                total_rows = num_modules,row_ids = which_module))
  }
}




