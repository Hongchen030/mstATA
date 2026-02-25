#' @title Generate Stimulus-Level Constraints to Explicitly Select or Not Select Specific Stimuli
#' in MST Panel Assembly
#'
#' @description
#' This function generates linear constraints that force specific stimuli to be
#' either selected or not selected in the MST panel assembly.
#'
#' This constraint can be enforced at the `"Module-level"`, `"Pathway-level"`, or
#' `"Panel-level"`.
#'
#' The number of generated linear constraints depends on the application level:
#'
#' \itemize{
#'   \item \strong{Module-level:} when \code{which_module} is provided and
#'         \code{which_pathway = NULL}
#'
#'  The number of constraints is
#'    \strong{(number of stimuli in \code{stim_ids}) × (number of modules specified)}
#'
#'   \item \strong{Pathway-level:} when \code{which_pathway} is provided and
#'         \code{which_module = NULL}
#'
#'  The number of constraints is
#'    \strong{(number of stimuli in \code{stim_ids}) × (number of pathways specified)}
#'
#'   \item \strong{Panel-level:} both \code{which_module} and \code{which_pathway} are \code{NULL}
#'
#'  The number of constraints is
#'    \strong{(number of stimuli in \code{stim_ids})}
#' }
#'
#' @param x An object of class `"mstATA_design"` created by `mst_design()`.
#' @param stim_ids   A numeric vector of stimuli indices or a character vector of stimuli names
#' @param select Logical. Default is TRUE.
#' @param which_module Integer vector of modules. Must be consistent with the choices made in
#'   \code{test_stimcount_con()}, \code{test_stimcat_con()},
#'   and \code{test_stimquant_con()}.
#' @param which_pathway Integer vector of pathways. Must be consistent with the choices made in
#'   \code{test_stimcount_con()}, \code{test_stimcat_con()},
#'   and \code{test_stimquant_con()}.
#'
#' @details
#' **1. Specification**
#'
#' The constraint enforced by this function is:
#'
#' \strong{The specified stimuli must be selected or not selected in the chosen set of
#' modules, pathways, or across the entire panel.}
#'
#' The key properties of this constraint are:
#'
#' \itemize{
#'
#'   \item The attribute type is \emph{categorical}: each stimulus has a unique
#'   ID and may be treated as an unique category.
#'
#'   \item The constraint is defined at the \emph{stimulus level} in the item
#'   pool (each stimulus is a distinct entity).
#'
#'   \item The constraint can be applied at:
#'     \itemize{
#'       \item `"Module-level"` → stimulus must be selected/not selected in the
#'              specified module(s).
#'       \item `"Pathway-level"` → stimulus must be selected/not selected in one
#'              of the modules composing that pathway.
#'       \item `"Panel-level"` → stimulus must be selected/not selected somewhere in the
#'              panel (but placement is not specified).
#'     }
#'
#' }
#'
#' **2. Interaction with \code{panel_itemreuse_con()}**
#'
#' When \code{overlap = FALSE} in \code{panel_itemreuse_con()}, each item can at most be selected once
#' in a panel. This also indicates that each stimulus pivot item—may appear in at most one module in the
#' panel.
#'
#' Therefore:
#'
#' \itemize{
#'   \item \code{which_module} or \code{which_pathway} must be a scalar (not
#'   a vector).
#'   \item If both are \code{NULL}, the constraint simply enforces that the
#'         stimulus must appear somewhere in the panel, without specifying the
#'         module/pathway.
#' }
#'
#' @section Mathematical Formulation:
#'
#' Suppose the item pool contains (S - 1) stimulus-based item sets, indexed by
#' \eqn{s = 1, \ldots, S - 1}. Each stimulus has a designated pivot item,
#' indexed by \eqn{i_s^{*}}. In addition, the pool contains a set of discrete
#' (non–stimulus-based) items, which are represented by a dummy stimulus
#' \eqn{s = S} to allow a unified indexing scheme. Items belonging to stimulus
#' \eqn{s} are indexed as \eqn{i_s = 1, 2, \ldots, I_s}.
#'
#' Suppose there are \eqn{M} modules in an MST panel. Let
#' \eqn{m = 1, \ldots, M} denote the module index.
#'
#'
#' **1. Module-level stimulus selection/not selection**
#'
#' In specified module m: for all \eqn{s} (s= 1,2...,S-1) specified in \code{stim_ids}
#'
#' \deqn{
#'   x_{i_s^{*},m} = 1 \quad (\text{selected}), \qquad
#'   x_{i_s^{*},m} = 0 \quad (\text{not selected}), \qquad
#' }
#'
#'
#' **2. Pathway-level stimulus selection/not selection**
#'
#' In specified pathway r: for all \eqn{s} (s= 1,2...,S-1) specified in \code{stim_ids}
#'
#' \deqn{
#'   \sum_{m \in r} x_{i_s^{*},m} = 1 \quad (\text{selected}), \qquad
#'   \sum_{m \in r} x_{i_s^{*},m} = 0 \quad (\text{not selected}), \qquad
#' }
#'
#'
#' **3. Panel-level stimulus selection/not selection**
#'
#' In a panel: for all \eqn{s} (s= 1,2...,S-1) specified in \code{stim_ids}
#'
#' \deqn{
#'   \sum_{m} x_{i_s^{*},m} = 1 \quad (\text{selected}), \qquad
#'   \sum_{m} x_{i_s^{*},m} = 0 \quad (\text{not selected}), \qquad
#' }
#'
#' Here:
#' \itemize{
#'   \item \eqn{x_{i_s^{*},m}} indicates whether the pivot item for stimulus
#'   s is selected into module m, thereby indicating whether the
#'   stimulus is selected in that module.
#'
#'   \item \eqn{m \in r} denote modules belonging to pathway r.
#' }
#'
#'
#' @return An object of S3 class \code{"mstATA_constraint"} with named elements:
#' \describe{
#'   \item{name}{A character vector indicating the specifications in each row of `A_binary`}
#'   \item{specification}{A \code{data.frame} summarizing the constraint specification, including
#'    the requirement name, attribute, constraint type, application level,
#'    operator, and the number of constraint rows generated.}
#'   \item{A_binary}{A sparse binary matrix representing the linear constraint coefficients.}
#'   \item{A_real}{NULL for 'mstATA_constraint' object}
#'   \item{operators}{A character vector of constraint operators, one per row of `A_binary`.}
#'   \item{d}{A right-hand-side vector of 1s or 0s depending on whether \code{select} is \code{TRUE} or \code{FALSE}.}
#'   \item{C_binary}{NULL for 'mstATA_constraint' object}
#'   \item{C_real}{NULL for 'mstATA_constraint' object}
#'   \item{sense}{NULL for 'mstATA_constraint' object}
#' }
#' @examples
#' data("reading_itempool")
#' pivot_stim_map <- create_pivot_stimulus_map(
#'   itempool = reading_itempool,
#'   stimulus = "stimulus",
#'   pivot_item = "pivot_item"
#' )
#'
#' test_mstATA <- mst_design(
#'   itempool = reading_itempool,
#'   design = "1-3-3",
#'   module_length = c(5,7,7,7,8,8,8),
#'   pivot_stim_map = pivot_stim_map
#' )
#'
#' # Example 1: Force stimulus 1 to be selected in the routing module
#' stimcat_con(
#'   x = test_mstATA,
#'   stim_ids = 1,
#'   select = TRUE,
#'   which_module = 1
#' )
#'
#' # Example 2: Force stimulus 1 to be included somewhere in the MST panel
#' stimcat_con(
#'   x = test_mstATA,
#'   stim_ids = 1,
#'   select = TRUE
#' )
#'
#' # Example 3: Stimulus 1 must appear in stage-2 easy and medium modules
#' stimcat_con(
#'   x = test_mstATA,
#'   stim_ids = 1,
#'   select = TRUE,
#'   which_module = c(2,3)
#' )
#'
#' # Example 4: Stimulus 1 must appear in the REE pathway
#' stimcat_con(
#'   x = test_mstATA,
#'   stim_ids = 1,
#'   select = TRUE,
#'   which_pathway = 1
#' )
#'
#' # Example 5: Stimuli 1 and 3 must appear in the routing module
#' stimcat_con(
#'   x = test_mstATA,
#'   stim_ids = c(1,3),
#'   select = TRUE,
#'   which_module = 1
#' )
#'
#' # Example 6: Stimuli 1 and 3 must appear in the REE pathway
#' stimcat_con(
#'   x = test_mstATA,
#'   stim_ids = c(1,3),
#'   select = TRUE,
#'   which_pathway = 1
#' )
#'
#' # Example 7: Stimuli 1 and 3 must appear somewhere in the panel
#' stimcat_con(
#'   x = test_mstATA,
#'   stim_ids = c(1,3),
#'   select = TRUE
#' )
#'
#' @export


stimcat_con <- function(x,stim_ids, select = TRUE,
                        which_module=NULL,which_pathway=NULL) {

  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.")
  }

  pivot_stim_map<-x$pivot_stim_map
  if (is.null(pivot_stim_map)) {
    stop(
      "Stimulus-based constraints require `pivot_stim_map`. ",
      "Create it using `create_pivot_stimulus_map()` and supply it via `mst_design()`.",
      call. = FALSE
    )
  }
  stimulus_name<-pivot_stim_map$stimulus_name
  pivot_items<-pivot_stim_map$pivot_item_id
  NumStimuli<-length(pivot_items)

  ItemPool<-x$ItemPool

  if (any(duplicated(stim_ids))) {
    warning("Duplicate values in 'stim_ids' detected; duplicates removed.")
    stim_ids <- unique(stim_ids)
  }

  if(is.character(stim_ids)){
    if(!all(stim_ids%in%stimulus_name)){
      bad <- stim_ids[!stim_ids %in% stimulus_name]
      stop("Stimulus ", paste(bad, collapse = ", "), " not in the item pool.")
    }
    pivot_ids <- pivot_items[match(stim_ids, stimulus_name)]
  }else if(is.numeric(stim_ids)){
    if (any(stim_ids < 1 | stim_ids > NumStimuli)) {
      stop("'stim_ids' must be valid stimulus indices between 1 and ", NumStimuli, ".")
    }
    pivot_ids<-pivot_items[stim_ids]
  }else{
    stop("'stim_ids' must be either character stimulus names or numeric indices.")
  }

  out<-itemcat_con(x = x,item_ids = pivot_ids,select = select,
                  which_module = which_module,which_pathway = which_pathway)
  new_names <- out$name
  for (i in seq_along(pivot_ids)) {
    pid <- pivot_ids[i]
    sname <- stimulus_name[pivot_items == pid]
    new_names <- gsub(pattern = paste0("item ", pid),
                      replacement = paste0("stimulus ", sname),
                      x = new_names,fixed = TRUE)
  }
  out$name <- new_names

  out$specification$Requirement <- if (select) {
    paste("Select stimulus", paste(stim_ids, collapse = "/"))
  } else {
    paste("Not select stimulus", paste(stim_ids, collapse = "/"))
  }

  out$specification$Attribute<-"Stimulus_id"

  return(out)
}

