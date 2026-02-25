#' @title Assemble Selected Items into MST panel
#'
#' @description
#' Constructs a panel-centric representation of an assembled multistage panel
#' (MST) from the solver output. The function extracts binary decision
#' variables, maps them to items in the item pool, and organizes the
#' results by panel, module, and pathway.
#'
#' This function is typically called after \code{solve_model()} and is intended
#' for post-solution interpretation, diagnostics, and reporting.
#'
#' @param x An object of class \code{"mstATA_design"} created by \code{mst_design()}.
#' @param result A solver result returned by \code{solve_model()}, containing
#'   components \code{model} and \code{solution}.
#'
#' @details
#' The workflow is:
#' \enumerate{
#'   \item Extract item-module-panel binary decision variables with value 1 from the solver solution.
#'   \item Parse decision variable names of the form
#'     \code{x[module_id, item_id, panel_id]}.
#'   \item Map internal item indices (\code{item_id}) to item-module-panel identifiers.
#'   \item Merge selected items with the item pool to attach item attributes.
#'   \item For each panel:
#'     \itemize{
#'       \item Construct \code{ItemsInModules}, listing items selected in each module.
#'       \item Construct \code{ItemsInPathways}, listing items associated with each
#'       pathway based on the pathway–module mapping.
#'     }
#' }
#'
#' If pathway or module labels are available in \code{x$PathwayIndex} and
#' \code{x$ModuleIndex}, labels for modules/pathways are attached.
#'
#'
#' @return An object of S3 class \code{mstATA_panel} with a named list with one element per panel:
#'        Panel_1, Panel_2, ....Each panel is a list with components:
#'  \describe{
#'     \item{ItemsInModules}{A data frame containing selected items grouped by
#'       module. Columns include \code{item_name}, \code{item_id},
#'       \code{varname}, \code{module_id}, and all item attributes from the item pool.}
#'     \item{ItemsInPathways}{A data frame containing selected items grouped by
#'       pathway. Columns include \code{item_name}, \code{item_id},
#'       \code{varname}, \code{pathway_id}, and all item attributes from the item pool.}
#'   }
#'
#' @seealso [solve_model()]
#'
#' @export

assembled_panel<-function(x, result){
  if (!inherits(x, "mstATA_design")) {
    stop("'x' must be an mstATA_design object.", call. = FALSE)
  }

  if (!is.list(result) ||is.null(result$solution$best_solution)) {
    stop("'result' must be a solver result returned by solve_model().",
         call. = FALSE)
  }

  vtype<-result$model$vtype
  varname<-result$model$varname
  sol<-result$solution$best_solution
  stopifnot(length(vtype) == length(varname),length(vtype) == length(sol))

  which_selected<-which(vtype == "B" & sol>0.5)
  if (!length(which_selected)) {
    stop("No selected items found in solver solution.", call. = FALSE)
  }
  selected_items<-data.frame(varname = varname[which_selected],value = sol[which_selected],
                             stringsAsFactors = FALSE)

  selected_items<-parse_x_indices(selected_items)
  itempool<-x$ItemPool
  item_id_col<-x$item_id_col
  id_map <- data.frame(item_id = 1:nrow(itempool),
                       item_name = itempool[[item_id_col]],
                       stringsAsFactors = FALSE)

  selected_items <- merge(selected_items,id_map,
                          by = "item_id",all.x = TRUE,sort = FALSE)
  selected_items<-merge(selected_items,itempool,by.x = "item_name",
                        by.y = item_id_col,
                        all.x = TRUE,sort = FALSE)
  ModuleIndex<-x$ModuleIndex
  PathwayIndex<-x$PathwayIndex
  NumStages<-x$NumStages
  NumPathways<-x$NumPathways
  modules_involved<-vapply(seq_len(NumPathways),FUN = function(pathway_id) {
    as.integer(as.vector(as.matrix(PathwayIndex[PathwayIndex$pathway_index==pathway_id,1:NumStages])))
  },FUN.VALUE = integer(NumStages))

  panels<-sort(unique(selected_items$panel_id))
  out<-lapply(panels,function(panel_id){
    panel_dat<-selected_items[selected_items$panel_id==panel_id,,drop=FALSE]
    module_col<-c("item_name","item_id","varname","module_id",
                  setdiff(names(panel_dat),c("item_name","item_id","varname","module_id","value","panel_id")))
    ItemsInModules<-panel_dat[,module_col,drop = FALSE]


    ItemsInPathways<-do.call(rbind,lapply(seq_len(NumPathways),function(pathway_id){
      mods<-as.vector(modules_involved[,pathway_id])
      pathway_out<-panel_dat[panel_dat$module_id%in%mods,,drop=FALSE]
      pathway_out$pathway_id<-pathway_id
      pathway_out
    }))
    pathway_col<-c("item_name", "item_id", "varname", "pathway_id",
                   setdiff(names(ItemsInPathways),
                           c("item_name","item_id","varname","value","panel_id","module_id","pathway_id")))
    ItemsInPathways <- ItemsInPathways[,pathway_col,drop = FALSE]

    if("pathway_label"%in%names(PathwayIndex)){
      map <- ModuleIndex[, c("module_index", "module_label")]
      map <- map[order(map$module_index), ]
      ItemsInModules$module_index<-factor(ItemsInModules$module_id,levels = map$module_index,
                                          labels = map$module_label)

      map2 <- PathwayIndex[, c("pathway_index", "pathway_label")]
      map2 <- map2[order(map2$pathway_index), ]
      ItemsInPathways$pathway_index<-factor(ItemsInPathways$pathway_id,levels = map2$pathway_index,
                                         labels = map2$pathway_label)
    }
    list(ItemsInModules = ItemsInModules,
         ItemsInPathways = ItemsInPathways)
  })
  names(out)<-paste0("Panel_",panels)
  class(out)<-"mstATA_panel"
  return(out)
}

