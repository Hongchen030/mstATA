#' @title Estimate objective function bounds
#' @description
#'
#' Estimate feasible bounds for a^T x given module/pathway length caps etc.
#' @param x An object of class `"mstATA_design"` created by `mst_design()`.
#' @param a_vec An object created by `objective_term()`, the length is equal to the number of decision variables for one panel.
#' @return c(lower_bound,upper_bound)
#' @keywords internal
#' @noRd
#'
estimate_term_bounds <- function(x, a_vec) {
  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.", call. = FALSE)
  }

  dv_names <- x$decisionvar_name
  if (length(dv_names) != length(a_vec)) {
    stop("Length of 'a_vec' must match the number of decision variables.", call. = FALSE)
  }
  ## ---- parse decision variables x[module_id, item_id] ----
  m <- regexec("^x\\[(\\d+),(\\d+)\\]$", dv_names)
  parsed <- regmatches(dv_names, m)

  keep <- lengths(parsed) == 3
  if (!any(keep)) {
    stop("No item-selection decision variables found.", call. = FALSE)
  }

  parsed <- do.call(rbind, lapply(parsed[keep], function(z) as.integer(z[-1])))
  colnames(parsed) <- c("module_id", "item_id")

  dv_df <- data.frame(
    module_id = parsed[, 1],
    item_id   = parsed[, 2],
    a         = a_vec[keep],
    stringsAsFactors = FALSE
  )
  stopifnot(nrow(dv_df) == sum(keep))

  NumStages<-x$NumStages
  NumModules<-x$NumModules
  NumPathways<-x$NumPathways
  NumPanels<-x$NumPanels
  PoolSize<-nrow(x$ItemPool)
  ModuleIndex <- x$ModuleIndex
  PathwayIndex <- x$PathwayIndex
  if (!is.null(ModuleIndex$ModuleLength)) {
    LB<-numeric(length = NumModules)
    UB<-numeric(length = NumModules)
    for(module_id in 1:NumModules){
      which_col<-which(dv_df$module_id==module_id)
      sub_a<-dv_df$a[which_col]
      if (length(sub_a) == 0L) {
        LB[module_id] <- 0
        UB[module_id] <- 0
        next
      }
      k <- ModuleIndex$ModuleLength[module_id]
      k <- min(k, length(sub_a))
      ord <- sort(sub_a)
      LB[module_id] <- sum(ord[seq_len(k)])
      UB[module_id] <- sum(rev(ord)[seq_len(k)])
    }

    lower_bound<-sum(LB)
    upper_bound<-sum(UB)
  }else {
    LB<-numeric(length = NumPathways)
    UB<-numeric(length = NumPathways)
    pathway_length<-PathwayIndex$PathwayLength[1]
    for(pathway_id in 1:NumPathways){
      module_involved<-as.vector(as.matrix(PathwayIndex[PathwayIndex$pathway_index == pathway_id, 1:NumStages]))
      which_col<-which(dv_df$module_id %in% module_involved)
      sub_a <- dv_df$a[which_col]
      if (length(sub_a) == 0L) {
        LB[pathway_id] <- 0
        UB[pathway_id] <- 0
        next
      }
      k <- min(pathway_length, length(sub_a))
      ord <- sort(sub_a)
      LB[pathway_id] <- sum(ord[seq_len(k)])
      UB[pathway_id] <- sum(rev(ord)[seq_len(k)])
    }
    lower_bound<-min(LB)
    upper_bound<-sum(UB)
  }

  return(c(lower_bound=lower_bound,upper_bound=upper_bound))
}


