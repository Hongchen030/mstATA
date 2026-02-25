#' @title Validate the module min/max length when top-down strategy is used.
#'
#' @description
#' This constraint must be included when top-down strategy is used to assemble MST panels.
#' @param x An object of class `"mstATA_design"` created by `mst_design()`.
#' @param stage_length_bound A optimal dataframe with three columns, 'stage','min','max', indicating the min/max number of
#' items in each stage.
#' @noRd
#' @keywords internal
validate_stage_length_bounds <- function(x,stage_length_bound = NULL) {
  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.")
  }
  NumStages<-x$NumStages
  if (is.null(stage_length_bound)){
    stage_length_bound<-data.frame(stage=1:NumStages,
                                   min=rep(1L,NumStages),
                                   max=NA_integer_)
  }
  colnames(stage_length_bound) <- tolower(colnames(stage_length_bound))
  if(ncol(stage_length_bound)!=3L){
    stop("'stage_length_bound' must be a data.frame with 3 columns: stage,min,max.")
  }

  if (!all(c("stage","min","max") %in% colnames(stage_length_bound))){
    stop("Columns must be named 'stage', 'min', 'max'.")
  }

  if (!all(stage_length_bound$stage %in% seq_len(NumStages))) {
    stop("Stage indices in 'stage_length_bound' must be between 1 and NumStages.")
  }

  if (any(duplicated(stage_length_bound$stage))) {
    stop("Each stage must appear only once in 'stage_length_bound'.")
  }

  minv <- stage_length_bound[, "min"]
  maxv <- stage_length_bound[, "max"]
  if (any(!is.na(minv) & minv < 0L)) {
    stop("'min' must be non-negative for all stages.")
  }
  if (any(!is.na(maxv) & maxv < 0L)) {
    stop("'max' must be non-negative for all stages.")
  }

  which_notNA<-which(!is.na(minv) & !is.na(maxv))
  if(any(minv[which_notNA]>maxv[which_notNA])){
    stop("'min' cannot exceed 'max' for a stage.")
  }

  ModuleIndex<-x$ModuleIndex
  ModuleIndex<-dplyr::left_join(ModuleIndex,stage_length_bound,by = "stage")
  ModuleIndex$min[is.na(ModuleIndex$min)]<-1L
  return(ModuleIndex)
}
