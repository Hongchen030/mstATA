#' @keywords internal
#' @noRd
#'
check_item_ids<-function(item_ids,item_names){

  if(is.null(item_ids))return(NULL)

  PoolSize<-length(item_names)

  if (is.character(item_ids)) {
    item_index <- match(item_ids,item_names)
    if (anyNA(item_index)) {
      stop("Some item_ids not found in itempool.")
    }
    return(item_index)
  }

  if (is.numeric(item_ids)) {
    if (any(item_ids < 1 | item_ids > PoolSize)) {
      stop("'item_ids' must be valid indices within the item pool.")
    }
    return(as.integer(item_ids))
  }

  stop("'item_ids' must be either numeric indices or character names.")
}

