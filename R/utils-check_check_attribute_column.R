#' @title Check the item-level categorical and quantitative attribute in the itempool
#'
#' @description
#'
#' Confirm that the specified column exists in the item pool.
#'
#' @param itempool A \code{data.frame} containing the item pool. Must include metadata
#' for each item (e.g., item ID, difficulty, content, etc.).
#' @param attribute A string giving the column name for the item attribute in \code{itempool}.
#'
#' @return A vector of item attribute values with the same length as the item pool.
#'
#' - **Quantitative attribute:** Returns a numeric vector.
#'   Missing values (`NA`) are not allowed; a warning message will be issued if they are detected.
#'
#' - **Categorical attribute:** Returns a factor vector indicating the category to which each item belongs.
#'   Missing values (`NA`) are allowed and indicate that the corresponding item does not belong to any category for this attribute.
#'
#' @keywords internal
#'
#'
check_attribute_column <- function(itempool, attribute) {
  if (!is.data.frame(itempool)) {
    stop("`itempool` must be a data frame.")
  }

  if (!attribute %in% colnames(itempool)) {
    stop(paste0("Attribute '", attribute, "' not found in the item pool."))
  }
  item_vals<-itempool[[attribute]]
  missing<-is.na(item_vals)

  if(is.numeric(item_vals)){
    if(!all(is.finite(item_vals))){
      stop(paste0("Quantitative attribute: '", attribute, "' must not contain NA, NaN, or infinite values."),
           call. = FALSE)
    }

    if(length(unique(item_vals))<=10){
      message(paste0("Note: attribute '", attribute,
                     "' has le 10 unique numeric values. ",
                     "Is it intended to be categorical?"))
    }

    attr(item_vals, "attribute_type") <- "quantitative"
    return(item_vals)
  }

  if (is.character(item_vals) || is.factor(item_vals)) {
    nonmissing_vals <- item_vals[!missing]
    levels <- sort(unique(nonmissing_vals))
    itemcat_vals<-factor(item_vals,levels = levels,labels = levels)
    attr(itemcat_vals, "attribute_type") <- "categorical"
    return(itemcat_vals)
  }

  stop(paste0(
    "Attribute '", attribute, "' has unsupported type: ",
    class(item_vals), ". Only numeric, character, and factor are allowed."
  ))
}


