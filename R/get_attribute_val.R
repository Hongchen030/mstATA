#' @title Get Item Categorical/Quantitative Attribute Value
#'
#' @description
#'
#' Create a vector representing the attribute value.
#'
#' @param itempool A \code{data.frame} containing the item pool.
#' @param attribute A string giving the column name in \code{itempool} that represents
#' the categorical/quantitative attribute.
#' @param cat_level A optional character string for one category for the categorical attribute.
#' Default is NULL, indicating the attribute is a quantitative attribute.
#'
#' @return A vector of item attribute values. The same length as the item pool.
#' - If the attribute is a quantitative attribute, return a numeric vector. NA is not allowed in the quantitative attribute.
#' - If the attribute is a categorical attribute, return a binary integer vector with 1 indicating an item having the
#' categorical attribute, 0 indicating an item not having the categorical attribute. If an item has NA in the categorical attribute,
#' it will be coded as 0, indicating this item not having the categorical attribute.
#'
#' @examples
#' # Example 1, item categorical attribute
#' get_attribute_val(itempool = mini_itempool,attribute = "itemtype",cat_level = "MC")
#' # Example 2, item quantitative attribute
#' get_attribute_val(itempool = mini_itempool,attribute = "difficulty",cat_level = NULL)
#' # Example 3: stimulus categorical attribute
#' get_attribute_val(itempool = reading_itempool[!is.na(reading_itempool$pivot_item),],
#'                   attribute = "stimulus_type",cat_level = "history")
#' # Example 4: stimulus quantitative attribute
#' get_attribute_val(itempool = reading_itempool[!is.na(reading_itempool$pivot_item),],
#'                   attribute = "stimulus_words")
#' @export

get_attribute_val<-function(itempool,attribute,cat_level = NULL){
  item_vals <- check_attribute_column(itempool, attribute)
  attribute_type <- attr(item_vals, "attribute_type")
  if (is.null(attribute_type)) {
    stop("Attribute type information missing for column: ", attribute,
         call. = FALSE)
  }
  if (is.null(cat_level)) {
    if (attribute_type == "quantitative") {
      return(as.numeric(item_vals))               # no need for NA check here
    }

    out <- suppressWarnings(as.numeric(as.character(item_vals)))
    bad <- is.na(out) & !is.na(item_vals)
    if (any(bad)) {
      stop(
        sprintf(
          "Column '%s' is categorical and cannot be safely coerced to numeric; ",
          attribute
        ),
        "supply 'cat_level' to obtain indicator coefficients.",
        call. = FALSE
      )
    }
    return(out)
  }

  ## ---- categorical indicator requested ----
  if (attribute_type != "categorical") {
    stop(
      "'cat_level' can only be used with categorical attributes.",
      call. = FALSE
    )
  }

  if (length(cat_level) != 1L || is.na(cat_level)) {
    stop("'cat_level' must be a single non-NA value.",call. = FALSE)
  }


  idx <- item_vals == cat_level

  if(anyNA(idx)){
    message(paste0("Item ",paste(which(is.na(idx)),collapse = ",")," attribute value is NA."))
    idx[is.na(idx)]<-FALSE
  }
  if (!any(idx, na.rm = TRUE)) {
    stop(sprintf("'cat_level' (%s) is not present in attribute '%s'.",as.character(cat_level), attribute))
  }

  return(as.integer(idx))
}
