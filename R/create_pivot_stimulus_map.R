#' @title Create Pivot–Stimulus Mapping
#'
#' @description
#'
#' Construct a stimulus–pivot item map from an item pool containing
#' stimulus identifiers and pivot-item indicators. This function extracts
#' the pivot item for each stimulus and identifies all items associated
#' with that stimulus.
#'
#' This function is typically used as a **preprocessing** step before applying
#' any constraints related with stimuli and stimulus-based items.
#'
#' @param itempool A \code{data.frame} containing the item pool.
#' @param item_id_col A string giving the column name in \code{itempool}
#'   that uniquely identifies items.
#' @param stimulus A string giving the column name in \code{itempool}
#'   that identifies the stimulus or passage to which each item belongs.
#'   Values may be character, or numeric (coerced to character).
#'   Items not associated with a stimulus should be coded as \code{NA}.
#' @param pivot_item A string giving the column name in \code{itempool}.
#'   Values must be character and non-missing values identify pivot items.
#'   Each stimulus must have exactly one
#'   pivot item. Empty strings or whitespace are treated as missing. See Details.
#'
#' @details
#' Formally, a pivot item is defined as an item that is selected
#' if and only if its corresponding stimulus is selected. In
#' practice, the pivot item is typically chosen as the item within a set
#' that best represents the stimulus—identified by content experts as having
#' the most representative content or desirable psychometric properties.
#'
#' @return A list with the following components:
#' \describe{
#'
#'   \item{\code{pivot_item_id}}{A character vector of pivot item IDs, one per
#'     stimulus.}
#'
#'   \item{\code{stimulus_name}}{A character vector giving the stimulus
#'   identifier associated with each pivot item. The order matches
#'   \code{pivot_item_id}.}
#'
#'   \item{\code{stimulus_members}}{A named list. Each element contains the item
#'     IDs belonging to a stimulus. Names match \code{stimulus_name}.}
#'
#'   \item{\code{numItems_stimulus}}{An integer vector giving the number of
#'   items associated with each stimulus. This is \code{length(stimulus_members[[i]])}.}
#' }
#'
#' @examples
#' itempool <- data.frame(
#'   item_id = 1:7,
#'   stimulus_id = c("S1","S1","S1","S2","S2", NA, NA),
#'   is_pivot   = c("P", NA, NA, "P", NA, NA, NA)
#' )
#'
#' create_pivot_stimulus_map(
#'   itempool = itempool,
#'   item_id_col = "item_id",
#'   stimulus = "stimulus_id",
#'   pivot_item = "is_pivot"
#' )
#'
#'
#' @export

create_pivot_stimulus_map<-function(itempool,item_id_col = "item_id",stimulus,pivot_item){
  item_ids<-as.character(check_attribute_column(itempool = itempool,attribute = item_id_col))
  stimulus_vec <-as.character(check_attribute_column(itempool = itempool,attribute = stimulus))
  pivot_vec <- as.character(check_attribute_column(itempool = itempool,attribute =  pivot_item))

  pivot_vec[trimws(pivot_vec) == ""] <- NA
  if (anyDuplicated(item_ids)) {
    stop("Item identifiers must be unique.", call. = FALSE)
  }

  if(all(is.na(stimulus_vec))){
    stop(paste0("No stimulus information found in column: ",stimulus))
  }

  ## treat any non-NA as pivot flag
  pivot_flag <- !is.na(pivot_vec)
  if (!any(pivot_flag)) {
    stop("No pivot items found in column: ", pivot_item, call. = FALSE)
  }
  if (any(is.na(stimulus_vec[pivot_flag]))) {
    stop("Each pivot item should be associated with a stimulus.", call. = FALSE)
  }

  ## exactly one pivot per stimulus
  pivot_count <- tapply(pivot_flag, stimulus_vec, sum, na.rm = TRUE)
  bad <- pivot_count[pivot_count != 1]
  if (length(bad) > 0) {
    stop(
      sprintf(
        "Invalid: Stimulus '%s' has %d pivot items (must have exactly one).",
        names(bad)[1], bad[1]
      ),
      call. = FALSE
    )
  }

  ## stimulus members
  stimulus_members_all <- split(item_ids, stimulus_vec)


  pivot_idx <- which(pivot_flag)
  stimulus_name <- stimulus_vec[pivot_idx]
  pivot_item_ids <- item_ids[pivot_idx]
  ord <- order(stimulus_name)

  return(list(pivot_item_id = pivot_item_ids[ord],
              stimulus_name = stimulus_name[ord],
              stimulus_members  = stimulus_members_all[stimulus_name][ord],
              numItems_stimulus = lengths(stimulus_members_all[stimulus_name])[ord]))
}
