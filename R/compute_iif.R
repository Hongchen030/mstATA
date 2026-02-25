#' @title Compute Item Information Function (IIF) at Target Theta Points
#'
#' @description
#'
#' Computes item information function (IIF) values for dichotomous,
#' polytomous, or mixed-format item pools across a set of target ability
#' values. The function supports multiple unidimensional IRT models simultaneously and
#' returns an item-by-theta information matrix.
#'
#' @param items A data frame containing item metadata.
#'   Each row represents an item. The data frame must include a column
#'   specifying the item model (see \code{model_col}) and the parameter
#'   columns referenced in \code{item_par_cols}.
#' @param item_par_cols A named list defining the IRT parameter columns
#'   required for each supported model. The list names must exactly match
#'   the model identifiers specified in \code{items[[model_col]]}:
#'   \code{"1PL"}, \code{"RASCH"}, \code{"2PL"}, \code{"3PL"},
#'   \code{"4PL"}, \code{"GRM"}, \code{"MGRM"}, \code{"PCM"},
#'   \code{"GPCM"}, \code{"RSM"}, and \code{"NRM"}.
#'   Each list element is a character vector giving the column names
#'   in \code{items} that contain the required item parameters for
#'   the corresponding model.
#' @param theta A numeric vector of ability values at which the
#'   IIFs are evaluated.
#' @param model_col A character string specifying the column name in
#'   \code{items} that indicates the IRT model used for each item.
#'   Values in this column must correspond to one of the supported
#'   model names:
#'   \code{"1PL"}, \code{"RASCH"}, \code{"2PL"}, \code{"3PL"},
#'   \code{"4PL"}, \code{"GRM"}, \code{"MGRM"}, \code{"PCM"},
#'   \code{"GPCM"}, \code{"RSM"}, or \code{"NRM"}.
#' @param D Scaling constant used in the IRT model. Default is D=1 (for logistic metric);
#'  D=1.702 yields approximately the normal metric.
#'
#' @return
#' A numeric matrix of item information values, with rows corresponding to items
#' and columns corresponding to `theta`.
#'
#' @seealso [compute_icc()]
#'
#' @examples
#' ## Example 1: Dichotomous model
#' data("mini_itempool")
#' compute_iif(mini_itempool,item_par_cols = list("3PL"=c("discrimination","difficulty","guessing")),
#'             theta = c(-1,0,1),model_col = "model")
#'
#' ## Example 2: Polytomous model (PCM)
#' data("poly_itempool")
#' compute_iif(mini_itempool,item_par_cols = list("3PL"=c("discrimination","difficulty","guessing")),
#'             theta = c(-1,0,1),model_col = "model")
#'
#' ## Example 3: multiple IRT models (3PL + GPCM + GRM)
#' data("Rmst_pool")
#' compute_iif(Rmst_pool,item_par_cols = list("3PL"=c("a","b","c"),
#'                                            "GPCM" = c("alpha","delta1","delta2","delta3"),
#'                                             "GRM" = c("alpha","beta1","beta2")),
#'             theta = c(-1,0,1),model_col = "model")
#' @export
#'
compute_iif<-function(items,item_par_cols,
                      theta,
                      model_col,D = 1){
  if (!is.data.frame(items) || nrow(items) == 0L) {
    stop("`items` must be a non-empty data frame.", call. = FALSE)
  }
  if (!is.numeric(theta) || length(theta) < 1) {
    stop("`theta` must be a non-empty numeric vector.", call. = FALSE)
  }

  if (!model_col %in% names(items)) {
    stop("`model_col` must be a column in `items`.", call. = FALSE)
  }
  if (!is.list(item_par_cols) || is.null(names(item_par_cols))) {
    stop("`item_par_cols` must be a named list indexed by model.", call. = FALSE)
  }
  names(item_par_cols) <- toupper(names(item_par_cols))

  n_items <- nrow(items)
  if (is.null(rownames(items))) {
    item_ids <- paste0("Item", seq_len(n_items))
  }else{
    item_ids<-rownames(items)
  }

  ## ---- per-item parameter extraction ----
  item_models <- toupper(items[[model_col]])
  dichotomous_models <- c("1PL", "RASCH", "2PL", "3PL", "4PL")
  ## ---- extract item parameters ----
  item_params <- lapply(seq_len(n_items), function(i) {
    model <- item_models[i]
    if (!model %in% names(item_par_cols)) {
      stop("Unsupported model '", model, "' at item ", i, ".", call. = FALSE)
    }
    par_cols <- item_par_cols[[model]]
    if (!all(par_cols %in% names(items))) {
      stop("Missing parameter columns for model '", model,
           "' at item ", i, ".", call. = FALSE)
    }
    params <- items[i, par_cols]
    if(model %in%dichotomous_models){
      params<-fill_dichotomous_defaults(params)
    }
    list(model = model, params = params)
  })

  item_groups <- split(seq_len(n_items), item_models)
  ## ---- compute IIF matrix ----
  iif_mat <- matrix(NA_real_,nrow = n_items,ncol = length(theta),
                    dimnames = list(item_ids, paste0("theta=", theta)))

  for (t in seq_along(theta)) {
    th <- theta[t]
    for(g in names(item_groups)){
      idx <- item_groups[[g]]
      model <- item_params[[idx[1]]]$model
      ## stack parameters
      par_mat <- do.call(rbind,
                         lapply(idx, function(i) item_params[[i]]$params))
      if(model%in%dichotomous_models){
        iif_mat[idx,t] <- Ii_internal(theta = th,itempar_mat = par_mat,model = NULL,D = D)
      }else{
        iif_mat[idx,t] <- Ii_internal(theta = th,itempar_mat = par_mat,model = model,D = D)
      }
    }
  }
  if (any(!is.finite(iif_mat))) {
    warning("Non-finite IIF values detected.")
  }
  return(iif_mat)
}
