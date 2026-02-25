#' @title Compute Item Characteristic Curves (ICC)
#'
#' @description
#'
#' Computes item category characteristic curves (ICCs) for dichotomous,
#' polytomous, or mixed-format item pools. The function supports multiple IRT models simultaneously and
#' returns category-level response probabilities for each item at specified ability points.
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
#'   ICCs are evaluated.
#' @param model_col A character string specifying the column name in
#'   \code{items} that indicates the IRT model used for each item.
#'   Values in this column must correspond to one of the supported
#'   model names:
#'   \code{"1PL"}, \code{"RASCH"}, \code{"2PL"}, \code{"3PL"},
#'   \code{"4PL"}, \code{"GRM"}, \code{"MGRM"}, \code{"PCM"},
#'   \code{"GPCM"}, \code{"RSM"}, or \code{"NRM"}.
#' @param nrCat_col A character string specifying the column name in
#'   \code{items} that indicates the number of response categories for
#'   each items. Default is NULL, the number of response categories is derived
#'   based on item parameters. If not NULL, the number of response categories
#'   is checked with number of item parameters for consistency.
#' @param D Scaling constant used in the IRT model. Default is D=1 (for logistic metric);
#'  D=1.702 yields approximately the normal metric.
#'
#' @details
#' The output matrices returned by \code{compute_icc()} include
#' a fixed set of category columns (\code{cat0}, \code{cat1}, \dots),
#' with unused categories filled with zeros to ensure consistent dimensions
#' across mixed-format item pools.
#'
#'
#' (1) \strong{Dichotomous models.}
#'
#' For the models \code{"1PL"}, \code{"RASCH"}, \code{"2PL"}, \code{"3PL"},
#' and \code{"4PL"}, item parameters are extended to the following four
#' columns:
#' \describe{
#'   \item{\code{discrimination}}{Slope parameter (\eqn{a}).}
#'   \item{\code{difficulty}}{Location parameter (\eqn{b}).}
#'   \item{\code{guessing}}{Lower asymptote (\eqn{c}).}
#'   \item{\code{upper}}{Upper asymptote (\eqn{d}).}
#' }
#'
#' Missing parameters required for a given model are automatically filled. For example, the RASCH and 1PL
#' models are represented with \eqn{a = 1}, \eqn{c = 0}, and \eqn{d = 1}.
#'
#' All dichotomous item parameters are validated prior to ICC computation:
#' \itemize{
#'   \item \code{discrimination > 0}
#'   \item \code{0 <= guessing <= 1}
#'   \item \code{0 <= upper <= 1}
#'   \item \code{guessing < upper}
#' }
#'
#' (2) \strong{Polytomous models.}
#'
#' The supported polytomous models include \code{"GRM"}, \code{"MGRM"},
#' \code{"PCM"}, \code{"GPCM"}, \code{"RSM"}, and \code{"NRM"}. Let
#' \eqn{K} denotes the number of non-baseline response
#' categories.
#' Required parameters depend on the specific model and may include
#' discrimination parameters and category-specific thresholds or step
#' parameters (e.g., \code{betaj1}, \dots, \code{betajK},
#' \code{deltaj1}, \dots, \code{deltajK}). See \code{Pi_internal()}.
#'
#' @return A named list of matrices, one for each value in
#'   \code{theta}. List length =  the number of ability values
#'   and List names = Character strings of the form \code{"theta=<value>"}.
#'
#'  Each matrix entry gives the probability of responding in the
#'   corresponding category for the given item at the specified
#'   ability level. Matrix rows = the number of items, with row names equal to item identifiers.
#'   Matrix columns = Response categories labeled
#'       \code{cat0}, \code{cat1}, \dots, \code{catM}.
#'
#' @examples
#' ## Example 1: Dichotomous model
#' data("mini_itempool")
#' compute_icc(mini_itempool,list("3PL"=c("discrimination","difficulty","guessing")),
#'             theta = c(-1,0,1),model_col = "model",D = 1.7)
#'
#' ## Example 2: Polytomous model (PCM)
#' data("poly_itempool")
#' compute_icc(poly_itempool,list("PCM"=c("deltaj1","deltaj2","deltaj3")),
#'             theta = c(-1,0,1),model_col = "model")
#'
#' ## Example 3: multiple IRT models (3PL + GPCM + GRM)
#' data("Rmst_pool")
#' compute_icc(Rmst_pool,item_par_cols = list("3PL"=c("a","b","c"),
#'                                            "GPCM" = c("alpha","delta1","delta2","delta3"),
#'                                             "GRM" = c("alpha","beta1","beta2")),
#'             theta = c(-1,0,1),model_col = "model")
#' @export

compute_icc<-function(items,item_par_cols,
                      theta,model_col,
                      nrCat_col = NULL,D=1){
  if (!is.data.frame(items) || nrow(items) == 0L) {
    stop("`items` must be a non-empty data frame.", call. = FALSE)
  }
  if (!is.numeric(theta) || anyNA(theta) || any(!is.finite(theta))) {
    stop("`theta` must be a numeric vector with finite values.", call. = FALSE)
  }
  theta <- sort(unique(theta))
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

  dichotomous_models <- c("1PL", "RASCH", "2PL", "3PL", "4PL")
  ## ---- per-item parameter extraction ----
  item_models <- toupper(items[[model_col]])
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

  ## ---- determine number of categories per item ----
  if (is.null(nrCat_col)) {
    ncat_by_item <- vapply(seq_len(n_items),
      FUN = function(i) {
        expected_nrCat(model  = item_params[[i]]$model,params = item_params[[i]]$params)
      },
      FUN.VALUE = integer(1))
  } else {
    if (!nrCat_col %in% names(items)) {
      stop("`nrCat_col` must be a column in `items`.", call. = FALSE)
    }
    ncat_by_item <- items[[nrCat_col]]
    if (!is.numeric(ncat_by_item) || anyNA(ncat_by_item)) {
      stop("`nrCat_col` must be a non-missing numeric vector.", call. = FALSE)
    }
    if (any(ncat_by_item < 2 | ncat_by_item %% 1 != 0)) {
      stop("`nrCat_col` must contain integers >= 2.", call. = FALSE)
    }
    for (i in seq_len(n_items)) {
      check_nrCat_consistency(model  = item_params[[i]]$model,
                              params = item_params[[i]]$params,
                              nrCat  = ncat_by_item[i])
    }
  }
  max_cat <- max(ncat_by_item)
  ## -------------------- group items by (model, ncat) --------------------
  group_key <- paste(item_models, ncat_by_item, sep = "_")
  item_groups <- split(seq_len(n_items), group_key)

  ## -------------------- allocate output --------------------
  icc_list <- vector("list", length(theta))
  names(icc_list) <- paste0("theta=", theta)

  for(t in seq_along(theta)){
    th <- theta[t]

    mat <- matrix(0,nrow = n_items,ncol = max_cat,
                  dimnames = list(item_ids,paste0("cat", 0:(max_cat - 1))))
    for(g in item_groups){
      idx <- g
      model <- item_params[[idx[1]]]$model
      k     <- ncat_by_item[idx[1]]

      ## stack parameters
      par_mat <- do.call(rbind,
                         lapply(idx, function(i) item_params[[i]]$params))

      ## compute probabilities
      if (model %in% dichotomous_models) {
        p1 <- Pi_internal(theta = th,itempar_mat = par_mat,D = D)$Pi
        probs<-cbind(1-p1,p1)
        mat[idx,paste0("cat",c(0,1))] <- probs
      } else {
        probs <- Pi_internal(theta = th,itempar_mat = par_mat,model = model,D = D)$Pi
        mat[idx,paste0("cat",0:(k-1))]<-probs[,paste0("cat",0:(k-1))]
      }

      ## safety check
      n_non_na <- rowSums(!is.na(probs))
      if (!all(n_non_na == k)) {
        stop(
          "Internal error: Pi_internal returned unexpected number of categories ",
          "for model ", model,
          " (items: ", paste(idx[n_non_na != k], collapse = ", "), ").",
          call. = FALSE
        )
      }
    }

    icc_list[[t]] <- mat
  }
  return(icc_list)
}

