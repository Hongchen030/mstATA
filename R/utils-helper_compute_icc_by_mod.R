#' @keywords internal
#' @noRd
compute_icc_by_mod<-function(items_in_modules,
                              item_par_cols,
                              model_col,
                              theta = seq(-5, 5, 0.1),
                              D = 1.7){

  ## ---- validation ----
  if (!is.data.frame(items_in_modules)) {
    stop("`items_in_modules` must be a data.frame.", call. = FALSE)
  }

  if (!"module_id" %in% names(items_in_modules)) {
    stop("`items_in_modules` must contain a 'module_id' column.", call. = FALSE)
  }

  if (!is.numeric(theta) || anyNA(theta)) {
    stop("`theta` must be a numeric vector with no NA.", call. = FALSE)
  }
  theta <- sort(unique(theta))

  ## ---- split items by module (stable order) ----
  mods <- sort(unique(items_in_modules$module_id))
  items_by_module <- split(items_in_modules, items_in_modules$module_id)
  items_by_module <- items_by_module[as.character(mods)]
  if (any(vapply(items_by_module, nrow, integer(1)) == 0)) {
    stop("Each module must contain at least one item.", call. = FALSE)
  }

  ## ---- compute ICCs ----
  icc_by_mod <- lapply(items_by_module, function(df) {
    compute_icc(
      items = df,
      item_par_cols = item_par_cols,
      theta = theta,
      model_col = model_col,
      nrCat_col = NULL,
      D = D
    )
  })

  ## ---- structural sanity check (first module) ----
  if (!is.list(icc_by_mod[[1]]) ||
      !all(vapply(icc_by_mod[[1]], is.matrix, logical(1)))) {
    stop("`compute_icc()` must return a list of matrices per theta.",
         call. = FALSE)
  }

  icc_by_mod
}
