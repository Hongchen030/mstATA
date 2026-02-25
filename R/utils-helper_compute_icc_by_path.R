#' Build pathway-level ICCs from module ICCs
#'
#' @description
#' Combines module-level ICCs into pathway-level ICCs by row-binding
#' item ICC matrices across modules for each theta point.
#'
#' @param icc_by_mod Named list of module ICC objects (output of compute_icc()).
#'   Each element must itself be a named list over theta.
#'
#' @param pathway_modules Integer or character vector of module IDs defining a pathway.
#'
#' @return A named list of ICC matrices, one per theta value.
#'
#' @keywords internal
compute_icc_by_path <- function(icc_by_mod, pathway_modules) {

  if (!is.list(icc_by_mod) || length(icc_by_mod) == 0) {
    stop("`icc_by_mod` must be a non-empty list.", call. = FALSE)
  }

  if (is.null(names(icc_by_mod))) {
    stop("`icc_by_mod` must be a named list with module IDs as names.",
         call. = FALSE)
  }

  if (!is.numeric(pathway_modules) && !is.character(pathway_modules)) {
    stop("`pathway_modules` must be a numeric or character vector of module IDs.",
         call. = FALSE)
  }

  pathway_modules <- as.character(pathway_modules)

  if (!all(pathway_modules %in% names(icc_by_mod))) {
    missing_mods <- setdiff(pathway_modules, names(icc_by_mod))
    stop("Modules not found in `icc_by_mod`: ",
         paste(missing_mods, collapse = ", "),
         call. = FALSE)
  }

  ## extract ICC lists for the modules in this pathway
  icc_lists <- icc_by_mod[pathway_modules]

  ## check theta grid consistency
  theta_names <- names(icc_lists[[1]])
  if (is.null(theta_names)) {
    stop("Each module ICC must be a named list over theta.", call. = FALSE)
  }

  for (m in seq_along(icc_lists)) {
    if (!setequal(theta_names, names(icc_lists[[m]]))) {
      stop("Theta grid mismatch across modules in `icc_by_mod`.",
           call. = FALSE)
    }
  }

  ## ensure consistent theta order
  icc_lists <- lapply(icc_lists, function(x) x[theta_names])

  ## row-bind ICCs at each theta
  icc_path <- lapply(theta_names, function(th) {
    mats <- lapply(icc_lists, function(mod_icc) mod_icc[[th]])

    if (!all(vapply(mats, is.matrix, logical(1)))) {
      stop("All ICC elements must be matrices.", call. = FALSE)
    }

    ## check category consistency
    ncat <- vapply(mats, ncol, integer(1))
    if (length(unique(ncat)) != 1L) {
      max_col <- max(ncat)
      mats <- lapply(mats, function(m) {
        if (ncol(m) < max_col) {
          cbind(m, matrix(0, nrow = nrow(m),
                          ncol = max_col - ncol(m)))
        } else {
          m
        }
      })
    }

    do.call(rbind, mats)
  })

  names(icc_path) <- theta_names
  icc_path
}
