#' @title Create Pathways Based an MST Design
#'
#' @description
#' Generates all possible pathways implied by a multistage test (MST) design and
#' assigns module difficulty labels when available. If difficulty levels are not
#' supplied, default labels are applied for stages containing up to five modules.
#'
#' @param design A character string specifying the multistage test (MST) design.
#'   The string defines the number of modules in each stage and may use commas
#'   (\code{,}), dashes (\code{-}), or slashes (\code{/}) as separators. For
#'   example, \code{"1-3-3"}, \code{"1,3,3"}, and \code{"1/3/3"} all define an MST
#'   with 1 module in stage 1, 3 modules in stage 2, and 3 modules in stage 3.
#'
#' @param diff_levels An optional named list specifying difficulty labels for
#'   stages with a given number of modules. Names correspond to the number of
#'   modules in a stage, and values are character vectors of difficulty labels.
#'   If \code{NULL} (default), built-in labels are used for stages with up to five
#'   modules. See **Details**.
#'
#' @param exclude_pathways An optional character vector identifying disallowed
#'   pathways. Each element must be a string of the form \code{"x-y-z"}, where
#'   each number denotes the module selected at a given stage. Module indices
#'   must correspond to those implied by the MST design. If \code{NULL} (default),
#'   all pathways implied by the design are allowed. This option is commonly used
#'   to prohibit extreme routing transitions (e.g., from very easy to very hard
#'   modules).
#'
#' @details
#' **1. Difficulty labels**
#'
#' When \code{diff_levels = NULL}, default difficulty labels are assigned for
#' stages with up to five modules:
#'
#' \itemize{
#'   \item 1 module: \code{"M"}
#'   \item 2 modules: \code{"E"}, \code{"H"}
#'   \item 3 modules: \code{"E"}, \code{"M"}, \code{"H"}
#'   \item 4 modules: \code{"XE"}, \code{"E"}, \code{"H"}, \code{"XH"}
#'   \item 5 modules: \code{"XE"}, \code{"E"}, \code{"M"}, \code{"H"}, \code{"XH"}
#' }
#'
#' If any stage contains more than five modules and \code{diff_levels} is not
#' provided, a message is issued and pathway difficulty labels are omitted from
#' the output. In such cases, users are encouraged to supply a custom
#' \code{diff_levels} list to ensure meaningful interpretation of module
#' difficulty.
#'
#' **2. Excluding pathways**
#'
#' The \code{exclude_pathways} argument allows users to remove specific module
#' combinations from the set of possible pathways.
#'
#' For example, under a \code{"1-3-3"} MST design:
#'
#' \itemize{
#'   \item All possible pathways (1 × 3 × 3 = 9) are:
#'   \code{c("1-1-1", "1-1-2", "1-1-3", "1-2-1", "1-2-2", "1-2-3",
#'          "1-3-1", "1-3-2", "1-3-3")}
#'
#'   \item If extreme transitions are not permitted, one may specify:
#'   \code{exclude_pathways = c("1-1-3", "1-3-1")}
#'
#'   \item This excludes transitions such as easy-to-hard (\code{"1-1-3"}) and
#'   hard-to-easy (\code{"1-3-1"}).
#' }
#'
#' @return
#' A list with two data frames:
#'
#' \describe{
#'   \item{\code{Modules}}{A data frame containing module-level information with
#'     columns \code{stage}, \code{module}, and \code{module_index}.}
#'   \item{\code{Pathways}}{A data frame describing all constructed pathways.
#'     Includes one column per stage (e.g., \code{stage1}, \code{stage2}, \dots),
#'     a \code{pathway} identifier, and an \code{allowed} indicator. If difficulty
#'     labels are available, an additional column \code{pathway_label} is
#'     included.}
#' }
#'
#' @keywords internal


create_pathways <- function(design, diff_levels = NULL,exclude_pathways = NULL) {
  Design <- suppressWarnings(as.integer(unlist(strsplit(design, "[-, /]"))))
  if (anyNA(Design) || any(Design < 1)) {
    stop("`design` must be a string of positive integers, e.g., '1-3-3'.",
         call. = FALSE)
  }
  NumStages <- length(Design)

  # Create module dataframe
  module <- do.call(rbind, lapply(seq_len(NumStages), function(stage_id) {
    data.frame(stage = stage_id, module = seq_len(Design[stage_id]),
               stringsAsFactors = FALSE)
  }))
  module$module_index <- seq_len(nrow(module))

  # Create full factorial grid of pathways
  pathway <- expand.grid(split(module$module_index, module$stage),
                         KEEP.OUT.ATTRS = FALSE,stringsAsFactors = FALSE)
  colnames(pathway) <- paste0("stage", seq_len(NumStages))

  # Add stage-specific module index
  for (stage_id in seq_len(NumStages)) {
    stage_modules <- module[module$stage == stage_id, ]
    pathway[[paste0("stage", stage_id, "_index")]] <-stage_modules$module[match(pathway[[paste0("stage", stage_id)]],
                                                                                stage_modules$module_index)]
  }

  # Create numeric pathway ID (e.g., "1-2-4")
  pathway$pathway <- do.call(paste,c(pathway[paste0("stage", seq_len(NumStages), "_index")], sep = "-"))

  pathway$allowed<-TRUE
  if (!is.null(exclude_pathways)) {
    bad <- setdiff(exclude_pathways, pathway$pathway)
    if (length(bad) > 0) {
      stop("Invalid pathway(s) in exclude_pathways: ",
           paste(bad, collapse = ", "),
           call. = FALSE)
    }
    pathway$allowed[pathway$pathway %in% exclude_pathways] <- FALSE
  }


  # Add difficulty labels if applicable
  if (is.null(diff_levels)) {
    if (max(Design) > 5) {
      message("Default diff_levels only cover designs with le 5 modules per stage. ",
              "Returning pathways without labels. Provide `diff_levels` to add labels.")
      return(list(Modules = module, Pathways = pathway[, c(paste0("stage", seq_len(NumStages)), "pathway", "allowed")]))
    } else {
      diff_levels <- list(
        `1` = c("M"),
        `2` = c("E", "H"),
        `3` = c("E", "M", "H"),
        `4` = c("XE", "E", "H", "XH"),
        `5` = c("XE", "E", "M", "H", "XH")
      )
    }
  }

  for (k in unique(Design)) {
    lv <- diff_levels[[as.character(k)]]
    if (is.null(lv) || length(lv) != k) {
      stop("diff_levels must provide exactly ", k,
           " labels for stage with ", k, " modules.",
           call. = FALSE)
    }
  }


  for (stage_id in seq_len(NumStages)) {
    k <- Design[stage_id]
    labels <- diff_levels[[as.character(k)]]
    stage_mod <- module[module$stage == stage_id, ]
    module[module$stage == stage_id, "module_label"] <-paste0("S", stage_id, labels)

    pathway[[paste0("stage", stage_id, "_label")]] <-labels[match(pathway[[paste0("stage", stage_id)]],
                                                                  stage_mod$module_index)]
  }

  pathway$pathway_label <- do.call(paste,c(pathway[paste0("stage", seq_len(NumStages), "_label")], sep = "-"))

  pathway <- pathway[, c(paste0("stage", seq_len(NumStages)),
                         "pathway", "allowed", "pathway_label")]

  return(list(Modules = module, Pathways = pathway))
}

