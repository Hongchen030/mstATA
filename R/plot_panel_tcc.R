#' @title Plot Test Category Probability Curves
#'
#' @description
#'
#' Plots category probability curves aggregated at the module or pathway
#' level based on the output of \code{report_test_tcc()}.
#'
#' The function supports both single-panel and multi-panel visualization
#' and automatically adjusts color and faceting to facilitate meaningful
#' comparisons across modules, pathways, and panels.
#'
#' @param assembled_panel
#' A \code{"mstATA_panel"} object returned by
#' \code{\link{assembled_panel}}. All panels must share identical module and
#' pathway structure.
#' @param theta A numeric vector of ability values. Default is \code{seq(-5,5,0,1)}.
#' @param item_par_cols A named list defining the IRT parameter columns
#'   required for each supported model. The list names must exactly match
#'   the model identifiers specified in \code{items[[model_col]]}:
#'   \code{"1PL"}, \code{"RASCH"}, \code{"2PL"}, \code{"3PL"},
#'   \code{"4PL"}, \code{"GRM"}, \code{"MGRM"}, \code{"PCM"},
#'   \code{"GPCM"}, \code{"RSM"}, and \code{"NRM"}.
#'   Each list element is a character vector giving the column names
#'   that contain the required item parameters for
#'   the corresponding model.
#' @param model_col A character string specifying the column name in
#'   either \code{ItemsInModules} or \code{ItemsInPathways} data frames
#'   that indicates the IRT model used for each item.
#'   Values in this column must correspond to one of the supported
#'   model names:
#'   \code{"1PL"}, \code{"RASCH"}, \code{"2PL"}, \code{"3PL"},
#'   \code{"4PL"}, \code{"GRM"}, \code{"MGRM"}, \code{"PCM"},
#'   \code{"GPCM"}, \code{"RSM"}, or \code{"NRM"}.
#' @param D Scaling constant used in the IRT model. Default is D=1 (for logistic metric);
#'  D=1.702 yields approximately the normal metric.
#' @param unit The aggregation unit to display. Must be either
#'   \code{"module"} or \code{"pathway"}.
#' @param mode Plotting mode for multi-panel results. Must be either
#' \code{"within_panel"} or \code{"across_panel"}. Default is "within_panel".
#' @param legend Logical; whether to display the legend (default is TRUE).
#'
#' @details
#' For a single panel, category probability curves are colored by module
#' or pathway.
#'
#' For multiple panels, two visualization modes are supported:
#' \itemize{
#'   \item \code{"within_panel"}: curves are colored by module or pathway
#'   and faceted by panel.
#'   \item \code{"across_panel"}: curves are colored by panel and faceted
#'   by module or pathway.
#' }
#'
#' For dichotomously-scored items (two response categories), only the
#' probability curve for the correct-response category (\code{cat1}) is
#' displayed.
#'
#' For polytomously-scored items (more than two response categories),
#' probability curves for all response categories are displayed.
#'
#' Category probability curves represent the average probability of each
#' response category across items within the selected module or pathway
#' at each ability value.
#'
#' @return A \code{ggplot} object.
#'
#' @seealso [report_test_tcc()]
#'
#' @export

plot_panel_tcc <- function(assembled_panel,item_par_cols,model_col,D = 1,theta = seq(-5,5,0.1),
                           unit = "module",
                           mode = "within_panel",
                           legend = TRUE) {
  if (!inherits(assembled_panel, "mstATA_panel")) {
    stop("Input 'assembled_panel' must be an object of class 'mstATA_panel'.",
         call. = FALSE)
  }
  unit <- match.arg(unit,choices = c("module", "pathway"))
  mode <- match.arg(mode,choices = c("within_panel", "across_panel"))

  first_panel <- assembled_panel[[1]]
  if (unit == "module") {
    which_module  <- sort(unique(first_panel$ItemsInModules$module_id))
    which_pathway <- NULL
  } else {
    which_module  <- NULL
    which_pathway <- sort(unique(first_panel$ItemsInPathways$pathway_id))
  }

  plot_df<-report_test_tcc(assembled_panel,theta = theta,item_par_cols = item_par_cols,
                           model_col = model_col,D = D,
                           which_module = which_module,which_pathway = which_pathway)
  ## ---- collect into tidy data ----
  unit_id <- paste0(unit, "_id")
  ## ---- category columns ----
  required_cols <- c("panel_id", unit_id, "theta")
  cat_cols <- setdiff(names(plot_df), required_cols)
  n_cat <- length(cat_cols)
  if (n_cat == 2) {
    cat_cols <- "cat1"
    dichotomous <- TRUE
  } else {
    dichotomous <- FALSE
  }


  n_panel<-length(unique(plot_df$panel_id))

  plot_long <- tidyr::pivot_longer(plot_df,
                                   cols = dplyr::all_of(cat_cols),
                                   names_to = "category",values_to = "probability")

  ## ---- aesthetics ----
  if (n_panel == 1) {
    color_var <- unit_id
    facet_var <- NULL
  } else if (mode == "within_panel") {
    color_var <- unit_id
    facet_var <- "panel_id"
  } else {
    color_var <- "panel_id"
    facet_var <- unit_id
  }
  if (!is.factor(plot_long[[unit_id]])) {
    plot_long[[unit_id]] <- factor(plot_long[[unit_id]])
  }
  ## ---- plot ----
  p <- ggplot2::ggplot(
    plot_long,
    ggplot2::aes(
      x = .data[["theta"]],
      y = .data[["probability"]],
      color = .data[[color_var]]
    )
  )

  if (!dichotomous) {
    p <- p + ggplot2::aes(linetype = .data[["category"]])
  }

  p<-p+ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_x_continuous(
      breaks = pretty(range(theta), n = 11)) +
    ggplot2::labs(
      x = expression(theta),
      y = if (dichotomous) "P(X = 1)" else "Category Probability",
      color = if (color_var == "panel_id") "Panel" else unit,
      linetype = if (dichotomous) NULL else "category"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "grey85",color = NA),
      strip.text = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = if (legend) "bottom" else "none",
      legend.direction = "horizontal"
    )

  if (!is.null(facet_var)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_var)))
  }

  p
}
