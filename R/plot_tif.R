#' @title Plot Test Information Functions
#'
#' @description
#'
#' Plots testing information functions (TIFs).
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
#' @return A \code{ggplot} object.
#'
#'
#' @export


plot_tif<-function(items,item_par_cols,theta = seq(-3,3,0.1),model_col,D = 1){
  plot_dat<-compute_iif(items = items,item_par_cols = item_par_cols,
                        model_col = model_col,theta = theta,D = D)
  plot_dat2<-data.frame(theta = theta,
                        information = apply(plot_dat,2,sum))
  ggplot2::ggplot(plot_dat2,
                  ggplot2::aes(x = .data[["theta"]],y = .data[["information"]]))+
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(x = expression(theta),y = "Information") +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "grey85",color = NA),
                   strip.text = ggplot2::element_text(face = "bold"))

}


