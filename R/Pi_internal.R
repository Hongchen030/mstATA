#' Compute Item Response Category Probabilities and Derivatives
#'
#' Computes item response category probabilities under various IRT models
#' at a given ability value \eqn{\theta}.
#'
#' @param theta A numeric scalar specifying the ability value at which
#'   probabilities are evaluated.
#'
#' @param itempar_mat A numeric matrix with one row per item and columns
#'   corresponding to the required parameters for the specified IRT model.
#'   The required structure depends on \code{model}. See Details.
#'
#' @param model A character string specifying the IRT model. Supported models are:
#'   \code{"GRM"}, \code{"MGRM"}, \code{"PCM"}, \code{"GPCM"},
#'   \code{"RSM"}, and \code{"NRM"}.
#'   If \code{NULL}, a dichotomous logistic model (1PL/2PL/3PL/4PL)
#'   is assumed.
#'
#' @param D Scaling constant used in the IRT model. Default is D=1 (for logistic metric);
#'  D=1.702 yields approximately the normal metric.
#'
#' @details
#'
#' (1) Dichotomous IRT models
#'
#' For the models \code{"1PL"}, \code{"RASCH"}, \code{"2PL"},
#' \code{"3PL"}, and \code{"4PL"}, the argument \code{itempar_mat}
#' must be a numeric matrix with one row per item and four columns:
#' discrimination (\eqn{a_j}), difficulty (\eqn{b_j}), guessing (\eqn{c_j}),upper (\eqn{d_j}).
#'
#'
#' (2) Polytomous IRT models
#'
#' For a polytomous item \eqn{j}, let \eqn{g_j} denote the maximum score,
#' so that the possible response categories are
#' \eqn{0, 1, \ldots, g_j}.
#'
#' The number of response categories may vary across items under the
#' \code{"GRM"}, \code{"PCM"}, \code{"GPCM"}, and \code{"NRM"} models.
#' In contrast, under the \code{"MGRM"} and \code{"RSM"} models,
#' the number of response categories is assumed to be the same for all items.
#'
#' The argument \code{itempar_mat} must be a numeric matrix with one row per item
#' and a number of columns determined by the specified IRT model. The required
#' structure is:
#'
#' \itemize{
#'   \item \strong{GRM}:
#'   A matrix with \eqn{\max_j g_j + 1} columns, containing parameters
#'   \eqn{(\alpha_j, \beta_{j1}, \ldots, \beta_{j g_j})},
#'   where \eqn{\alpha_j} is the discrimination parameter and
#'   \eqn{\beta_{jk}} are the ordered threshold parameters.
#'
#'   \item \strong{MGRM}:
#'   A matrix with \eqn{g_j + 2} columns, containing parameters
#'   \eqn{(\alpha_j, b_j, c_{j1}, \ldots, c_{j g_j})},
#'   where \eqn{\alpha_j} is the discrimination parameter,
#'   \eqn{b_j} is the location parameter, and
#'   \eqn{c_{jk}} are category step parameters.
#'
#'   \item \strong{PCM}:
#'   A matrix with \eqn{\max_j g_j} columns, containing step parameters
#'   \eqn{(\delta_{j1}, \ldots, \delta_{j g_j})}.
#'
#'   \item \strong{GPCM}:
#'   A matrix with \eqn{\max_j g_j + 1} columns, containing parameters
#'   \eqn{(\alpha_j, \delta_{j1}, \ldots, \delta_{j g_j})},
#'   where \eqn{\alpha_j} is the discrimination parameter and
#'   \eqn{\delta_{jk}} are step parameters.
#'
#'   \item \strong{RSM}:
#'   A matrix with \eqn{g + 1} columns, containing parameters
#'   \eqn{(\lambda_j, \delta_1, \ldots, \delta_g)},
#'   where \eqn{\lambda_j} is the item location parameter and
#'   \eqn{\delta_k} are common step parameters across items.
#'
#'   \item \strong{NRM}:
#'   A matrix with \eqn{2 \max_j g_j} columns, containing parameters
#'   \eqn{(\alpha_{j1}, c_{j1}, \alpha_{j2}, c_{j2}, \ldots,
#'   \alpha_{j g_j}, c_{j g_j})},
#'   where \eqn{\alpha_{jk}} and \eqn{c_{jk}} are the slope and intercept
#'   parameters for category \eqn{k}.
#' }
#'
#' @return A list with components:
#' \describe{
#'   \item{Pi}{A matrix of category response probabilities. Rows correspond
#'   to items and columns correspond to categories.}
#'
#'   \item{dPi}{Matrix of first derivatives
#'   \eqn{\partial P_{jk}(\theta)/\partial \theta}.}
#'
#' }
#'
#' @export
Pi_internal<-function(theta, itempar_mat,model = NULL, D = 1){
  if (!is.numeric(theta) ||length(theta) != 1L){
    stop("Pi_internal() expects a scalar theta.", call. = FALSE)
  }
  itempar_mat<-as.matrix(itempar_mat)
  if(is.null(model)){
    return(Pi_dichotomous(theta = theta,itempar_mat = itempar_mat,D = D))
  }

  model<-toupper(model)
  if (model %in% c("GRM","MGRM")) {
    return(Pi_grm(theta = theta,itempar_mat = itempar_mat,model = model,D = D))
  }

  if (model %in% c("PCM","GPCM","RSM","NRM")) {
    return(Pi_otherpoly(theta = theta,itempar_mat = itempar_mat,model = model,D = D))
  }

  stop("Unsupported IRT model: ", model, call. = FALSE)
}
