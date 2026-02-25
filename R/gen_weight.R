#' @title Generate population weights over a specified ability grid
#'
#' @description
#'
#' Computes normalized weights on a user-specified ability grid \code{theta} based on a chosen population distribution.
#' The function evaluates the specified density at each grid point and rescales the resulting values so that the weights sum to 1.
#' The output provides a discrete approximation of the population ability distribution and can be used for marginal
#' summaries in analytic classification accuracy or precision evaluation.
#'
#' Supports:
#' \itemize{
#'   \item Named base-R distributions via their densities (e.g., \code{"norm"}, \code{"t"}, \code{"logis"}, \code{"unif"}, \code{"beta"}, \code{"gamma"}, \code{"lnorm"}, …)
#'   \item Finite **mixtures** of named distributions
#'   \item **Empirical** distributions via Gaussian-kernel KDE
#' }
#'
#' Weights are normalized so that \code{sum(w) == 1}. Use them to approximate expectations:
#' \deqn{\mathbb{E}[f(\theta)] \approx \sum_i w_i f(\theta_i).}
#'
#' @param theta Optional numeric vector specifying the ability grid at which the
#'   population distribution is evaluated. Default is \code{seq(-5,5,0.1)}.
#' @param dist Character string specifying the distribution family:
#'   one of \code{"norm"}, \code{"t"}, \code{"logis"}, \code{"unif"}, \code{"beta"},
#'   \code{"gamma"}, \code{"lnorm"}, \code{"mixture"}, \code{"empirical"}. Default is \code{"norm"}.
#' @param params List of parameters for the named distribution (when \code{dist} is not
#'   \code{"mixture"}, \code{"empirical"}). For example,
#'   \code{list(mean = 0, sd = 1)} for \code{"norm"}, \code{list(df = 5)} for \code{"t"},
#'   \code{list(location = 0, scale = 1)} for \code{"logis"}. Default is \code{list(mean = 0, sd = 1)}.
#' @param components For \code{dist = "mixture"}: a list of components, each like
#'   \code{list(dist = "norm", params = list(mean = 0, sd = 1), weight = 0.7)}.
#'   Component weights are rescaled to sum to 1 if needed.
#' @param empirical_theta For \code{dist = "empirical"}: numeric sample theta values used to build a KDE.
#'
#' @return A dataframe with:
#' \describe{
#'   \item{\code{theta}}{The supplied ability grid.}
#'   \item{\code{w}}{Normalized weights, \code{sum(w) == 1}.}
#' }
#'
#' @examples
#'## Standard normal population on a custom grid
#' theta <- seq(-3, 3, by = 0.2)
#' gen_weight(theta,dist = "norm")
#'
#' ## Normal population with custom parameters
#' gen_weight(
#'   theta,
#'   dist = "norm",
#'   params = list(mean = -0.5, sd = 1.2)
#' )
#'
#' ## Empirical population via KDE
#' gen_weight(
#'   theta,
#'   dist = "empirical",
#'   empirical_theta = rnorm(1000)
#' )
#'
#' ## Two-component mixture distribution
#' gen_weight(
#'   theta,
#'   dist = "mixture",
#'   components = list(
#'     list(dist = "norm", params = list(mean = -1, sd = 0.8), weight = 0.4),
#'     list(dist = "norm", params = list(mean =  1, sd = 0.8), weight = 0.6)
#'   )
#' )
#'
#'@export
#'

gen_weight <- function(theta = seq(-5,5,0.1),
                       dist = "norm",
                       params = list(mean = 0, sd = 1),
                       components = NULL,
                       empirical_theta = NULL){

  if (!is.numeric(theta) || any(!is.finite(theta))) {
    stop("`theta` must be a finite numeric vector.", call. = FALSE)
  }

  theta<-sort(theta,decreasing = FALSE)

  dist <- match.arg(dist,choices = c("norm","t","logis","unif","beta","gamma",
                                     "lnorm","mixture","empirical"))

  ## ---- helper ----
  get_density_fun <- function(d) {
    fun <- tryCatch(get(paste0("d", d), mode = "function"),
                    error = function(e) NULL)
    if (!is.function(fun)) {
      stop("Unknown density: ", d,
           " (expected base-R density d", d, ").",
           call. = FALSE)
    }
    fun
  }

  dens <- switch(dist,
                 "empirical" = {
                   if (is.null(empirical_theta)) {
                     stop("Provide sample vector `empirical_theta` when dist='empirical'.",
                          call. = FALSE)
                   }
                   d <- stats::density(empirical_theta, bw = "nrd0", kernel = "gaussian")
                   stats::approx(d$x, d$y, xout = theta, yleft = 0, yright = 0, ties = "ordered")$y
                 },
                 "mixture" = {
                   if (is.null(components) || !length(components)){
                     stop("Provide non-empty `components` for a mixture distribution.",
                          call. = FALSE)
                   }
                   dens_mix <- numeric(length(theta))
                   wsum <- 0
                   for (comp in components) {
                     if (!is.list(comp) || is.null(comp$dist) || is.null(comp$weight)) {
                       stop("Each mixture component must define 'dist' and 'weight'.",
                            call. = FALSE)
                     }
                     fun <- get_density_fun(comp$dist)
                     wi <- comp$weight
                     if (!is.finite(wi) || wi < 0) {
                       stop("Mixture component weights must be non-negative and finite.",
                            call. = FALSE)
                     }
                     pi  <- if (!is.null(comp$params)) comp$params else list()

                     di <- tryCatch(
                       do.call(fun, c(list(x = theta), pi)),
                       error = function(e)
                         stop("Invalid parameters for mixture component '",
                              comp$dist, "'.", call. = FALSE)
                     )
                     if (anyNA(di)) {
                       stop("Mixture component '", comp$dist,
                            "' produced NA densities.", call. = FALSE)
                     }
                     dens_mix <- dens_mix + wi * di
                     wsum <- wsum + wi
                   }
                   if (abs(wsum - 1) > 1e-8) dens_mix <- dens_mix / wsum
                   dens_mix
                 },
                 {
                   fun <- get_density_fun(dist)
                   tryCatch(
                     do.call(fun, c(list(x = theta), params)),
                     error = function(e)
                       stop("Invalid parameters for distribution '", dist, "'.",
                            call. = FALSE)
                   )
                 })

  if (!is.numeric(dens) || anyNA(dens)) {
    stop("Density evaluation failed (NA/NaN).",
         call. = FALSE)
  }
  dens[dens < 0] <- 0

  raw <- dens
  if (length(raw) >= 2L) {
    raw[1]   <- raw[1]   / 2
    raw[length(raw)] <- raw[length(raw)] / 2
  }

  s <- sum(raw)
  if (s <= 0) {
    stop("Density integrates to ~0 over the provided theta grid.",
         call. = FALSE)
  }
  w <- raw / s
  out<-data.frame(theta = theta, w = w)
  return(out)
}
