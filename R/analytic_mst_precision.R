#' @title Evaluate MST designs via recursive score-distribution propagation
#'
#' @description
#'
#' Computes conditional bias and conditional standard error of measurement (CSEM)
#' for a multistage test (MST) panel using a recursion-based evaluation method.
#' The implementation follows the logic of \code{irtQ::reval_mst()}, propagating
#' score distributions stage by stage conditional on true ability \eqn{\theta},
#' applying routing rules, and mapping final scores to reported ability estimates
#' via inverse test characteristic curves (TCCs).
#'
#' This function assumes that item- and module-level response category
#' probabilities (ICCs) have already been computed on a fixed ability grid.
#'
#' @param design A character string specifying the MST design. The string encodes
#'   the number of modules per stage and may use commas (\code{,}), dashes
#'   (\code{-}), or slashes (\code{/}) as separators.
#'   For example, \code{"1-3-3"}, \code{"1,3,3"}, and \code{"1/3/3"} all define
#'   an MST with 1 module in stage 1, 3 modules in stage 2, and 3 modules in stage 3.
#' @param icc_by_mod List of ICC objects indexed by module.
#' Each element must be a named list over a common ability grid
#' (e.g., \code{"theta=-1"}, \code{"theta=0"}, ...), where each element is an
#' item-by-category probability matrix compatible with
#' \code{\link{module_score_dist}}.
#' @param cut_scale Character string indicating the scale of routing cuts.
#'   Must be one of \code{"score"} or \code{"theta"}. Default is "score".
#' @param cuts List of length \code{NumStages - 1}.Each element is a numeric
#'   vector of internal cuts defining routing thresholds for the next stage.
#' @param range_tcc Numeric length-2 vector giving the search interval for
#' inverse TCC evaluation (passed to \code{\link{inv_tcc_from_icc}}). Defaults is (-5,5).
#' @param tol A convergence tolerance used for inverse TCC calculations. Default is 1e-4.
#' @return A list with components:
#' \itemize{
#'   \item \code{cdist_by_mod}: module-level score distributions
#'   \item \code{joint_dist}: stage-wise joint score distributions by prefix
#'   \item \code{prefix_table}: prefix-to-module lookup tables by stage
#'   \item \code{icc_path}: pathway-level ICC objects
#'   \item \code{inv_theta_by_path}: score-to-theta lookup tables
#'   \item \code{eval_tb}: data.frame with columns
#'     \code{theta, mu, sigma2, bias, csem}
#' }
#'
#' @details
#'
#' **1. Recursion-based analytic evaluation**
#'
#' The analytic evaluation proceeds in four conceptual steps:
#'
#' \enumerate{
#'   \item \strong{Module score distributions}
#'
#'   For each module, the conditional score distribution
#'   \eqn{P(S_m \mid \theta)} is computed from item response category
#'   probabilities using \code{\link{module_score_dist}}.
#'
#'   \item \strong{Stage-wise recursion with routing}
#'
#'   Joint (cumulative) score distributions are built recursively across
#'   stages. At each stage, examinees are deterministically assigned
#'   to next-stage modules based on their observed cumulative score and the
#'   predefined score cut points for that stage.
#'
#'   \item \strong{Cut score normalization}:
#'
#'   Routing cut scores are defined at the stage level and shared across all prefixes.
#'   If \code{cut_scale = "theta"}, cut scores are converted once per prefix to the
#'   score scale using the prefix-specific test characteristic curve (TCC). These
#'   cumulative score thresholds may be non-integer; observed
#'   scores are integers and are compared against the real-valued cut points
#'   during routing.
#'
#'   \item \strong{Exact joint score propagation}
#'
#'   For each valid routing branch, the cumulative score distribution is
#'   updated by convolving the previous-stage score distribution with the
#'   score distribution of the next module.
#'
#'   \item \strong{Inverse TCC and conditional moments}
#'
#'   Final pathway-specific score distributions are mapped to reported ability
#'   estimates via inverse test characteristic curves (ITCCs). The conditional
#'   mean (bias) and conditional variance (CSEM) are
#'   computed at each true ability value \eqn{\theta}.
#' }
#'
#'
#' **2. Conditional bias and conditional SEM**
#'
#' Let \eqn{S} denote the cumulative test score and
#' \eqn{\hat\theta(S)} the ITCC estimator.
#'
#' For each true ability value \eqn{\theta}, this function computes:
#'
#' \deqn{
#'   \mu(\theta) = E[\hat\theta \mid \theta], \quad
#'   \sigma^2(\theta) = \mathrm{Var}(\hat\theta \mid \theta)
#' }
#'
#' where the expectation is taken with respect to the exact joint score
#' distribution induced by the MST routing rules.
#'
#' @section Mathematical Formulation:
#'
#' Let \eqn{\theta \in \Theta} denote a true ability value on a discrete grid.
#' Consider a multistage test (MST) consisting of \eqn{T} stages, where at each
#' stage exactly one module is administered according to deterministic routing
#' rules that map observed cumulative scores to modules.
#'
#' \strong{(a) Module score distributions}
#'
#' For module \eqn{m}, let \eqn{S_m} denote the module score. Conditional on
#' ability \eqn{\theta}, the module score distribution is
#' \deqn{
#'   P(S_m = s \mid \theta),
#' }
#' which is obtained by analytically combining item response category
#' probabilities under the specified IRT model. This distribution is computed
#' independently for each module using \code{\link{module_score_dist}}.
#'
#' \strong{(b) Stage-wise recursion with routing}
#'
#' Let \eqn{S^{(t)}} denote the cumulative score after stage \eqn{t}.
#' At each stage \eqn{t}, examinees are assigned to a
#' next-stage module \eqn{m_{t+1}} according to score cut points
#' \eqn{c^{(t)}_1, \dots, c^{(t)}_{K_t}} defined on the cumulative score scale.
#'
#' The routing rule can be written as a deterministic mapping
#' \deqn{
#'   m_{t+1} = r_t\!\left(S^{(t)}\right),
#' }
#' where \eqn{r_t(\cdot)} maps observed cumulative scores to modules at stage
#' \eqn{t+1}.
#'
#' \strong{(c) Exact joint score propagation}
#'
#' For a given routing branch induced by the realized cumulative score, the
#' cumulative score evolves as
#' \deqn{
#'   S^{(t+1)} = S^{(t)} + S_{m_{t+1}}.
#' }
#'
#' Conditional on \eqn{\theta}, the joint score distribution is updated
#' analytically via convolution:
#' \deqn{
#'   P\!\left(S^{(t+1)} = s \mid \theta\right)
#'   =
#'   \sum_{u}
#'   P\!\left(S^{(t)} = u \mid \theta\right)
#'   P\!\left(S_{m_{t+1}} = s-u \mid \theta\right),
#' }
#' where the sum is taken over all reachable values of \eqn{S^{(t)}} along the
#' routing branch.
#'
#' \strong{(d) Inverse TCC and conditional moments}
#'
#' Let \eqn{S = S^{(T)}} denote the final cumulative test score, and let
#' \eqn{\hat\theta(S)} be the reported ability estimate obtained by applying the
#' inverse test characteristic curve (TCC) associated with the administered
#' pathway determined by the routing rules.
#'
#' For each true ability value \eqn{\theta}, the conditional mean and conditional
#' variance of the reported ability are defined as
#' \deqn{
#'   \mu(\theta) = E[\hat\theta(S) \mid \theta],
#'   \qquad
#'   \sigma^2(\theta) = \mathrm{Var}(\hat\theta(S) \mid \theta),
#' }
#' where the expectation is taken with respect to the exact joint distribution
#' \eqn{P(S \mid \theta)} induced by the MST routing rules.
#'
#' Explicitly,
#' \deqn{
#'   \mu(\theta)
#'   =
#'   \sum_{s} \hat\theta(s)\, P(S = s \mid \theta),
#' }
#' and
#' \deqn{
#'   \sigma^2(\theta)
#'   =
#'   \sum_{s} \bigl[\hat\theta(s) - \mu(\theta)\bigr]^2
#'   P(S = s \mid \theta).
#' }
#'
#' @seealso [analytic_mst_precision_items()],[module_score_dist()],[joint_module_score_dist()],
#' [normalize_cut_scores()],[inv_tcc_from_icc()]
#'
#' @references
#' Lim, H., Davey, T., & Wells, C. S. (20201).A recursion-based analytical approach to evaluate the performance of MST.
#' \emph{Journal of Educational Measurement}, 58(2), 154--178.{doi:10.1111/jedm.12276}
#'
#' @export
analytic_mst_precision<-function(design,
                                 icc_by_mod,
                                 cut_scale = "score",
                                 cuts,
                                 range_tcc = c(-5, 5),
                                 tol = 1e-4) {
  Design <- suppressWarnings(as.integer(unlist(strsplit(design, "[-, /]"))))
  if (anyNA(Design) || any(Design < 1)) {
    stop("`design` must be a string of positive integers, e.g., '1-3-3'.",
         call. = FALSE)
  }
  NumStages <- length(Design)

  structure<-create_pathways(design = design)
  ModuleIndex<-structure$Modules
  NumModules<-nrow(ModuleIndex)
  PathwayIndex<-structure$Pathways
  NumPathways<-nrow(PathwayIndex)
  stage_cols <- paste0("stage", seq_len(NumStages))
  path_mods <- PathwayIndex[, stage_cols, drop = FALSE]

  cut_scale<-match.arg(cut_scale,choices = c("score","theta"))



  if (!is.list(icc_by_mod) || length(icc_by_mod) != NumModules) {
    stop("`icc_by_mod` must be a list of length = NumModules.", call. = FALSE)
  }
  if (!is.list(cuts) || length(cuts) != (NumStages - 1L)) {
    stop("`cuts` must be a list of length = NumStages - 1.", call. = FALSE)
  }
  if (!is.numeric(range_tcc) || length(range_tcc) != 2L || anyNA(range_tcc)) {
    stop("`range_tcc` must be a numeric length-2 vector.", call. = FALSE)
  }

  ## ---- theta grid checks ----
  theta_names <- names(icc_by_mod[[1]])
  if (is.null(theta_names)) {
    stop("Each ICC element must be a named list indexed by theta.", call. = FALSE)
  }
  theta_grid_by_mod <- lapply(icc_by_mod, names)
  if (!all(vapply(theta_grid_by_mod, identical, logical(1), theta_names))) {
    stop("All modules must share the same theta grid (names).", call. = FALSE)
  }
  ## parse numeric theta for bias/csem (robust-ish)
  theta_num <- suppressWarnings(as.numeric(theta_names))
  if (anyNA(theta_num)) {
    theta_num <- suppressWarnings(as.numeric(sub("^theta=", "", theta_names)))
  }
  if (anyNA(theta_num)) {
    stop("Cannot parse numeric theta values from theta grid names.", call. = FALSE)
  }
  ## ---- module score distributions P(S_m | theta)----
  cdist_by_mod <- lapply(icc_by_mod, module_score_dist)
  ## Ensure theta columns align for every module distribution
  for (m in seq_len(NumModules)) {
    mat <- cdist_by_mod[[m]]
    if (!is.matrix(mat) || is.null(colnames(mat)) || !identical(colnames(mat), theta_names)) {
      stop("`module_score_dist()` output must be a matrix with colnames identical to theta grid names.", call. = FALSE)
    }
  }
  mod_max_score <- vapply(cdist_by_mod, function(mat) nrow(mat)-1L, integer(1))
  names(mod_max_score) <- as.character(seq_len(NumModules))

  ## coerce module ids to character keys consistently
  path_mods_chr <- as.data.frame(lapply(path_mods, function(z) as.character(z)),
                                 stringsAsFactors = FALSE)
  ## precompute prefix keys for each pathway row and each stage
  prefix_keys_by_stage <- vector("list", NumStages)
  for (s in seq_len(NumStages)) {
    prefix_keys_by_stage[[s]] <- apply(path_mods_chr[, stage_cols[1:s], drop = FALSE], 1,
                                       paste, collapse = "_")
  }
  ## mapping full prefix key -> pathway_index (should be 1-1)
  key_to_pathway <- setNames(seq_len(NumPathways),
                             prefix_keys_by_stage[[NumStages]])
  # ## ---- pathway-level ICC (cached) ----
  icc_path <- vector("list", NumPathways)
  for (pid in seq_len(NumPathways)) {
    mods_pid <- as.character(unlist(path_mods_chr[as.character(pid), , drop = FALSE]))
    icc_path[[pid]] <- compute_icc_by_path(icc_by_mod, mods_pid)
  }
  ## ---- helper: build prefix_table from keys ----
  keys_to_table <- function(keys, s) {
    parts <- strsplit(keys, "_", fixed = TRUE)
    mat <- do.call(rbind, lapply(parts, function(v) v[seq_len(s)]))
    out <- as.data.frame(mat, stringsAsFactors = FALSE)
    names(out) <- stage_cols[seq_len(s)]
    rownames(out) <- NULL
    out
  }

  ## ---- stage recursion storage ----
  joint_dist <- vector("list", NumStages)     # each element: named list(prefix_key -> score x theta matrix)
  prefix_table <- vector("list", NumStages)   # each element: data.frame stage1..stageS for existing prefixes

  ## ---- stage 1 initialization (multiple start modules supported) ----
  start_mods <- sort(unique(path_mods_chr[[stage_cols[1]]]))
  joint_dist[[1]] <- setNames(lapply(start_mods, function(m) cdist_by_mod[[as.integer(m)]]),
                              start_mods)
  prefix_table[[1]] <- data.frame(stage1 = start_mods, stringsAsFactors = FALSE)

  ## ---- recursion for stages 2..S ----
  for (s in 2:NumStages) {
    cuts_s <- cuts[[s - 1L]]
    if (!is.numeric(cuts_s) || anyNA(cuts_s) ||
        any(diff(cuts_s) <= 0)) {
      stop("cuts[[", s - 1L, "]] must be a strictly increasing numeric vector.",
           call. = FALSE)
    }
    prev_list <- joint_dist[[s - 1L]]
    prev_keys <- names(prev_list)

    next_mod_by_prev <- split(
      path_mods_chr[[stage_cols[s]]],
      prefix_keys_by_stage[[s - 1L]]
    )
    num_next_mods<-sort(unique(as.character(unique(unlist(next_mod_by_prev)))))
    if(length(cuts_s)!=length(num_next_mods)-1L){
      stop(paste0("cuts[[",s-1L,"]] length mismatch at stage ",s),call. = FALSE)
    }
    new_stage <- list()
    for (pk in prev_keys) {
      prev_mat <- prev_list[[pk]]
      if (!is.matrix(prev_mat) || is.null(colnames(prev_mat)) || !identical(colnames(prev_mat), theta_names)) {
        stop("joint_dist[[", s - 1L, "]][[", pk, "]] theta grid mismatch.", call. = FALSE)
      }
      # Score support in prev_mat
      prev_scores <- suppressWarnings(as.integer(rownames(prev_mat)))
      if (anyNA(prev_scores)) {
        stop("Previous joint score matrices must have rownames as integer scores.", call. = FALSE)
      }

      taken_mods<-as.integer(strsplit(pk,"_")[[1]])
      next_mods <- sort(unique(as.character(next_mod_by_prev[[pk]])))
      normalized_cuts_s<-normalize_cut_scores(cut_scale = cut_scale,cuts = cuts_s,
                                              icc_list = compute_icc_by_path(icc_by_mod,
                                                                             taken_mods))
      lower <- c(-Inf, normalized_cuts_s)
      upper <- c(normalized_cuts_s, Inf)

      for (b in seq_along(next_mods)) {

        mid <- as.integer(next_mods[b])
        # routing mask: only prev scores that lead to branch b
        in_branch <- (prev_scores > lower[b]) & (prev_scores <= upper[b])
        prev_mat_b <- prev_mat[in_branch, , drop = FALSE]
        prev_scores_b <- prev_scores[in_branch]
        if (nrow(prev_mat_b) == 0L) next

        # support of joint scores for this branch (optional but fine)
        possible_scores_b <- compute_joint_score_support(
          prev_scores = prev_scores_b,
          next_mod_max_score = mod_max_score[as.character(mid)],
          lower = lower[b],
          upper = upper[b]
        )[[1]]
        if(length(possible_scores_b)==0){
          message(paste0("Module ",mid," is not reachable for examinees who have taken Module ",
                         paste(taken_mods,collapse = " + ")))
        }else{
          new_key <- paste(pk, next_mods[b], sep = "_")
          new_stage[[new_key]] <- joint_module_score_dist(
            cdist_by_prev = prev_mat_b,
            prev_scores = prev_scores_b,
            icc_by_next_mod = icc_by_mod[[mid]],
            possible_joint_score = possible_scores_b
          )
        }
      }
    }

    joint_dist[[s]] <- new_stage
    prefix_table[[s]] <- keys_to_table(names(new_stage), s)
  }

  ## ---- inverse TCC tables ----
  inv_theta_by_path <- vector("list", NumPathways)
  max_score_by_path <- integer(NumPathways)

  for (pid in seq_len(NumPathways)) {
    mods <- as.character(unlist(path_mods_chr[as.character(pid), ]))
    max_score <- sum(mod_max_score[mods])
    max_score_by_path[pid] <- max_score

    inv_theta_by_path[[pid]] <- vapply(
      0:max_score,
      function(sc) inv_tcc_from_icc(
        icc_list  = icc_path[[pid]],
        target_score = sc,
        range_tcc = range_tcc,
        tol = tol
      ),
      numeric(1)
    )
    names(inv_theta_by_path[[pid]])<-as.character(0:max_score)
  }

  ## ---- evaluate precision ----
  final_keys <- names(joint_dist[[NumStages]])
  path_id_for_key <- key_to_pathway[final_keys]
  final_list <- vector("list", length(final_keys))
  names(final_list) <- final_keys
  for(k in final_keys){
    prob_mat <- joint_dist[[NumStages]][[k]]   # numeric matrix; rows are scores, cols are theta_names
    if (!is.matrix(prob_mat) || is.null(colnames(prob_mat)) || !identical(colnames(prob_mat), theta_names)) {
      stop("Final joint distribution matrix theta grid mismatch for key ", k, ".", call. = FALSE)
    }
    pathway_id <- unname(path_id_for_key[[k]])
    itcc <- inv_theta_by_path[[pathway_id]]    # named numeric vector: names are "0","1",...

    score_names <- rownames(prob_mat)
    if (is.null(score_names)) stop("Final joint_dist matrices must have rownames = scores.", call. = FALSE)

    idx <- match(score_names, names(itcc))
    if (anyNA(idx)) {
      stop("Inverse TCC missing scores for pathway ", pathway_id, " (key ", k, ").", call. = FALSE)
    }

    final_list[[k]] <- list(
      prob = prob_mat,
      thetahat = unname(itcc[idx])
    )
  }

  eval_mat <- matrix(NA_real_, length(theta_num), 2,
                     dimnames = list(theta_names, c("mu", "sigma2")))

  for (t in seq_along(theta_num)) {

    tot_p <- 0
    s1 <- 0
    s2 <- 0

    for (k in final_keys) {
      prob_vec <- final_list[[k]]$prob[, t]       # probabilities over scores at theta t
      th <- final_list[[k]]$thetahat              # thetahat per score

      # safety
      if (length(prob_vec) != length(th)) stop("prob/thetahat length mismatch for key ", k, call. = FALSE)

      tot_p <- tot_p + sum(prob_vec)
      s1 <- s1 + sum(prob_vec * th)
      s2 <- s2 + sum(prob_vec * (th^2))
    }

    if (tot_p > 0) {
      mu <- s1 / tot_p
      sigma2 <- max(0, s2 / tot_p - mu^2)
      eval_mat[t, ] <- c(mu, sigma2)
    }
  }

  eval_tb <- data.frame(
    theta = theta_num,
    mu = eval_mat[, "mu"],
    sigma2 = eval_mat[, "sigma2"],
    bias = eval_mat[, "mu"] - theta_num,
    csem = sqrt(eval_mat[, "sigma2"])
  )

  eval_tb[,"rmse"]<-sqrt(eval_tb$bias^2+eval_tb$sigma2)

  list(
    cdist_by_mod = cdist_by_mod,
    joint_dist = joint_dist,
    prefix_table = prefix_table,
    icc_path = icc_path,
    inv_theta_by_path = inv_theta_by_path,
    eval_tb = eval_tb
  )
}
