#' @title Evaluate MST designs via recursive score-distribution propagation
#'
#' @description
#'
#' Computes conditional bias and conditional standard error of measurement (CSEM)
#' for a multistage test (MST) panel using a recursion-based evaluation method.
#' The implementation follows the logic of \code{irtQ::reval_mst()}, propagating
#' score distributions stage by stage conditional on true ability \eqn{\theta}{\theta},
#' applying routing rules, and mapping final scores to reported ability estimates
#' via inverse test characteristic curves (TCCs).
#'
#' @param design A character string specifying the MST design. The string encodes
#'   the number of modules per stage and may use commas, dashes, or slashes
#'  as separators. For example, "1-3-3", "1,3,3", and "1/3/3" all define
#'   an MST with 1 module in stage 1, 3 modules in stage 2, and 3 modules in stage 3.
#' @param exclude_pathways Optional character vector specifying disallowed pathways.
#'   Each element must be a string of the form "x-y-z", where each number
#'   refers to a module index at the corresponding stage. If \code{NULL} (default),
#'   all pathways implied by the design are allowed. This option is commonly used
#'   to prohibit extreme routing transitions (e.g., from very easy to very hard
#'   modules).
#' @param assembled_panel A "mstATA_panel" object returned by
#' \link{assembled_panel}(). All panels share identical module and
#' pathway structure.
#' @param item_par_cols A named list defining the IRT parameter columns
#'   required for each supported model. The list names must exactly match
#'   the model identifiers specified in \code{model_col} column:
#'   "1PL", "RASCH", "2PL", "3PL","4PL", "GRM", "MGRM", "PCM",
#'   "GPCM", "RSM", and "NRM".
#'   Each list element is a character vector giving the column names
#'   that contain the required item parameters for
#'   the corresponding model.
#' @param model_col A character string specifying the column name in either
#'   \code{assembled_panel$ItemsInModules} or \code{assembled_panel$ItemsInPathways}
#'   data frames that indicates the IRT model used for each item.
#'   Values in this column must correspond to the supported
#'   model names: "1PL", "RASCH", "2PL", "3PL","4PL", "GRM", "MGRM", "PCM",
#'   "GPCM", "RSM", and "NRM".
#' @param D Scaling constant used in the IRT model.
#'    Default is D=1 (for logistic metric);
#'    D=1.702 yields approximately the normal metric.
#' @param theta Numeric vector specifying the ability grid used to construct
#'    ICCs. Defaults are -3 to 3 with 0.1 interval.
#' @param range_tcc Numeric vector giving the search interval for
#'   inverse TCC evaluation (length = 2). Defaults is (-5,5).
#' @param rdps List of routing cuts on the theta scale.
#' @param tol A convergence tolerance used for inverse TCC calculations.
#'   Default is 1e-4.
#'
#' @details
#' **1. Recursion-based analytic evaluation**
#'
#' The analytic evaluation proceeds in four conceptual steps:
#'
#' \enumerate{
#'   \item \strong{Module score distributions}
#'
#'   For each module, the conditional score distribution
#'   \eqn{P(S_m \mid \theta)}{P(S_m \mid \theta)} is computed from item response category
#'   probabilities.
#'
#'   \item \strong{Stage-wise recursion with routing}
#'
#'   Joint (cumulative) score distributions are built recursively across
#'   stages. At each stage, examinees are deterministically assigned
#'   to next-stage modules based on the comparison between their ITCC
#'   estimates and the predefined cut points for that stage.
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
#'   mean, conditional bias and conditional variance (CSEM) are
#'   computed at each true ability value \eqn{\theta}{\theta}.
#' }
#'
#'
#' **2. Conditional bias and conditional SEM**
#'
#' Let \eqn{S}{S} denote the cumulative test score and
#' \eqn{\hat \theta(S)}{\hat \theta(S)} the ITCC estimator.
#'
#' For each true ability value \eqn{\theta}{\theta}, this function computes:
#'
#' \deqn{\mu(\theta) = E(\hat{\theta} \mid \theta)}
#' \deqn{\sigma^2(\theta) = Var(\hat{\theta} \mid \theta)}
#'
#' where the expectation is taken with respect to the exact joint score
#' distribution induced by the MST routing rules.
#'
#' @section Mathematical Formulation:
#'
#' Let \eqn{\theta \in \Theta}{\theta \in \Theta} denote a true ability value on a discrete grid.
#' Consider a multistage test (MST) consisting of \eqn{T}{T} stages, where at each
#' stage exactly one module is administered according to deterministic routing
#' rules that map observed cumulative scores to modules.
#'
#' \strong{(a) Module score distributions}
#'
#' For module \eqn{m}{m}, let \eqn{S_m}{S_m} denote the module score. Conditional on
#' ability \eqn{\theta}{\theta}, the module score distribution is
#' \deqn{P(S_m = s \mid \theta)}
#'
#' which is obtained by analytically combining item response category
#' probabilities under the specified IRT model. This distribution is computed
#' independently for each module using
#' \link{module_score_dist}().
#'
#' \strong{(b) Stage-wise recursion with routing}
#'
#' Let \eqn{S^{(t)}}{S(t)} denote the cumulative score after stage \eqn{t}{t}.
#' At each stage \eqn{t}{t}, examinees are assigned to a
#' next-stage module \eqn{m_{t+1}}{m_{t+1}} based on the comparison between
#' the ITCC estimates and the cut theta points.
#'
#' The routing rule can be written as a deterministic mapping
#' \deqn{m_{t+1} = r_t(S^{(t)})}
#' where \eqn{r_t(\cdot)}{r_t(\cdot)} maps observed cumulative scores to modules at stage
#' \eqn{t+1}{t+1}.
#'
#' \strong{(c) Exact joint score propagation}
#'
#' For a given routing branch induced by the realized cumulative score, the
#' cumulative score evolves as
#' \deqn{S^{t+1} = S^t + S^{t+1}_m}
#'
#' Conditional on \eqn{\theta}{\theta}, the joint score distribution is updated
#' analytically via convolution:
#' \deqn{P(S^{t+1} = s \mid \theta) = \sum_u P(S^t = u \mid \theta) P(S^{t+1}_m = s - u \mid \theta)}
#' where the sum is taken over all reachable values of \eqn{S^{(t)}}{S^{(t)}} along the
#' routing branch.
#'
#' \strong{(d) Inverse TCC and conditional moments}
#'
#' Let \eqn{S^{(t)}}{S(t)} denote the final cumulative test score, and let
#' \eqn{\hat{\theta}(S)}{theta_hat(S)} be the reported ability estimate obtained by applying the
#' inverse test characteristic curve (TCC) associated with the administered
#' pathway determined by the routing rules.
#'
#' For each true ability value \eqn{\theta}{theta}, the conditional mean and conditional
#' variance of the reported ability are defined as
#'
#' \deqn{\mu(\theta) = E[\hat{\theta} \mid \theta]}
#'
#' \deqn{\sigma^2(\theta) = Var[\hat{\theta} \mid \theta]}
#'
#' where the expectation is taken with respect to the exact joint distribution
#' \eqn{P(S \mid \theta)}{P(S | theta)} induced by the MST routing rules.
#'
#' Explicitly,
#'
#' \deqn{\mu(\theta) = \sum_{s} \hat{\theta}(s)\, P(S = s \mid \theta)}
#'
#' \deqn{\sigma^2(\theta) =
#'       \sum_{s} (\hat{\theta}(s) - \mu(\theta))^2\, P(S = s \mid \theta)}
#'
#' @return A list with components:
#' \describe{
#'   \item{prefix_table}{prefix-to-module lookup tables by stage.}
#'   \item{eq_theta}{ITCC estimates for each stage.}
#'   \item{cdist_by_mod}{Module-level score distributions.}
#'   \item{joint_dist}{Stage-wise joint score distributions by prefix.}
#'   \item{eval_tb}{A data frame with columns
#'     \code{theta}, \code{mu}, \code{sigma2}, \code{bias}, and \code{csem}.}
#' }
#'
#' @seealso
#' [compute_icc()]
#'
#' [module_score_dist()]
#'
#' [joint_module_score_dist()]
#'
#' [inv_tcc_from_icc()]
#'
#' @references
#' Lim, H., Davey, T., & Wells, C. S. (2020).
#' A recursion-based analytical approach to evaluate the performance of MST.
#' \emph{Journal of Educational Measurement}, 58(2), 154--178.
#' doi:10.1111/jedm.12276
#'
#' @export

analytic_mst_precision<- function(design,exclude_pathways=NULL,
                                         assembled_panel,
                                         item_par_cols,model_col,
                                         D = 1,
                                         theta = seq(-3,3,0.1), range_tcc = c(-5,5),
                                         rdps,
                                         tol = 1e-4){
  ## ---- require validated assembled panel ----
  if (!inherits(assembled_panel, "mstATA_panel")) {
    stop("Input 'assembled_panel' must be an object of class 'mstATA_panel'.",
         call. = FALSE)
  }

  Structure <- create_pathways(design = design, diff_levels = NULL,exclude_pathways = exclude_pathways)
  ModuleIndex <- Structure$Modules
  NumStages<-length(unique(ModuleIndex$stage))
  NumModules<-nrow(ModuleIndex)

  PathwayIndex <- Structure$Pathways
  Allowed_PathwayIndex<-PathwayIndex[PathwayIndex$allowed,]
  Allowed_PathwayIndex$pathway_index <- seq_len(nrow(Allowed_PathwayIndex))
  rownames(Allowed_PathwayIndex) <- NULL
  NumPathways<-nrow(Allowed_PathwayIndex)

  stage_cols <- paste0("stage", seq_len(NumStages))
  path_mods <- Allowed_PathwayIndex[, stage_cols, drop = FALSE]
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
  ## ---- helper: build prefix_table from keys ----
  keys_to_table <- function(keys, s) {
    parts <- strsplit(keys, "_", fixed = TRUE)
    mat <- do.call(rbind, lapply(parts, function(v) v[seq_len(s)]))
    out <- as.data.frame(mat, stringsAsFactors = FALSE)
    names(out) <- stage_cols[seq_len(s)]
    rownames(out) <- NULL
    out
  }

  if (!is.list(rdps) || length(rdps) != (NumStages - 1L)) {
    stop("`rdps` must be a list of length = NumStages - 1.", call. = FALSE)
  }

  out <- lapply(names(assembled_panel), function(panel_name) {

    panel <- assembled_panel[[panel_name]]
    items_in_modules<-panel$ItemsInModules
    items_in_pathways<-panel$ItemsInPathways

    ## ---- module score distributions P(S_m | theta)----
    icc_by_mod<-vector("list",NumModules)
    names(icc_by_mod)<-as.character(seq_len(NumModules))
    for(module_id in seq_len(NumModules)){
      icc_by_mod[[as.character(module_id)]]<-compute_icc(items = items_in_modules[items_in_modules$module_id==module_id,],
                                                         item_par_cols = item_par_cols,theta = theta,model_col = model_col,
                                                         D = D)
    }
    cdist_by_mod <- lapply(icc_by_mod, module_score_dist)
    mod_max_score <- vapply(cdist_by_mod, function(mat) nrow(mat)-1L, integer(1))
    names(mod_max_score) <- as.character(seq_len(NumModules))

    ## ---- stage recursion storage ----
    joint_dist <- vector("list", NumStages)     # each element: named list(prefix_key -> score x theta matrix)
    eq_theta<-vector("list",NumStages)
    prefix_table <- vector("list", NumStages)   # each element: data.frame stage1..stageS for existing prefixes

    ## ---- stage 1 initialization (multiple start modules supported) ----
    start_mods <- sort(unique(path_mods_chr[[stage_cols[1]]]))
    joint_dist[[1]] <- setNames(lapply(start_mods, function(m) cdist_by_mod[[as.integer(m)]]),
                                start_mods)
    eq_theta[[1]]<-lapply(start_mods,function(m) inv_tcc_from_icc(items = items_in_modules[items_in_modules$module_id%in%as.integer(m),],
                                                                                item_par_cols = item_par_cols,model_col = model_col,
                                                                                D = D,range_tcc = range_tcc,tol = tol)$est.theta)
    names(eq_theta[[1]])<-start_mods
    eq_theta[[1]]<-lapply(eq_theta[[1]], function(x) {
      data.frame(score = seq_along(x) - 1,eq_theta = x)
    })
    prefix_table[[1]] <- data.frame(stage1 = start_mods, stringsAsFactors = FALSE)


    for(s in 2:NumStages){
      rdp_s <- rdps[[s - 1L]]
      if (!is.numeric(rdp_s) || anyNA(rdp_s) ||
          any(diff(rdp_s) <= 0)) {
        stop("rdp[[", s - 1L, "]] must be a strictly increasing numeric vector.",
             call. = FALSE)
      }
      prev_list <- joint_dist[[s - 1L]]
      prev_keys <- names(prev_list)
      next_mod_by_prev <- split(
        path_mods_chr[[stage_cols[s]]],
        prefix_keys_by_stage[[s - 1L]]
      )

      next_mod_by_prev <- next_mod_by_prev[intersect(names(next_mod_by_prev), prev_keys)]
      next_mod_ids <- sort(unique(unlist(next_mod_by_prev, use.names = FALSE)))
      if(length(rdp_s)!=length(next_mod_ids)-1L){
        stop(paste0("rdp[[",s-1L,"]] length mismatch at stage ",s),call. = FALSE)
      }
      new_stage <- list()
      temp_eqtheta<-list()

      for (pk in prev_keys) {
        prev_mat <- prev_list[[pk]]
        # Score support in prev_mat
        prev_scores <- suppressWarnings(as.integer(rownames(prev_mat)))
        if (anyNA(prev_scores)) {
          stop("Previous joint score matrices must have rownames as integer scores.", call. = FALSE)
        }
        taken_mods<-as.integer(strsplit(pk,"_")[[1]])
        next_mods <- sort(unique(as.character(next_mod_by_prev[[pk]])))
        lower <- c(-Inf, rdp_s)
        upper <- c(rdp_s, Inf)
        names(lower)<-names(upper)<-next_mod_ids
        region_to_module<-collapse_regions(global_next_mods  = as.integer(next_mod_ids),
                                           allowed_next_mods  = as.integer(next_mods))
        collapsed<-collapse_cuts(lower = lower,upper = upper,
                                 region_to_module = region_to_module)
        eqth_table<-eq_theta[[s-1]][[pk]]
        est_th<-eqth_table$eq_theta
        est_th_scores<-eqth_table$score
        for (b in seq_along(collapsed$module)) {
          mid <- as.integer(collapsed$module[b])
          l<- collapsed$lower[b]
          u<- collapsed$upper[b]
          in_branch <- est_th_scores[intersect(which(est_th>l),
                                               which(est_th<=u))]
          prev_mat_b <- prev_mat[which(prev_scores%in%in_branch), , drop = FALSE]
          prev_scores_b <- prev_scores[prev_scores%in%in_branch]
          if (nrow(prev_mat_b) == 0L) next

          # support of joint scores for this branch (optional but fine)
          possible_scores_b <- sort(unique(as.vector(outer(prev_scores_b,0:mod_max_score[as.character(mid)],`+`))))
          if(length(possible_scores_b)==0){
            message(paste0("Module ",mid," is not reachable for examinees who have taken Module ",
                           paste(taken_mods,collapse = " + ")))
          }else{
            new_key <- paste(pk,as.character(mid), sep = "_")
            new_stage[[new_key]] <- joint_module_score_dist(
              cdist_by_prev = prev_mat_b,
              prev_scores = prev_scores_b,
              icc_by_next_mod = icc_by_mod[[as.character(mid)]],
              possible_joint_score = possible_scores_b
            )
          }
          inv<-inv_tcc_from_icc(items = items_in_modules[items_in_modules$module_id%in%as.integer(strsplit(new_key,"_")[[1]]),],
                                item_par_cols = item_par_cols,model_col = model_col,
                                D = D,range_tcc = range_tcc,tol = tol)$est.theta
          temp_eqtheta[[new_key]]<-inv
        }
      }

      joint_dist[[s]] <- new_stage
      eq_theta[[s]]<-temp_eqtheta
      eq_theta[[s]]<-lapply(eq_theta[[s]], function(x) {
        data.frame(score = seq_along(x) - 1,eq_theta = x)
      })
      prefix_table[[s]] <- keys_to_table(names(new_stage), s)
    }

    inv_theta_by_path <- eq_theta[[NumStages]]

    pathway_scorepoints<-vapply(inv_theta_by_path,nrow,integer(1))
    if(length(unique(pathway_scorepoints))!=1){
      message("Pathways do not share the same maximum score.")
    }
    ## ---- evaluate precision ----
    final_keys <- names(joint_dist[[NumStages]])
    path_id_for_key <- key_to_pathway[final_keys]
    final_list <- vector("list", length(final_keys))
    names(final_list) <- final_keys
    for(k in final_keys){
      prob_mat <- joint_dist[[NumStages]][[k]]   # numeric matrix; rows are scores, cols are theta_names
      pathway_id <- unname(path_id_for_key[[k]])
      itcc_table <- inv_theta_by_path[[k]]    # named numeric vector: names are "0","1",...

      score_names <- rownames(prob_mat)
      if (is.null(score_names)) stop("Final joint_dist matrices must have rownames = scores.", call. = FALSE)

      idx <- match(score_names, itcc_table$score)
      if (anyNA(idx)) {
        stop("Inverse TCC missing scores for pathway ", pathway_id, " (key ", k, ").", call. = FALSE)
      }

      final_list[[k]] <- list(prob = prob_mat,thetahat = itcc_table$eq_theta[idx])
    }

    eval_mat <- matrix(NA_real_, length(theta), 2,
                       dimnames = list(NULL, c("mu", "sigma2")))

    for (t in seq_along(theta)) {

      tot_p <- 0
      s1 <- 0
      s2 <- 0

      for (k in final_keys) {

        prob_vec <- final_list[[k]]$prob[, t]   # P(score | theta_t)
        th       <- final_list[[k]]$thetahat    # theta_hat per score

        if (length(prob_vec) != length(th)) {
          stop("prob/thetahat length mismatch for key ", k, call. = FALSE)
        }

        if (any(prob_vec < 0)) {
          stop("Negative probabilities detected for key ", k, call. = FALSE)
        }

        tot_p <- tot_p + sum(prob_vec)
        s1    <- s1 + sum(prob_vec * th)
        s2    <- s2 + sum(prob_vec * (th^2))
      }

      if (tot_p > 0) {

        mu <- s1 / tot_p

        # Conditional variance: E(theta^2) - mu^2
        sigma2 <- s2 / tot_p - mu^2

        # Numerical safeguard
        sigma2 <- max(0, sigma2)

        eval_mat[t, ] <- c(mu, sigma2)
      }
    }

    eval_tb <- data.frame(
      theta = theta,
      mu = eval_mat[, "mu"],
      sigma2 = eval_mat[, "sigma2"],
      bias = eval_mat[, "mu"] - theta,
      csem = sqrt(eval_mat[, "sigma2"])
    )

    list(
      prefix_table = prefix_table,
      eq_theta = eq_theta,
      cdist_by_mod = cdist_by_mod,
      joint_dist = joint_dist,
      eval_tb = eval_tb
    )
  })
  names(out)<-names(assembled_panel)
  return(out)
}

