#' @title Compile a Weighted-Sum Objective
#'
#' @description
#' This function compiles a **weighted-sum objective** when multiple objective
#' components are present. Each component contributes a linear score, and the
#' solver optimizes a weighted linear combination of these scores.
#'
#' The weighted-sum strategy is the simplest and most flexible multi-objective
#' formulation. It is appropriate when objective components are commensurate
#' (i.e., comparable in scale) or when weights can be meaningfully justified
#' based on operational priorities. It is also the only strategy that allows
#' combining multiple compiled objectives produced by other multi-objective
#' formulations (e.g., maximin, capped maximin, minimax, or goal programming)
#' into a single final objective.
#'
#' Under this strategy, multiple linear objective components are aggregated
#' into one objective function by forming a weighted linear combination.
#'
#' @param x An object of class \code{mstATA_design} created by \code{mst_design()}.
#' @param obj_set A list containing **two or more** objective components.
#'   Each element must be either:
#'   \describe{
#'     \item{An objective term}{created using \code{objective_term()}.}
#'     \item{A compiled objective}{produced by a multi-objective formulation
#'       such as \code{maximin_obj()}, \code{capped_maximin_obj()},
#'       \code{minimax_obj()}, or \code{goal_programming_obj()}.}
#'   }
#'   Each element may represent either a relative or an absolute objective.
#'   The weighted aggregation respects the optimization sense (maximize or
#'   minimize) specified within each objective term or compiled objective.
#'
#' @param strategy A character string specifying the optimization strategy.
#' Currently, only \code{"weighted_sum"} is supported.
#'
#' @param strategy_args
#' A list of strategy-specific arguments. For the \code{"weighted_sum"}
#' strategy, \code{strategy_args} must be either an empty list or
#' \code{list(weights = weights)}. The \code{weights} must be a positive
#' numeric vector of length \code{length(obj_set)}. If not provided,
#' \code{weights} defaults to \code{rep(1, length(obj_set))}.
#'
#' @details
#'
#' **Optimization Form**
#'
#' Let there be K objective terms, each defined as a linear score
#'
#' \deqn{
#'   y_k = a_k^\top x, \quad k = 1, \ldots, K,
#' }
#'
#' where \eqn{a_k} is the coefficient vector constructed by
#' \code{objective_term()}, and x denotes the item–module–panel
#' decision variables.
#'
#' The weighted-sum objective optimizes:
#'
#' \deqn{
#'   \min \; \sum_{k=1}^K w_k \, y_k
#'   \quad \text{or} \quad
#'   \max \; \sum_{k=1}^K w_k \, y_k,
#' }
#'
#' where \eqn{w_k > 0} are user-specified or default weights.
#'
#' **Interpretation**
#'
#' This strategy is appropriate when:
#' \itemize{
#'   \item All objective terms can be meaningfully combined on a common scale, and
#'   \item The test developer is willing to trade off performance across multiple
#'         objectives using explicit weights.
#' }
#'
#' The weighted-sum approach reduces a multi-objective optimization problem
#' to a single linear objective that can be handled directly by standard
#' mixed-integer solvers.
#'
#' This function is automatically invoked by \code{compile_objective()}
#' when \code{strategy = "weighted_sum"}.
#'
#'
#' @return A list of class \code{"compiled_objective"}
#' \describe{
#'   \item{name}{A character vector indicating the specifications in each row of `A_binary`}
#'   \item{specification}{A \code{data.frame} summarizing the constraint specification, including
#'    the requirement name, attribute, constraint type, application level,
#'    operator, and the number of constraint rows generated.}
#'   \item{A_binary}{Sparse matrix of coefficients for binary decision variables.}
#'   \item{A_real}{Sparse matrix of coefficients for real decision variables.}
#'   \item{operators}{A character vector of constraint operators, one per row of `A_binary`.}
#'   \item{d}{A numeric vector of right-hand-side values for the constraints.}
#'   \item{C_binary}{Penalty vector for binary variables (if any)}
#'   \item{C_real}{Penalty vector for real variables (if any)}
#'   \item{sense}{"min" or "max"}
#'   \item{decisionvar_name_new}{Character vector indicating the names of new decision variables, same length as `ncol(A_real)`.}
#'   \item{decisionvar_type_new}{A character vector of `"C"` (continuous), same length as `ncol(A_real)`.}
#'   \item{notes}{List of metadata.}
#' }
#'
#' @keywords internal
#'
compile_weighted_sum_ <- function(x,obj_set,strategy = "weighted_sum",
                                  strategy_args = list()) {
  if (inherits(obj_set, "mstATA_objective") ||
      inherits(obj_set, "compiled_objective")) {
    obj_set <- list(obj_set)
  }

  if (!is.list(obj_set) || length(obj_set) == 0L) {
    stop("'obj_set' must be a non-empty list.", call. = FALSE)
  }


  num_objs <- length(obj_set)
  if (num_objs < 2L) {
    stop("Multiple-term strategies require at least two objectives. ",
         call. = FALSE)
  }

  is_term <- vapply(obj_set, inherits,
                    logical(1L), "mstATA_objective")
  is_comp <- vapply(obj_set, inherits,
                    logical(1L), "compiled_objective")
  if (all(is_term)) {
    input_args <- "objective term"
  }else if (all(is_comp)) {
    input_args <- "compiled objective"
  } else {
    stop(
      "'obj_set' must be either a list of 'mstATA_objective' ",
      "or a list of 'compiled_objective'. Mixing is not allowed.",
      call. = FALSE
    )
  }


  strategy <- match.arg(strategy,"weighted_sum")
  term_goals <- if (input_args == "objective term") {
    lapply(obj_set, `[[`, "goal")
  } else {
    NULL
  }

  strategy_args <- validate_strategy_args(strategy, strategy_args,
                                          n_terms = num_objs,
                                          goals = term_goals)
  weight_vals <- strategy_args$weights

  direction_vec    <- vapply(obj_set, `[[`, character(1), "sense")

  if(input_args == "objective term"){
    term_names <- vapply(obj_set, function(t) t$name,character(1L))
    if (anyDuplicated(term_names)) stop("All objective term names must be unique.")

    decisionvar_name<-x$decisionvar_name
    n_bin<-length(decisionvar_name)

    A_list  <- lapply(obj_set, `[[`, "coef_val")
    goal_list  <- lapply(obj_set, `[[`, "goal")


    rows_per_term <- ifelse(vapply(goal_list, is.null, logical(1)), 1L, 2L)
    n_rows <- sum(rows_per_term)
    nnz_bin <- sum(vapply(A_list, function(a) sum(a != 0), integer(1)) * rows_per_term)

    new_var_name<-character(num_objs)
    new_var_type<-rep("C",num_objs)
    C_real<-numeric(num_objs)
    y_bounds<-matrix(0,nrow = num_objs,ncol = 2)
    rows_i_b <- integer(nnz_bin)
    rows_j_b <- integer(nnz_bin)
    rows_x_b <- numeric(nnz_bin)

    rows_i_r <- integer(n_rows)
    rows_j_r <- integer(n_rows)
    rows_x_r <- numeric(n_rows)

    operators <- character(n_rows)
    rhs       <- numeric(n_rows)
    ConstraintMatrix_name<-character(n_rows)

    row_ptr<-1L
    bin_ptr<-1L
    for(obj_id in seq_len(num_objs)){
      weight_val<-weight_vals[obj_id]
      direction<-direction_vec[obj_id]
      if(direction=="max"){
        C_real[obj_id]<--1L*weight_val
      }else{
        C_real[obj_id]<-1L*weight_val
      }
      new_var_name[obj_id]<-paste0("y",obj_id," ",direction,"value")
      a_vec<-as.vector(A_list[[obj_id]])
      nz_b<-which(a_vec!=0)
      bounds<-estimate_term_bounds(x = x, a_vec = a_vec)
      goal<-goal_list[[obj_id]]
      if(!is.null(goal)){
        idx<-bin_ptr:(bin_ptr+2*length(nz_b)-1)
        rows_i_b[idx]<- rep(c(row_ptr,row_ptr+1L),each=length(nz_b))
        rows_j_b[idx]<-rep(nz_b,2)
        rows_x_b[idx] <-rep(a_vec[nz_b],2)
        bin_ptr<-bin_ptr+2*length(nz_b)

        rows_i_r[row_ptr:(row_ptr+1L)] <- c(row_ptr,row_ptr+1L)
        rows_j_r[row_ptr:(row_ptr+1L)] <- rep(obj_id,2)
        rows_x_r[row_ptr:(row_ptr+1L)] <- c(1L,-1L)

        operators[row_ptr:(row_ptr+1L)]<-c(">=","<=")
        rhs[row_ptr:(row_ptr+1L)]<-c(goal,goal)
        ConstraintMatrix_name[row_ptr:(row_ptr+1L)]<-c(paste0(term_names[obj_id],"(shortfall)"),
                                                       paste0(term_names[obj_id],"(excess)"))

        y_bounds[obj_id,1]<-0
        y_bounds[obj_id,2]<-max(abs(bounds[["lower_bound"]]-goal),abs(bounds[["upper_bound"]]-goal))
        row_ptr<-row_ptr+2L
      }else{
        idx<-bin_ptr:(bin_ptr+length(nz_b)-1)
        rows_i_b[idx]<-row_ptr
        rows_j_b[idx]<- nz_b
        rows_x_b[idx]<-a_vec[nz_b]
        bin_ptr<-bin_ptr+length(nz_b)

        rows_i_r[row_ptr]<-row_ptr
        rows_j_r[row_ptr]<-obj_id
        rows_x_r[row_ptr]<- -1L

        if(direction=="max"){
          operators[row_ptr]<-">="
        }else{
          operators[row_ptr]<-"<="
        }
        rhs[row_ptr]<-0
        ConstraintMatrix_name[row_ptr]<-term_names[obj_id]
        y_bounds[obj_id,]<-bounds
        row_ptr<-row_ptr+1L
      }
    }

    A_binary <-  Matrix::sparseMatrix(i = rows_i_b, j = rows_j_b,
                                      x = rows_x_b,
                                      dims = c(n_rows, n_bin))
    colnames(A_binary)<-colnames(A_list[[1]])

    A_real <- Matrix::sparseMatrix(i = rows_i_r, j = rows_j_r, x = rows_x_r,
                                   dims = c(n_rows, num_objs))
    colnames(A_real)<-new_var_name

    Specification <- do.call(
      rbind,
      lapply(seq_len(num_objs), function(k) {
        parsed <- parse_objective_string(term_names[k])
        if(parsed$attribute_type == "Category"){
          spec<-paste0("Item count from ",parsed$attribute)
        }else{
          spec<-paste0("Sum of ",parsed$attribute)
        }
        n_r <- rows_per_term[k]
        data.frame(
          Requirement = spec,
          Attribute = parsed$attribute,
          Type = parsed$attribute_type,
          `Application Level` = parsed$application_level,
          Operator = parsed$objective_type,
          `Num of Constraints` = n_r,
          stringsAsFactors = FALSE
        )
      })
    )
    colnames(Specification)<-c("Requirement","Attribute","Type","Application Level","Operator","Num of Constraints")
    Specification$`Num of Constraints`<-as.numeric(Specification$`Num of Constraints`)
  }

  if(input_args == "compiled objective"){
    ConstraintMatrix_name <- unlist(lapply(obj_set, `[[`, "name"), use.names = FALSE)
    Specification <- do.call(rbind,lapply(obj_set, `[[`, "specification"))
    A_binary <- Reduce(Matrix::rbind2,
                       lapply(obj_set, `[[`, "A_binary"))
    operators <- unlist(lapply(obj_set, `[[`, "operators"))
    rhs       <- unlist(lapply(obj_set, `[[`, "d"))
    new_var_type <- unlist(lapply(obj_set, `[[`, "decisionvar_type_new"))
    y_bounds<-do.call(rbind,lapply(obj_set,function(t) t$notes$y_bounds))
    new_var_name <- character(0)
    C_real <- numeric()
    A_real_list <- vector("list", num_objs)

    for(comp_obj_id in seq_len(num_objs)){
      comp<-obj_set[[comp_obj_id]]
      local_names<-paste(comp_obj_id,comp$decisionvar_name_new,sep = "_")
      new_var_name<-c(new_var_name,local_names)
      A_real_i<-comp$A_real
      colnames(A_real_i)<-local_names
      A_real_list[[comp_obj_id]]<-A_real_i
      weight_val<-weight_vals[comp_obj_id]
      C_real_i<-comp$C_real
      if(comp$sense=="max"){
        C_real<-c(C_real,-C_real_i*weight_val)
      }else{
        C_real<-c(C_real,C_real_i*weight_val)
      }
    }
    for(comp_obj_id in seq_len(num_objs)){
      A_real_list[[comp_obj_id]]<-align_binary_to_universe(A_real_list[[comp_obj_id]],
                                                           new_var_name)
    }
    A_real <- Reduce(Matrix::rbind2, A_real_list)
  }


  return(create_objective(name = ConstraintMatrix_name,
                          specification = Specification,
                          A_binary = A_binary,
                          A_real   = A_real,
                          C_binary = NULL,
                          C_real   = C_real,
                          operator = operators,
                          d      = rhs,
                          sense = "min",
                          decisionvar_name_new = new_var_name,
                          decisionvar_type_new    = new_var_type,
                          notes  = list(strategy = strategy,
                                        strategy_args = strategy_args,
                                        y_bounds = y_bounds)))
}
