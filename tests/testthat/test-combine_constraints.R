test_that("combine_constraints() errors when any element is not an mstATA_constraint", {

  c1 <- structure(list(A_binary = Matrix::Matrix(0, 1, 1),
                       A_real = NULL,
                       operators = ">=",
                       d = 1,
                       decisionvar_name = "x1",
                       name = "c1"),
                  class = "mstATA_constraint")

  bad <- list(A_binary = Matrix::Matrix(0,1,1), name = "bad")

  expect_error(
    combine_constraints(list(c1, bad)),
    "All elements of 'constraints' must be of class 'mstATA_constraint'.",
    fixed = TRUE
  )
})


test_that("combine_constraints() combines two valid constraints", {
  specification<-data.frame(matrix(NA,1,6))
  colnames(specification)  <-c("Requirement","Attribute","Type","Application Level",
                               "Operator","Num of Constraints")
  specification$`Num of Constraints`<-1
  # --- Construct first constraint ------------------------------------------
  A1 <- Matrix::sparseMatrix(i = 1, j = 1, x = 1,
                             dims = c(1, 2),
                             dimnames = list(NULL, c("x1", "x2")))
  c1 <- create_constraint(
    name          = "c1",
    specification = specification,
    A_binary      = A1,
    A_real        = NULL,
    operators     = "<=",
    d             = 1,
    C_binary      = NULL,
    C_real        = NULL
  )

  # --- Construct second constraint -----------------------------------------
  A2 <- Matrix::sparseMatrix(i = 1, j = 2, x = 2,
                             dims = c(1, 2),
                             dimnames = list(NULL, c("x1", "x2")))
  c2 <- create_constraint(
    name          = "c2",
    specification = specification,
    A_binary      = A2,
    A_real        = NULL,
    operators     = ">=",
    d             = 2,
    C_binary      = NULL,
    C_real        = NULL
  )

  # --- Combine ---------------------------------------------------------------
  combined <- combine_constraints(list(c1, c2))

  # --- Tests -----------------------------------------------------------------

  # result class
  expect_s3_class(combined, "mstATA_constraint")

  # two rows
  expect_equal(nrow(combined$A_binary), 2)

  # same column names and order
  expect_equal(colnames(combined$A_binary), c("x1", "x2"))

  # first row should be A1, second row A2
  expect_equal(combined$A_binary[1, ], A1[1, ])
  expect_equal(combined$A_binary[2, ], A2[1, ])

  # operators aligned
  expect_equal(combined$operators, c("<=", ">="))

  # d aligned
  expect_equal(combined$d, c(1, 2))

  # names aligned
  expect_equal(combined$name, c("c1", "c2"))

})

test_that("align decision variables", {

  test_mstATA <- mst_design(itempool = mini_itempool,design = "1-3-3",
                            module_length = c(14,12,12,12,6,6,6),
                            pivot_stim_map = NULL)
  discrete_con<-test_itemcat_con(x = test_mstATA,attribute = "itemtype",cat_levels = "TEI",target_num = 2,
                                 operator = "=",which_module = 1)
  solution_con<-combine_constraints(list(solution_itemcount_con(x = test_mstATA,operator = ">=",40),
                                         solution_itemcount_con(x = test_mstATA,operator = "<=",50)))

  combined<-combine_constraints(list(discrete_con,solution_con))
  PoolSize<-nrow(mini_itempool)
  expect_equal(colnames(combined$A_binary),c(paste0("x[1,",seq_len(PoolSize),"]"),
                                             paste0("x[2,",seq_len(PoolSize),"]"),
                                             paste0("x[3,",seq_len(PoolSize),"]"),
                                             paste0("x[4,",seq_len(PoolSize),"]"),
                                             paste0("x[5,",seq_len(PoolSize),"]"),
                                             paste0("x[6,",seq_len(PoolSize),"]"),
                                             paste0("x[7,",seq_len(PoolSize),"]"),
                                             paste0("s[",seq_len(PoolSize),"]")))
  A_binary<-combined$A_binary
  expect_equal(nrow(A_binary),3)
  expect_equal(ncol(A_binary),7*PoolSize+PoolSize)
  A_real<-combined$A_real
  expect_null(A_real)
  expect_true(all(as.vector(as.matrix(A_binary[1,paste0("s[",seq_len(PoolSize),"]")]))==0))
  expect_true(all(as.vector(as.matrix(A_binary[2:3,paste0("s[",seq_len(PoolSize),"]")]))==1))
  c_A<-rbind(cbind(discrete_con$A_binary,Matrix::Matrix(0,nrow = 1,ncol = PoolSize,sparse = TRUE)),
             cbind(Matrix::Matrix(0,nrow = 2,ncol = PoolSize*7,sparse = TRUE),solution_con$A_binary))
  colnames(c_A)<-colnames(A_binary)
  expect_equal(A_binary,c_A)
})
