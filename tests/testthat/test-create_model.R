test_that("create_model returns an mstATA_model object for valid inputs", {

  A_binary <- Matrix::Matrix(
    c(1, 0,
      0, 1),
    nrow = 2,
    sparse = TRUE
  )

  A_real <- Matrix::Matrix(
    c(1,
      1),
    nrow = 2,
    sparse = TRUE
  )

  model <- create_model(
    name               = c("c1", "c2"),
    specification      = data.frame(`Num of Constraints` = c(1, 1)),
    A_binary           = A_binary,
    A_real             = A_real,
    C_binary           = c(1, 1),
    C_real             = c(0.5),
    operators          = c("<=", "<="),
    d                = c(1, 1),
    decisionvar_name   = c("x1", "x2", "y1"),
    decisionvar_type   = c("B", "B", "C"),
    sense              = "max",
    lb_bound           = c(0),
    ub_bound           = c(10)
  )

  expect_s3_class(model, "mstATA_model")
  expect_equal(model$sense, "max")
  expect_equal(length(model$operators), 2)
})


test_that("create_model errors if A_binary is not a sparse Matrix", {

  expect_error(
    create_model(
      name               = "c1",
      specification      = NULL,
      A_binary           = matrix(1),
      A_real             = NULL,
      C_binary           = 1,
      C_real             = numeric(0),
      operators          = "<=",
      d                = 1,
      decisionvar_name   = "x1",
      decisionvar_type   = "B",
      sense              = "min",
      lb_bound           = numeric(0),
      ub_bound           = numeric(0)
    )
  )
})

test_that("create_model errors when nrow(A_binary) != length(d)", {

  A_binary <- Matrix::Matrix(1, nrow = 2, sparse = TRUE)

  expect_error(
    create_model(
      name               = c("c1", "c2"),
      specification      = NULL,
      A_binary           = A_binary,
      A_real             = NULL,
      C_binary           = 1,
      C_real             = numeric(0),
      operators          = "<=",
      d                = 1,  # mismatch
      decisionvar_name   = "x1",
      decisionvar_type   = "B",
      sense              = "min",
      lb_bound           = numeric(0),
      ub_bound           = numeric(0)
    )
  )
})

test_that("create_model errors for invalid decision variable types", {

  A_binary <- Matrix::Matrix(1, nrow = 1, sparse = TRUE)

  expect_error(
    create_model(
      name               = "c1",
      specification      = NULL,
      A_binary           = A_binary,
      A_real             = NULL,
      C_binary           = 1,
      C_real             = numeric(0),
      operators          = "<=",
      d                = 1,
      decisionvar_name   = "x1",
      decisionvar_type   = "Z",  # invalid
      sense              = "min",
      lb_bound           = numeric(0),
      ub_bound           = numeric(0)
    )
  )
})

test_that("create_model errors for invalid sense", {

  A_binary <- Matrix::Matrix(1, nrow = 1, sparse = TRUE)

  expect_error(
    create_model(
      name               = "c1",
      specification      = NULL,
      A_binary           = A_binary,
      A_real             = NULL,
      C_binary           = 1,
      C_real             = numeric(0),
      operators          = "<=",
      d                = 1,
      decisionvar_name   = "x1",
      decisionvar_type   = "B",
      sense              = "maximize",  # invalid
      lb_bound           = numeric(0),
      ub_bound           = numeric(0)
    )
  )
})

test_that("create_model errors if specification Num of Constraints does not match", {

  A_binary <- Matrix::Matrix(
    c(1, 0,
      0, 1),
    nrow = 2,
    sparse = TRUE
  )

  expect_error(
    create_model(
      name               = c("c1", "c2"),
      specification      = data.frame(`Num of Constraints` = c(1, 2),check.names = FALSE), # sum = 3
      A_binary           = A_binary,
      A_real             = NULL,
      C_binary           = c(1, 1),
      C_real             = numeric(0),
      operators          = c("<=", "<="),
      d                = c(1, 1),
      decisionvar_name   = c("x1", "x2"),
      decisionvar_type   = c("B", "B"),
      sense              = "min",
      lb_bound           = numeric(0),
      ub_bound           = numeric(0)
    )
  )
})
