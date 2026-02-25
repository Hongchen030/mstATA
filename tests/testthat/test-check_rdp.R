ModuleIndex_133 <- data.frame(
  module_index = 1:7,
  stage = c(1, 2, 2, 2, 3, 3, 3)
)

NumStages_133 <- 3

test_that("check_rdp accepts valid rdp specification", {

  rdp <- list(
    c(-0.5, 0.5),  # stage 1 -> 3 modules
    c(-1, 1)       # stage 2 -> 3 modules
  )

  out <- check_rdp(
    rdp = rdp,
    ModuleIndex = ModuleIndex_133,
    NumStages = NumStages_133
  )

  expect_equal(out, rdp)
})

test_that("check_rdp allows NULL rdp", {

  out <- check_rdp(
    rdp = NULL,
    ModuleIndex = ModuleIndex_133,
    NumStages = NumStages_133
  )

  expect_null(out)
})

test_that("check_rdp errors when rdp is not a list", {

  expect_error(
    check_rdp(
      rdp = c(-0.5, 0.5),
      ModuleIndex = ModuleIndex_133,
      NumStages = NumStages_133
    ),
    "must be a list"
  )
})

test_that("check_rdp errors when rdp has wrong length", {

  rdp <- list(c(-0.5, 0.5))  # should be length 2

  expect_error(
    check_rdp(
      rdp = rdp,
      ModuleIndex = ModuleIndex_133,
      NumStages = NumStages_133
    ),
    "length"
  )
})

test_that("check_rdp errors when rdp elements are non-numeric", {

  rdp <- list(
    c(-0.5, 0.5),
    c("low", "high")
  )

  expect_error(
    check_rdp(
      rdp = rdp,
      ModuleIndex = ModuleIndex_133,
      NumStages = NumStages_133
    ),
    "numeric"
  )
})

test_that("check_rdp errors when rdp contains NA", {

  rdp <- list(
    c(-0.5, NA),
    c(-1, 1)
  )

  expect_error(
    check_rdp(
      rdp = rdp,
      ModuleIndex = ModuleIndex_133,
      NumStages = NumStages_133
    ),
    "missing"
  )
})

test_that("check_rdp errors when rdp contains infinite values", {

  rdp <- list(
    c(-0.5, 0.5),
    c(-Inf, Inf)
  )

  expect_error(
    check_rdp(
      rdp = rdp,
      ModuleIndex = ModuleIndex_133,
      NumStages = NumStages_133
    ),
    "finite"
  )
})

test_that("check_rdp errors when rdp[[s]] has incorrect length", {

  rdp <- list(
    c(-0.5),       # should be length 2
    c(-1, 1)
  )

  expect_error(
    check_rdp(
      rdp = rdp,
      ModuleIndex = ModuleIndex_133,
      NumStages = NumStages_133
    ),
    "requires"
  )
})

test_that("check_rdp error message identifies the offending stage", {

  rdp <- list(
    c(-0.5, 0.5),
    c(-1)   # wrong length for stage 2
  )

  expect_error(
    check_rdp(
      rdp = rdp,
      ModuleIndex = ModuleIndex_133,
      NumStages = NumStages_133
    ),
    "stage 2"
  )
})


