test_that("expected_nrCat returns 2 for all dichotomous models", {

  params <- data.frame(a = 1, b = 0)

  expect_equal(expected_nrCat("1PL",   params), 2L)
  expect_equal(expected_nrCat("Rasch", params), 2L)
  expect_equal(expected_nrCat("2PL",   params), 2L)
  expect_equal(expected_nrCat("3PL",   cbind(params, g = 0.2)), 2L)
  expect_equal(expected_nrCat("4PL",   cbind(params, g = 0.2, d = 0.9)), 2L)
})

test_that("expected_nrCat returns 2 for all dichotomous models", {

  params <- mstR::genDichoMatrix(items = 1,model = "1PL")
  expect_equal(expected_nrCat("1PL",   params), 2L)
  params <- mstR::genDichoMatrix(items = 1,model = "2PL")
  expect_equal(expected_nrCat("2PL",   params), 2L)
  params <- mstR::genDichoMatrix(items = 1,model = "3PL")
  expect_equal(expected_nrCat("3PL",   params), 2L)
  params <- mstR::genDichoMatrix(items = 1,model = "4PL")
  expect_equal(expected_nrCat("4PL",   params), 2L)
})



test_that("expected_nrCat handles GRM correctly", {

  ## a + 2 thresholds -> 3 categories
  params <- data.frame(a = 1, b1 = -1, b2 = 1)
  expect_equal(expected_nrCat("GRM", params), 3L)

  ## insufficient parameters
  params_bad <- data.frame(a = 1)
  expect_error(
    expected_nrCat("GRM", params_bad),
    "requires at least 2 parameters"
  )
})

test_that("expected_nrCat handles GRM correctly", {
  params <- mstR::genPolyMatrix(items = 10,nrCat = 3,model = "GRM",same.nrCat = FALSE)
  expect_equal(max(vapply(1:10,FUN = function(i) expected_nrCat("GRM", params[i,]),
                          FUN.VALUE = integer(1))), 3L)
  params <- mstR::genPolyMatrix(items = 10,nrCat = 4,model = "GRM",same.nrCat = FALSE)
  expect_equal(max(vapply(1:10,FUN = function(i) expected_nrCat("GRM", params[i,]),
                          FUN.VALUE = integer(1))), 4L)
  params <- mstR::genPolyMatrix(items = 10,nrCat = 5,model = "GRM",same.nrCat = FALSE)
  expect_equal(max(vapply(1:10,FUN = function(i) expected_nrCat("GRM", params[i,]),
                          FUN.VALUE = integer(1))), 5L)
})

test_that("expected_nrCat handles MGRM correctly", {
  params <- mstR::genPolyMatrix(items = 10,nrCat = 3,model = "MGRM")
  expect_equal(max(vapply(1:10,FUN = function(i) expected_nrCat("MGRM", params[i,]),
                          FUN.VALUE = integer(1))), 3L)
  params <- mstR::genPolyMatrix(items = 10,nrCat = 4,model = "MGRM")
  expect_equal(max(vapply(1:10,FUN = function(i) expected_nrCat("MGRM", params[i,]),
                          FUN.VALUE = integer(1))), 4L)
  params <- mstR::genPolyMatrix(items = 10,nrCat = 5,model = "MGRM")
  expect_equal(max(vapply(1:10,FUN = function(i) expected_nrCat("MGRM", params[i,]),
                          FUN.VALUE = integer(1))), 5L)
})


test_that("expected_nrCat handles PCM correctly", {

  ## 2 step parameters -> 3 categories
  params <- data.frame(b1 = -1, b2 = 1)
  expect_equal(expected_nrCat("PCM", params), 3L)
})

test_that("expected_nrCat handles PCM correctly", {
  params <- mstR::genPolyMatrix(items = 10,nrCat = 3,model = "PCM",same.nrCat = FALSE)
  expect_equal(max(vapply(1:10,FUN = function(i) expected_nrCat("PCM", params[i,]),
                          FUN.VALUE = integer(1))), 3L)
  params <- mstR::genPolyMatrix(items = 10,nrCat = 4,model = "PCM",same.nrCat = FALSE)
  expect_equal(max(vapply(1:10,FUN = function(i) expected_nrCat("PCM", params[i,]),
                          FUN.VALUE = integer(1))), 4L)
  params <- mstR::genPolyMatrix(items = 10,nrCat = 5,model = "PCM",same.nrCat = FALSE)
  expect_equal(max(vapply(1:10,FUN = function(i) expected_nrCat("PCM", params[i,]),
                          FUN.VALUE = integer(1))), 5L)
})


test_that("expected_nrCat handles GPCM correctly", {

  ## a + 2 steps -> 3 categories
  params <- data.frame(a = 1, b1 = -1, b2 = 1)
  expect_equal(expected_nrCat("GPCM", params), 3L)

  ## missing discrimination
  params_bad <- data.frame(b1 = -1)
  expect_error(
    expected_nrCat("GPCM", params_bad),
    "requires at least 2 parameters"
  )
})


test_that("expected_nrCat handles GPCM correctly", {
  params <- mstR::genPolyMatrix(items = 10,nrCat = 3,model = "GPCM",same.nrCat = FALSE)
  expect_equal(max(vapply(1:10,FUN = function(i) expected_nrCat("GPCM", params[i,]),
                          FUN.VALUE = integer(1))), 3L)
  params <- mstR::genPolyMatrix(items = 10,nrCat = 4,model = "GPCM",same.nrCat = FALSE)
  expect_equal(max(vapply(1:10,FUN = function(i) expected_nrCat("GPCM", params[i,]),
                          FUN.VALUE = integer(1))), 4L)
  params <- mstR::genPolyMatrix(items = 10,nrCat = 5,model = "GPCM",same.nrCat = FALSE)
  expect_equal(max(vapply(1:10,FUN = function(i) expected_nrCat("GPCM", params[i,]),
                          FUN.VALUE = integer(1))), 5L)
})

test_that("expected_nrCat handles RSM correctly", {

  ## b + 2 steps -> 3 categories
  params <- data.frame(b = 0, s1 = -1, s2 = 1)
  expect_equal(expected_nrCat("RSM", params), 3L)

  ## insufficient parameters
  params_bad <- data.frame(b = 0)
  expect_error(
    expected_nrCat("RSM", params_bad),
    "requires at least 2 parameters"
  )
})
test_that("expected_nrCat handles RSM correctly", {
  params <- mstR::genPolyMatrix(items = 10,nrCat = 3,model = "RSM")
  expect_equal(max(vapply(1:10,FUN = function(i) expected_nrCat("RSM", params[i,]),
                          FUN.VALUE = integer(1))), 3L)
  params <- mstR::genPolyMatrix(items = 10,nrCat = 4,model = "RSM")
  expect_equal(max(vapply(1:10,FUN = function(i) expected_nrCat("RSM", params[i,]),
                          FUN.VALUE = integer(1))), 4L)
  params <- mstR::genPolyMatrix(items = 10,nrCat = 5,model = "RSM")
  expect_equal(max(vapply(1:10,FUN = function(i) expected_nrCat("RSM", params[i,]),
                          FUN.VALUE = integer(1))), 5L)
})


test_that("expected_nrCat handles NRM correctly", {

  ## (a1,b1),(a2,b2) -> 3 categories
  params <- data.frame(a1 = 1, b1 = -1, a2 = 1, b2 = 1)
  expect_equal(expected_nrCat("NRM", params), 3L)

  ## odd number of parameters
  params_bad <- data.frame(a1 = 1, b1 = 0, a2 = 1)
  expect_error(
    expected_nrCat("NRM", params_bad),
    "even number of parameters"
  )
})

test_that("expected_nrCat handles NRM correctly", {
  params <- mstR::genPolyMatrix(items = 10,nrCat = 3,model = "NRM",same.nrCat = FALSE)
  expect_equal(max(vapply(1:10,FUN = function(i) expected_nrCat("NRM", params[i,]),
                          FUN.VALUE = integer(1))), 3L)
  params <- mstR::genPolyMatrix(items = 10,nrCat = 4,model = "NRM",same.nrCat = FALSE)
  expect_equal(max(vapply(1:10,FUN = function(i) expected_nrCat("NRM", params[i,]),
                          FUN.VALUE = integer(1))), 4L)
  params <- mstR::genPolyMatrix(items = 10,nrCat = 5,model = "NRM",same.nrCat = FALSE)
  expect_equal(max(vapply(1:10,FUN = function(i) expected_nrCat("NRM", params[i,]),
                          FUN.VALUE = integer(1))), 5L)
})

test_that("expected_nrCat errors on unsupported model", {

  params <- data.frame(a = 1, b = 0)

  expect_error(
    expected_nrCat("UNKNOWN", params),
    "Unsupported model"
  )
})


test_that("expected_nrCat errors on non-numeric parameters", {

  params <- data.frame(a = "high", b = "low")

  expect_error(
    expected_nrCat("2PL", params),
    "must be numeric"
  )
})


test_that("expected_nrCat is case-insensitive for model names", {

  params <- data.frame(a = 1, b = 0)

  expect_equal(expected_nrCat("rasch", params), 2L)
  expect_equal(expected_nrCat("GrM",   cbind(params, b2 = 1)), 3L)
})
