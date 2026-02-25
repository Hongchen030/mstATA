test_that("check_nrCat_consistency passes for correct nrCat", {

  ## Dichotomous
  params <- data.frame(a = 1, b = 0)
  expect_silent(
    check_nrCat_consistency("2PL", params, nrCat = 2)
  )

  ## GRM: a + 2 thresholds -> 3 categories
  params <- mstR::genPolyMatrix(items = 10,nrCat = 3,model = "GRM",same.nrCat = FALSE)
  for(i in seq_len(10)){
    expect_silent(
      check_nrCat_consistency("GRM", params[i,], nrCat = expected_nrCat("GRM",params[i,]))
    )
  }
  # MGRM
  params <- mstR::genPolyMatrix(items = 10,nrCat = 3,model = "MGRM")
  for(i in seq_len(10)){
    expect_silent(
      check_nrCat_consistency("MGRM", params[i,], nrCat = expected_nrCat("MGRM",params[i,]))
    )
  }
  #PCM
  params <- mstR::genPolyMatrix(items = 10,nrCat = 3,model = "PCM",same.nrCat = FALSE)
  for(i in seq_len(10)){
    expect_silent(
      check_nrCat_consistency("PCM", params[i,], nrCat = expected_nrCat("PCM",params[i,]))
    )
  }
  # GPCM
  params <- mstR::genPolyMatrix(items = 10,nrCat = 3,model = "GPCM",same.nrCat = FALSE)
  for(i in seq_len(10)){
    expect_silent(
      check_nrCat_consistency("GPCM", params[i,], nrCat = expected_nrCat("GPCM",params[i,]))
    )
  }
  # RSM
  params <- mstR::genPolyMatrix(items = 10,nrCat = 3,model = "RSM",same.nrCat = FALSE)
  for(i in seq_len(10)){
    expect_silent(
      check_nrCat_consistency("RSM", params[i,], nrCat = expected_nrCat("RSM",params[i,]))
    )
  }
  # NRM
  params <- mstR::genPolyMatrix(items = 10,nrCat = 3,model = "NRM",same.nrCat = FALSE)
  for(i in seq_len(10)){
    expect_silent(
      check_nrCat_consistency("NRM", params[i,], nrCat = expected_nrCat("NRM",params[i,]))
    )
  }

})


test_that("check_nrCat_consistency errors on mismatched nrCat", {

  params <- data.frame(a = 1, b = 0)

  expect_error(
    check_nrCat_consistency("2PL", params, nrCat = 3),
    "Inconsistent nrCat"
  )
})


test_that("check_nrCat_consistency errors on invalid nrCat input", {

  params <- data.frame(a = 1, b = 0)

  expect_error(
    check_nrCat_consistency("2PL", params, nrCat = NA),
    "nrCat"
  )

  expect_error(
    check_nrCat_consistency("2PL", params, nrCat = c(2, 3)),
    "nrCat"
  )

  expect_error(
    check_nrCat_consistency("2PL", params, nrCat = "two"),
    "nrCat"
  )
})


test_that("check_nrCat_consistency is case-insensitive for model names", {

  params <- data.frame(a = 1, b = 0)

  expect_silent(
    check_nrCat_consistency("rasch", params, nrCat = 2)
  )

  expect_silent(
    check_nrCat_consistency("GrM",
                            data.frame(a = 1, b1 = -1, b2 = 1),
                            nrCat = 3)
  )
})


test_that("check_nrCat_consistency returns invisibly TRUE on success", {

  params <- data.frame(a = 1, b = 0)

  res <- check_nrCat_consistency("2PL", params, nrCat = 2)

  expect_true(isTRUE(res))
})


test_that("check_nrCat_consistency ignores NULL nrCat", {

  params <- data.frame(a = 1, b = 0)

  expect_silent(
    check_nrCat_consistency("2PL", params, nrCat = NULL)
  )
})

