test_that("convolve_probs computes correct convolution for simple cases", {

  ## p = score distribution for 1 item
  ## q = score distribution for 1 item
  p <- c(0.3, 0.7)   # P(0)=0.3, P(1)=0.7
  q <- c(0.4, 0.6)

  res <- convolve_probs(p, q)

  ## manual convolution:
  ## score 0: 0.3*0.4
  ## score 1: 0.3*0.6 + 0.7*0.4
  ## score 2: 0.7*0.6
  expected <- c(
    0.3 * 0.4,
    0.3 * 0.6 + 0.7 * 0.4,
    0.7 * 0.6
  )

  expect_equal(res, expected, tolerance = 1e-12)
})

test_that("convolve_probs returns correct length", {

  p <- c(0.2, 0.5, 0.3)  # scores 0..2
  q <- c(0.6, 0.4)       # scores 0..1

  res <- convolve_probs(p, q)

  ## length should be (len(p) + len(q) - 1)
  expect_length(res, length(p) + length(q) - 1)
})

test_that("convolve_probs preserves total probability mass", {

  p <- c(0.1, 0.2, 0.7)
  q <- c(0.8, 0.2)

  res <- convolve_probs(p, q)

  expect_equal(sum(res), 1, tolerance = 1e-12)
})

test_that("convolve_probs with degenerate distribution acts as identity", {

  p <- c(1)            # always score 0
  q <- c(0.3, 0.7)

  res <- convolve_probs(p, q)

  expect_equal(res, q, tolerance = 1e-12)
})

test_that("convolve_probs shifts distribution for deterministic score", {

  ## p always contributes score = 2
  p <- c(0, 0, 1)
  q <- c(0.4, 0.6)

  res <- convolve_probs(p, q)

  ## scores 2 and 3
  expected <- c(0, 0, 0.4, 0.6)

  expect_equal(res, expected, tolerance = 1e-12)
})

test_that("convolve_probs errors on non-numeric input", {

  expect_error(
    convolve_probs(p = c("a", "b"), q = c(0.5, 0.5)),
    class = "error"
  )
})

