test_that("check_attribute_column correctly identifies and returns attribute types", {
  # Quantitative
  time_attr <- check_attribute_column(test_itempool, "time")
  expect_length(time_attr,nrow(test_itempool))
  expect_equal(attr(time_attr, "attribute_type"), "quantitative")

  # Quantitative with NA warning
  test_itempool_bad <- test_itempool
  test_itempool_bad$time[2] <- NA
  expect_error(check_attribute_column(test_itempool_bad, "time"))

  # Categorical
  content_attr <- check_attribute_column(test_itempool, "content")
  expect_s3_class(content_attr, "factor")
  expect_length(content_attr, nrow(test_itempool))
  expect_false(anyNA(content_attr))
  expect_equal(attr(content_attr, "attribute_type"), "categorical")

  test_itempool_bad$content[1:5]<-NA
  expect_true(anyNA(check_attribute_column(test_itempool_bad, "content")))

  # Missing attribute
  expect_error(check_attribute_column(test_itempool, "iif(theta=0)"))
})

test_that("numeric attribute is returned unchanged with quantitative attribute_type", {
  itempool <- data.frame(a = c(1,2,3))
  out <- check_attribute_column(itempool, "a")

  expect_true(is.numeric(out))
  expect_equal(attr(out, "attribute_type"), "quantitative")
  expect_equal(as.numeric(out),itempool[["a"]])
})

test_that("categorical character attribute becomes factor with sorted levels", {
  itempool <- data.frame(a = c("b", "a", "c"))
  out <- check_attribute_column(itempool, "a")

  expect_true(is.factor(out))
  expect_equal(levels(out), c("a","b","c"))
  expect_equal(attr(out, "attribute_type"), "categorical")
  expect_equal(as.character(out),itempool[["a"]])
})

test_that("categorical factor attribute is re-leveled and kept as categorical", {
  itempool <- data.frame(a = factor(c("high","low","medium"), levels = c("high","medium","low")))
  out <- check_attribute_column(itempool, "a")

  expect_equal(levels(out), c("high","medium","low"))
  expect_equal(attr(out, "attribute_type"), "categorical")
})

test_that("numeric with NAs errors", {
  itempool <- data.frame(a = c(1, NA, 2))
  expect_error(check_attribute_column(itempool, "a"), "Quantitative attribute:")
})

test_that("numeric with Infs errors", {
  itempool <- data.frame(a = c(1, Inf, 2))
  expect_error(check_attribute_column(itempool, "a"), "Quantitative attribute:")
})

test_that("unsupported type errors", {
  itempool <- data.frame(a = as.Date(c("2020-01-01","2020-01-02")))
  expect_error(check_attribute_column(itempool, "a"), "unsupported type")
})
