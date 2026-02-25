test_that("numeric attribute without NA returns numeric values", {
  itempool <- data.frame(a = seq(0.5,1.5,length.out = 100))
  out <- get_attribute_val(itempool, "a")
  expect_equal(out, seq(0.5,1.5,length.out = 100))
})


test_that("character attribute coercible to numeric returns numeric", {
  itempool <- data.frame(a = c("1", "2", "3"))
  out <- get_attribute_val(itempool, "a")
  expect_equal(out, c(1, 2, 3))
})

test_that("non-numeric non-coercible character attribute triggers error", {
  itempool <- data.frame(a = c("x", "y", "z"))

  expect_error(get_attribute_val(itempool, "a"))
})

test_that("level must be single non-NA value", {
  itempool <- data.frame(a = c("A", "B", "C"))

  expect_error(
    get_attribute_val(itempool, "a", cat_level = c("A", "B")),
    "'cat_level' must be a single non-NA value"
  )

  expect_error(
    get_attribute_val(itempool, "a", cat_level = NA),
    "'cat_level' must be a single non-NA value"
  )
})

test_that("categorical attribute: level not present triggers error", {
  itempool <- data.frame(a = c("A", "B", "C"))

  expect_error(
    get_attribute_val(itempool, "a", cat_level = "D"),
    "is not present in attribute"
  )
})

test_that("categorical attribute: indicator vector correct", {
  itempool <- data.frame(a = c("A", "B", "A", "C"))
  out <- get_attribute_val(itempool, "a", cat_level = "A")

  expect_equal(out, c(1L, 0L, 1L, 0L))
})

test_that("categorical attribute: NA values produce message and treated as FALSE", {
  itempool <- data.frame(a = c("A", NA, "B"))

  expect_message(
    out <- get_attribute_val(itempool, "a", cat_level = "A"),
    "Item 2 attribute value is NA"
  )

  expect_equal(out, c(1L, 0L, 0L))
})
