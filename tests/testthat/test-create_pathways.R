test_that("create_pathways() works for 1-3 MST design", {
  result <- create_pathways("1-3")
  modules <- result$Modules
  pathways <- result$Pathways
  # Check returned structure
  expect_true(is.list(result))
  expect_true(all(c("Modules", "Pathways") %in% names(result)))
  # Check Modules
  expect_true(all(c("stage", "module", "module_index") %in% names(modules)))
  expect_equal(nrow(modules), 4)
  # Check Pathways
  expect_true(all(c("stage1", "stage2", "pathway", "allowed", "pathway_label") %in% names(pathways)))
  expect_equal(nrow(pathways), 3)
  expect_equal(sum(pathways$allowed), 3)
  # Check labeling is consistent with DiffLevels = default for 3
  expect_equal(as.character(pathways$pathway_label), c("M-E", "M-M", "M-H"))
})


test_that("create_pathways() works for 1-3-5 MST design", {
  result <- create_pathways("1-3-5")
  modules <- result$Modules
  pathways <- result$Pathways
  # Check returned structure
  expect_true(is.list(result))
  expect_true(all(c("Modules", "Pathways") %in% names(result)))
  # Check Modules
  expect_true(all(c("stage", "module", "module_index") %in% names(modules)))
  expect_equal(nrow(modules), 9)
  # Check Pathways
  expect_true(all(c("stage1", "stage2","stage3", "pathway", "allowed", "pathway_label") %in% names(pathways)))
  expect_equal(nrow(pathways), 15)
})


test_that("create_pathways() gives a message for stage > 5 (1-3-6 MST design)", {
  expect_message({
    result <- create_pathways("1-3-6")
  }, "Default diff_levels only cover designs with le 5 modules per stage.",fixed=TRUE)
  modules <- result$Modules
  pathways <- result$Pathways

  # Should not contain any *_label columns
  expect_false(any(grepl("_label", names(pathways))))
  # Should include default columns
  expect_true(all(c("stage1", "stage2", "stage3", "pathway", "allowed") %in% names(pathways)))
  expect_equal(nrow(pathways), 18)
})

test_that("exclude_pathways correctly flags pathways", {
  res <- create_pathways("2-2")

  ## all possible pathways
  expect_equal(
    sort(res$Pathways$pathway),
    c("1-1", "1-2", "2-1", "2-2")
  )

  res2 <- create_pathways("2-2", exclude_pathways = c("1-2", "2-1"))

  expect_true(res2$Pathways$allowed[res2$Pathways$pathway == "1-1"])
  expect_false(res2$Pathways$allowed[res2$Pathways$pathway == "1-2"])
  expect_false(res2$Pathways$allowed[res2$Pathways$pathway == "2-1"])
  expect_true(res2$Pathways$allowed[res2$Pathways$pathway == "2-2"])
})

test_that("exclude_pathways errors on invalid pathways", {
  expect_error(
    create_pathways("2-2", exclude_pathways = c("1-3")),
    "Invalid pathway"
  )
})
