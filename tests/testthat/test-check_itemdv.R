test_that("check_itemdv correctly identifies x_im columns", {

  x <- mst_design(itempool = data.frame(Item_id=1:10),design = "1-3",
                  pathway_length = 3,
                  item_id_col = "Item_id")
  item_cols <- check_itemdv(x)
  expect_equal(length(item_cols), 10)

  expect_equal(length(item_cols[[1]]), 4)
  expect_equal(length(item_cols[[2]]), 4)
  expect_equal(length(item_cols[[3]]), 4)

  # Check correct pattern for item 1: x[ module , 1 ]
  expect_true(all(grepl("^x\\[[0-9]+,1\\]$",
                        x$decisionvar_name[item_cols[[1]]])))

  # Ensure all detected columns exist in the true decision variable space
  base_cols <- grep("^x\\[[0-9]+,1\\]$", x$decisionvar_name)
  expect_setequal(item_cols[[1]], base_cols)

  item_module_eligibility<-list(`1`=1:nrow(mini_itempool),
                                `2`=which(mini_itempool$difficulty<=0),
                                `4`=which(mini_itempool$difficulty>=0),
                                `5`=intersect(which(mini_itempool$difficulty>=-2),which(mini_itempool$difficulty<0)),
                                `6`=intersect(which(mini_itempool$difficulty>=-1),which(mini_itempool$difficulty<1)),
                                `7`=intersect(which(mini_itempool$difficulty>=0),which(mini_itempool$difficulty<2)))
  x_new<-mst_design(itempool = mini_itempool,design = "1-3-3",
                    module_length = c(6,4,4,4,3,3,3),
                    item_module_eligibility = item_module_eligibility)
  item_cols <- check_itemdv(x_new)
  expect_equal(length(item_cols), nrow(mini_itempool))

  # Expected number of eligible modules per item
  item_module_eligibility_new<-check_item_module_eligibility(item_module_eligibility = item_module_eligibility,
                                                             index_map = setNames(seq(nrow(mini_itempool)),mini_itempool$item_id),
                                                             NumModules = 7)
  eligible_mods <- function(item_id) {
    names(unlist(lapply(item_module_eligibility_new,
                        function(idx) which(idx == item_id))))
  }

  expect_equal(length(item_cols[[1]]), length(eligible_mods(1)))
  expect_equal(length(item_cols[[2]]), length(eligible_mods(2)))
  expect_equal(length(item_cols[[3]]), length(eligible_mods(3)))

  # Pattern check for item 1 again — with eligibility applied
  expect_true(all(grepl("^x\\[[0-9]+,1\\]$",
                        x_new$decisionvar_name[item_cols[[1]]])))
})


