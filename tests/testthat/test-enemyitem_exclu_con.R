test_that("enemyitem_exclu_con errors when x is not mstATA_design", {
  expect_error(
    enemyitem_exclu_con(
      x = list()),
    "must be an object of class 'mstATA_design'"
  )
})


test_that("enemyitem_exclu_con produces correct exclusion pairs", {

  itempool <- data.frame(
    item_id = c("A", "B", "C"),
    enemy_items = c("B,C", "A,C", "A,B")
  )

  enemyitem_set<-create_enemy_sets(itempool$item_id, itempool$enemy_items)
  x <- mst_design(itempool = itempool, design = "1-3", pathway_length = 10,
                  enemyitem_set = enemyitem_set)
  res <- enemyitem_exclu_con(x = x)

  NumPathways<-x$NumPathways
  NumSets<-1
  NumStages<-2
  expect_equal(apply(res$A_binary,1,sum),rep(3*NumStages,NumSets*NumPathways))

  expect_equal(nrow(res$A_binary), NumPathways*NumSets)

  # Operator: enemy exclusion is usually <= 1
  expect_equal(res$operators, rep("<=",NumPathways*NumSets))
  expect_equal(res$d, rep(1,NumPathways*NumSets))

  # Correct specification
  spec <- res$specification
  expect_equal(spec$Type[1], "Logical")
  expect_equal(spec$`Application Level`[1], "Pathway-level")
  expect_equal(spec$Attribute, "Enemy items membership")
  expect_equal(spec$`Num of Constraints`, NumSets*NumPathways)
  expect_equal(res$name,
               c(paste("Do not include A, B, C together in Pathway",1:NumPathways)))
})


