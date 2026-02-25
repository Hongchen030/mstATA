check_all1 <- function(mat, tol = 1e-4) {
  # For each theta column, sum over scores should be ~1
  if (!is.matrix(mat)) return(FALSE)
  cs <- colSums(mat)
  all(is.finite(cs)) && all(abs(cs - 1) <= tol)
}


test_that("analytic_mst_precision preserves probability mass", {
  cuts<-list(c(-0.5,0.5),c(-1,1))
  ItemsInModules<-mini_itempool[1:16,]
  ItemsInModules[,"module_id"]<-c(rep(1,4),rep(c(2,3,4,5,6,7),each=2))
  assembled_panel<-list(`Panel 1` = list(ItemsInModules = ItemsInModules,ItemsInPathways = NULL))
  class(assembled_panel)<-"mstATA_panel"
  out <- analytic_mst_precision_items(
    design = "1-3-3",
    assembled_panel = assembled_panel,
    item_par_cols = list("3PL"=c("discrimination","difficulty","guessing")),
    model_col = "model",
    D = 1.7,
    theta = seq(-5,5,0.1),
    range_tcc = c(-5,5),
    cuts = cuts,
    cut_scale = "theta",
    tol = 1e-8
  )

  ## ---- check cdist_by_mod ----
  expect_true(is.list(out$`Panel 1`$cdist_by_mod))

  for (m in seq_along(out$`Panel 1`$cdist_by_mod)) {

    mat <- out$`Panel 1`$cdist_by_mod[[m]]

    expect_true(is.matrix(mat))

    expect_true(check_all1(mat, tol = 1e-8))
  }

  ## ---- check joint_dist at final stage ----
  NumStage <- 3

  final_joint <- out$`Panel 1`$joint_dist[[NumStage]]
  expect_true(is.list(final_joint))
  mat<-do.call(rbind,final_joint)

  expect_true(
    check_all1(mat, tol = 1e-8),
    info = paste("Column sums of joint_dist[[", NumStage,
                 "]][[", j, "]] do not equal 1"))

})


test_that("TD12",{
  ## TD12 two panels
  out<-analytic_mst_precision_items(design = "1-2",assembled_panel = TD12_panel,
                                            item_par_cols = list("3PL"=c("a","b","c"),
                                                                 "GPCM" = c("alpha","delta1","delta2","delta3"),
                                                                 "GRM" = c("alpha","beta1","beta2")),
                                            model_col = "model",
                                            D = 1,
                                            theta = seq(-3,3,0.1),
                                            range_tcc = c(-5,5),
                                            cuts = list(0),
                                            cut_scale = "theta",
                                            tol = 1e-4)
  ## ---- check cdist_by_mod ----
  expect_true(is.list(out$`Panel_1`$cdist_by_mod))
  expect_true(is.list(out$`Panel_2`$cdist_by_mod))


  for (m in seq_along(out$`Panel_1`$cdist_by_mod)) {

    mat <- out$`Panel_1`$cdist_by_mod[[m]]

    expect_true(is.matrix(mat),
                info = paste("cdist_by_mod[[", m, "]] is not a matrix"))

    expect_true(
      check_all1(mat, tol = 1e-8),
      info = paste("Column sums of cdist_by_mod[[", m, "]] do not equal 1")
    )
  }

  ## ---- check joint_dist at final stage ----
  NumStage <- 2

  final_joint <- out$`Panel_1`$joint_dist[[NumStage]]
  expect_true(is.list(final_joint))
  mat<-do.call(rbind,final_joint)

  expect_true(
    check_all1(mat, tol = 1e-8),
    info = paste("Column sums of joint_dist[[", NumStage,
                 "]][[", j, "]] do not equal 1"))

})

test_that("TD123",{
  ## TD12 two panels
  out<-analytic_mst_precision_items(design = "1-2-3",assembled_panel = TD123_panel,
                                    item_par_cols = list("3PL"=c("a","b","c"),
                                                         "GPCM" = c("alpha","delta1","delta2","delta3"),
                                                         "GRM" = c("alpha","beta1","beta2")),
                                    model_col = "model",
                                    D = 1,
                                    theta = seq(-3,3,0.1),
                                    range_tcc = c(-5,5),
                                    cuts = list(0,c(-0.43,0.43)),
                                    cut_scale = "theta",
                                    tol = 1e-4)
  ## ---- check cdist_by_mod ----
  expect_true(is.list(out$`Panel_1`$cdist_by_mod))
  expect_true(is.list(out$`Panel_2`$cdist_by_mod))


  for (m in seq_along(out$`Panel_1`$cdist_by_mod)) {

    mat <- out$`Panel_1`$cdist_by_mod[[m]]

    expect_true(is.matrix(mat),
                info = paste("cdist_by_mod[[", m, "]] is not a matrix"))

    expect_true(
      check_all1(mat, tol = 1e-8),
      info = paste("Column sums of cdist_by_mod[[", m, "]] do not equal 1")
    )
  }

  ## ---- check joint_dist at final stage ----
  NumStage <- 3

  final_joint <- out$`Panel_1`$joint_dist[[NumStage]]
  expect_true(is.list(final_joint))
  mat<-do.call(rbind,final_joint)

  expect_true(check_all1(mat, tol = 1e-8))

})


