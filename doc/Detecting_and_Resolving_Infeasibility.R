## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.5
)
library(knitr)
library(kableExtra)

## ----echo=FALSE---------------------------------------------------------------
testing3 <- data.frame(
  Specification = c(
    "MST structure",
    
    "Item exposure control",
    
    "TEI",
    "Response time",
    "TIF threshold","",
    
    "Stimulus num and type","","",
    "Content","DOK","Mean difficulty",
    
    "Item count in a selected stimulus","",
    "TEI item count in a selected stimulus",
    "No enemy items",

    "Stimulus complexity score",
    
    "capped maximin"
  ),
  Description = c(
    "MST 1-2-2, each module contains 12 items. RDP: 0",
    
    "Unique items are used across modules in the panel.",
    
    "Each pathway has at most 2 TEI items.",
    "The total response time per pathway should be between 20 and 40 minutes.",
    "Minimum TIF values for the easier pathways (S1R-S2E-S3E and S1R-S2H-S3E): TIF values must be at least 10 at $\\theta$ = -1.15, -0.67 and -0.32",
    "Minimum TIF values for the harder pathways (S1R-S2E-S3H and S1R-S2H-S3H): TIF values must be at least 10 at $\\theta$ = 0.32, 0.67 and 1.15",
    
    "S1R module does not include any stimuli",
    "S2E and S2H each contain 1 text-based stimulus.",
    "S3E and S3H each contain 1 graphic-based stimulus.",
    "Min and Max number of items (4 content areas) per module: (2,4)",
    "Min and Max number of items for DOK 1–3 per module: (3,5)",
    "S2E: (-0.55,-0.45), S2H: (0.45,0.55), S3E: (-1.05,-0.95), S3H: (0.95,1.05)",
    
    "S1R module does not have items associated with any stimulus.",
    "At least 4 items conditional on the selection of a stimulus in S2E, S2H, S3E and S3H.",
    "At most 1 TEI item conditional on a selected stimulus in S2E, S2H, S3E and S3H.",
     "Items from the same enemy group cannot appear in the same pathway.",

    "The complexity score from selected stimulus should be between 4.5 and 8.5.",

    "$\\min\\; d \\quad \\text{s.t. }\\; \\lvert y_k - 5 \\rvert \\le d$"
  ),
  `Num of Constraints` = c(9,1000,4,8,6,6,
                           2,2,2,40,30,8,
                           30,240,120,40,
                           240,
                           6)
)
colnames(testing3)<-c("Specification","Description","Num of Constraints")

kable(
  testing3,
  booktabs = TRUE,
  escape = FALSE,
  caption = "Specifications for MST 1-2-2 with discrete and stimulus-based items"
) |>
  kable_styling(
    latex_options = c("hold_position"),
    full_width = FALSE,
    font_size = 10,
    position = "center"
  ) |>
  column_spec(1, width = "4cm") |>
  column_spec(2, width = "8cm") |>
  column_spec(3, width = "2cm") |>
  pack_rows(group_label = "Structure requirements",1,1)|>
  pack_rows(group_label = "Panel-level requirements",2,2)|>
  pack_rows(group_label = "Pathway-level requirements",3,6)|>
  pack_rows(group_label = "Module-level requirements",7,12)|>
  pack_rows(group_label = "Itemset-level requirements",13,16)|>
  pack_rows(group_label = "Stimulus-level requirements",17,17)|>
  pack_rows(group_label = "Relative Objective: Maximize the TIFs at -1, 0 and 1",18,18)

## ----eval=FALSE---------------------------------------------------------------
# data("mixed_format_pool")
# # step 1: prepare item pool
# low_abilities<-c(-1.15,-0.67,-0.32)
# high_abilities<-c(0.32,0.67,1.15)
# target_abilities<-c(-1,0,1)
# theta_points<-c(low_abilities,high_abilities,target_abilities)
# theta_information<-compute_iif(mixed_format_pool,
#                                item_par_cols = list("2PL"=c("discrimination","difficulty")),
#                                theta = theta_points,
#                                model_col = "model",D = 1)
# mixed_format_pool[,paste0("iif(theta=",theta_points,")")]<-theta_information
# enemyitem_set<-create_enemy_sets(mixed_format_pool$item_id,
#                                  mixed_format_pool$enemyitem,
#                                  sep_pattern = ",")
# pivot_stim_map<-create_pivot_stimulus_map(mixed_format_pool,
#                                           item_id_col = "item_id",
#                                           stimulus = "stim",
#                                           pivot_item = "pivot")
# # step 2: specify mst structure
# mst122<-mst_design(itempool = mixed_format_pool,item_id_col = "item_id",
#                    design = "1-2-2",rdp = list(c(0),c(0)),
#                    module_length = rep(12,5),
#                    enemyitem_set = enemyitem_set,
#                    pivot_stim_map = pivot_stim_map)
# # step 3: identify hierarchical requirements
# 
# # step 4: translate to linear model
# mst_structure<-mst_structure_con(x = mst122,info_tol = 0.1)
# mst_noreuse<-panel_itemreuse_con(x = mst122,overlap = FALSE)
# mst_tei<-test_itemcat_con(x = mst122,attribute = "itemtype",cat_levels = "TEI",
#                           operator = "<=",target_num = 2,which_pathway = 1:4)
# mst_time<-test_itemquant_range_con(x = mst122,attribute = "time",
#                                    min = 20*60,max = 40*60,
#                                    which_pathway = 1:4)
# mst_tif_low1<-test_itemquant_con(x = mst122,
#                                  attribute = "iif(theta=-1.15)",
#                                  operator = ">=",
#                                  target_value = 10,
#                                  which_pathway = 1:2)
# mst_tif_low2<-test_itemquant_con(x = mst122,
#                                  attribute = "iif(theta=-0.67)",
#                                  operator = ">=",
#                                  target_value = 10,
#                                  which_pathway = 1:2)
# mst_tif_low3<-test_itemquant_con(x = mst122,
#                                  attribute = "iif(theta=-0.32)",
#                                  operator = ">=",
#                                  target_value = 10,
#                                  which_pathway = 1:2)
# mst_tif_high1<-test_itemquant_con(x = mst122,
#                                   attribute = "iif(theta=0.32)",
#                                   operator = ">=",
#                                   target_value = 10,
#                                   which_pathway = 3:4)
# mst_tif_high2<-test_itemquant_con(x = mst122,
#                                   attribute = "iif(theta=0.67)",
#                                   operator = ">=",
#                                   target_value = 10,
#                                   which_pathway = 3:4)
# mst_tif_high3<-test_itemquant_con(x = mst122,
#                                   attribute = "iif(theta=1.15)",
#                                   operator = ">=",
#                                   target_value = 10,
#                                   which_pathway = 3:4)
# mst_stimtype_s1<-test_stimcat_con(x = mst122,
#                                   attribute = "stimtype",
#                                   cat_levels = c("text-based","graphic-based"),
#                                   operator = "=",
#                                   target_num = 0,which_module = 1)
# mst_stimtype_s2<-test_stimcat_con(x = mst122,
#                                   attribute = "stimtype",
#                                   cat_levels = "text-based",
#                                   operator = "=",
#                                   target_num = 1,which_module = 2:3)
# mst_stimtype_s3<-test_stimcat_con(x = mst122,attribute = "stimtype",
#                                   cat_levels = "graphic-based",
#                                   operator = "=",
#                                   target_num = 1,which_module = 4:5)
# mst_content<-test_itemcat_range_con(x = mst122,
#                                     attribute = "content",
#                                     cat_levels = paste0("content ",1:4),
#                                     target = 3,deviation = 1,
#                                     which_module = 1:5)
# mst_dok<-test_itemcat_range_con(x = mst122,
#                                 attribute = "dok",
#                                 cat_levels = paste0("dok ",1:3),
#                                 min = 3,max = 5,
#                                 which_module = 1:5)
# mst_meandiff<-test_itemquant_range_con(x = mst122,
#                                        attribute = "difficulty",
#                                        target = c(-0.5,0.5,-1,1)*12,
#                                        deviation = 0.05*12,
#                                        which_module = 2:5)
# mst_stimitem_s1<-stim_itemcount_con(x = mst122,
#                                     min = 0,max = 0,
#                                     which_module = 1)
# mst_stimitem_s2_s3<-stim_itemcount_con(x = mst122,
#                                        min = 4,max = NULL,
#                                        which_module = 2:5)
# mst_stimitemtype<-stim_itemcat_con(x = mst122,
#                                    attribute = "itemtype",cat_levels = "TEI",
#                                    operator = "<=",target_num = 1,
#                                    which_module = 2:5)
# mst_noenemy<-enemyitem_exclu_con(x = mst122)
# mst_stimcomplexity<-stimquant_con(x = mst122,
#                                   attribute = "stimcomplexity",
#                                   min = 4.5,max = 8.5,
#                                   which_module = 2:5)
# constraint_list<-list(mst_structure,
#                       mst_noreuse,
#                       mst_tei,mst_time,
#                       mst_tif_low1,mst_tif_low2,mst_tif_low3,
#                       mst_tif_high1,mst_tif_high2,mst_tif_high3,
#                       mst_stimtype_s1,mst_stimtype_s2,mst_stimtype_s3,
#                       mst_content,mst_dok,mst_meandiff,
#                       mst_stimitem_s1,mst_stimitem_s2_s3,
#                       mst_stimitemtype,
#                       mst_noenemy,
#                       mst_stimcomplexity)
# abs_obj1<-objective_term(x = mst122,attribute = "iif(theta=-1)",
#                          applied_level = "Module-level",
#                          which_module = 1,sense = "min",
#                          goal = 5)
# abs_obj2<-objective_term(x = mst122,attribute = "iif(theta=0)",
#                          applied_level = "Module-level",
#                          which_module = 1,sense = "min",
#                          goal = 5)
# abs_obj3<-objective_term(x = mst122,attribute = "iif(theta=1)",
#                          applied_level = "Module-level",
#                          which_module = 1,sense = "min",
#                          goal = 5)
# unary_minimax<-minimax_obj(x = mst122,
#                            multiple_terms = list(abs_obj1,abs_obj2,abs_obj3),
#                            strategy_args = list(mode = "one_dev"))

## ----eval=FALSE---------------------------------------------------------------
# mst_tei_new<-test_itemcat_con(x = mst122,attribute = "itemtype",cat_levels = "TEI",
#                               operator = ">=",target_num = 1,which_module = 1:5)
# new_constraint_list<-constraint_list
# new_constraint_list[[length(constraint_list)+1]]<-mst_tei_new
# case1_model<-onepanel_spec(x = mst122,
#                            constraints = new_constraint_list,
#                            objective = unary_minimax)
# # \dontrun{
# # ### Step 5: Execute assembly via solver
# # case1_result<-solve_model(model_spec = case1_model,
# #                           solver = "HiGHS",check_feasibility = FALSE,
# #                           time_limit = 5*60)
# # ### Step 6: Diagnose infeasible model
# #
# # case1_singleblock<-check_singleblock_feasibility(model_spec = case1_model,
# #                                                  solver = "HiGHS",
# #                                                  time_limit = 60)
# # case1_TEIcomblock<-check_comblock_feasibility(model_spec = case1_model,
# #                                               con_blocks = c(5,28),
# #                                               solver = "HiGHS",
# #                                               time_limit = 60)
# # case1_resolve<-solve_with_slack(model_spec = case1_model,
# #                                 cat_penalty = 100,quant_penalty = 10,
# #                                 solver = "HiGHS",time_limit = 5*60)
# # }

## ----echo=FALSE---------------------------------------------------------------
case1_singleblock <- data.frame(
  Requirement = c(
    "Item count from TEI",
    "Sum of time",
    "Sum of time",
    "Sum of iif(theta=-1.15)",
    "Sum of iif(theta=-0.67)",
    "Sum of iif(theta=-0.32)",
    "Sum of iif(theta=0.32)",
    "Sum of iif(theta=0.67)",
    "Sum of iif(theta=1.15)",
    "Stimulus count from text-based/graphic-based",
    "Stimulus count from text-based",
    "Stimulus count from graphic-based",
    "Item count from content 1/content 2/content 3/content 4",
    "Item count from content 1/content 2/content 3/content 4",
    "Item count from dok 1/dok 2/dok 3",
    "Item count from dok 1/dok 2/dok 3",
    "Sum of difficulty",
    "Sum of difficulty",
    "Within-stimulus item count",
    "Within-stimulus item count",
    "Within-stimulus item count from TEI",
    "Enemy item exclusion",
    "Quantitative attribute per stimulus",
    "Item count from TEI",
    "Core set: (MST structure) Number of items in each module",
    "Core set: (MST structure) RDP",
    "Core set: (MST structure) RDP",
    "Core set: Item exposure control within a panel",
    "Core set: Sum of iif(theta=-1)",
    "Core set: Sum of iif(theta=0)",
    "Core set: Sum of iif(theta=1)"
  ),
  Attribute = c(
    "itemtype","time","time",
    "iif(theta=-1.15)","iif(theta=-0.67)","iif(theta=-0.32)",
    "iif(theta=0.32)","iif(theta=0.67)","iif(theta=1.15)",
    "stimtype","stimtype","stimtype",
    "content","content",
    "dok","dok",
    "difficulty","difficulty",
    "Stimulus item membership","Stimulus item membership",
    "itemtype","Enemy items membership",
    "stimcomplexity","itemtype",
    "Item_id","IIF","IIF",
    "Item_id (item-itself set)",
    "iif(theta=-1)","iif(theta=0)","iif(theta=1)"
  ),
  Type = c(
    "Categorical","Quantitative","Quantitative",
    "Quantitative","Quantitative","Quantitative",
    "Quantitative","Quantitative","Quantitative",
    "Categorical","Categorical","Categorical",
    "Categorical","Categorical",
    "Categorical","Categorical",
    "Quantitative","Quantitative",
    "Logical","Logical",
    "Logical","Logical",
    "Quantitative","Categorical",
    "Categorical","Quantitative","Quantitative",
    "Logical",
    "Quantitative","Quantitative","Quantitative"
  ),
  Application_Level = c(
    "Pathway-level","Pathway-level","Pathway-level",
    "Pathway-level","Pathway-level","Pathway-level",
    "Pathway-level","Pathway-level","Pathway-level",
    "Module-level","Module-level","Module-level",
    "Module-level","Module-level",
    "Module-level","Module-level",
    "Module-level","Module-level",
    "Module-level","Module-level",
    "Module-level","Pathway-level",
    "Module-level","Module-level",
    "Module-level","Module-level","Module-level",
    "Panel-level",
    "Module-level","Module-level","Module-level"
  ),
  Operator = c(
    "(max)","(min)","(max)",
    "(min)","(min)","(min)",
    "(min)","(min)","(min)",
    "(exact)","(exact)","(exact)",
    "(min)","(max)",
    "(min)","(max)",
    "(min)","(max)",
    "(exact)","(range)",
    "(max)","(max)",
    "(range)","(min)",
    "(exact)","(range)","(range)",
    "(max)",
    "Absolute objective","Absolute objective","Absolute objective"
  ),
  Num_Constraints = c(
    4,4,4,2,2,2,2,2,2,
    2,2,2,
    20,20,
    15,15,
    4,4,
    30,240,
    120,40,
    240,5,
    5,2,2,
    1000,
    2,2,2
  ),
  Source = c(rep("Constraint",28), rep("Objective",3)),
  Status = rep("OPTIMAL",31),
  stringsAsFactors = FALSE
)

kable(case1_singleblock,
      booktabs = TRUE,
      longtable = TRUE,
      caption = "Case 1: Infeasibility check for each specification") |>
  kable_styling(
    latex_options = c("hold_position"),
    full_width = FALSE,
    font_size = 10,
    position = "center") |>
  column_spec(1, width = "5cm") |>
  column_spec(2, width = "2cm") |>
  column_spec(3, width = "2cm") |>
  column_spec(4, width = "2cm") |>
  column_spec(5, width = "2cm") |>
  column_spec(6, width = "2cm") |>
  column_spec(7, width = "2cm") |>
  column_spec(8,width = "2cm")

## ----eval=FALSE---------------------------------------------------------------
# new_mst_tif_low1<-test_itemquant_con(x = mst122,
#                                      attribute = "iif(theta=-1.15)",
#                                      operator = ">=",
#                                      target_value = 30,
#                                      which_pathway = 1:2)
# new_mst_tif_low2<-test_itemquant_con(x = mst122,
#                                      attribute = "iif(theta=-0.67)",
#                                      operator = ">=",
#                                      target_value = 30,
#                                      which_pathway = 1:2)
# new_mst_tif_low3<-test_itemquant_con(x = mst122,
#                                      attribute = "iif(theta=-0.32)",
#                                      operator = ">=",
#                                      target_value = 30,
#                                      which_pathway = 1:2)
# new_mst_tif_high1<-test_itemquant_con(x = mst122,
#                                       attribute = "iif(theta=0.32)",
#                                       operator = ">=",
#                                       target_value = 30,
#                                       which_pathway = 3:4)
# new_mst_tif_high2<-test_itemquant_con(x = mst122,
#                                       attribute = "iif(theta=0.67)",
#                                       operator = ">=",
#                                       target_value = 30,
#                                       which_pathway = 3:4)
# new_mst_tif_high3<-test_itemquant_con(x = mst122,
#                                       attribute = "iif(theta=1.15)",
#                                       operator = ">=",
#                                       target_value = 30,
#                                       which_pathway = 3:4)
# 
# new_constraint_list<-list(mst_structure,
#                           mst_noreuse,
#                           mst_tei,mst_time,
#                           new_mst_tif_low1,new_mst_tif_low2,new_mst_tif_low3,
#                           new_mst_tif_high1,new_mst_tif_high2,new_mst_tif_high3,
#                           mst_stimtype_s1,mst_stimtype_s2,mst_stimtype_s3,
#                           mst_content,mst_dok,mst_meandiff,
#                           mst_stimitem_s1,mst_stimitem_s2_s3,
#                           mst_stimitemtype,
#                           mst_noenemy,
#                           mst_stimcomplexity)
# case2_model<-onepanel_spec(x = mst122,
#                            constraints = new_constraint_list,
#                            objective = unary_minimax)
# # \dontrun{
# # ### Step 5: Execute assembly via solver
# # case2_result<-solve_model(model_spec = case2_model,
# #                           solver = "HiGHS",check_feasibility = FALSE,
# #                           time_limit = 5*60)
# # ### Step 6: Diagnose infeasible model
# # case2_singleblock<-check_singleblock_feasibility(model_spec = case2_model,
# #                                                  solver = "HiGHS",
# #                                                  time_limit = 60)
# #}

## ----echo=FALSE---------------------------------------------------------------
case2_singleblock <- data.frame(
  Requirement = c(
    "Item count from TEI",
    "Sum of time",
    "Sum of time",
    "Sum of iif(theta=-1.15)",
    "Sum of iif(theta=-0.67)",
    "Sum of iif(theta=-0.32)",
    "Sum of iif(theta=0.32)",
    "Sum of iif(theta=0.67)",
    "Sum of iif(theta=1.15)",
    "Stimulus count from text-based/graphic-based",
    "Stimulus count from text-based",
    "Stimulus count from graphic-based",
    "Item count from content 1/content 2/content 3/content 4",
    "Item count from content 1/content 2/content 3/content 4",
    "Item count from dok 1/dok 2/dok 3",
    "Item count from dok 1/dok 2/dok 3",
    "Sum of difficulty",
    "Sum of difficulty",
    "Within-stimulus item count",
    "Within-stimulus item count",
    "Within-stimulus item count from TEI",
    "Enemy item exclusion",
    "Quantitative attribute per stimulus",
    "Core set: (MST structure) Number of items in each module",
    "Core set: (MST structure) RDP",
    "Core set: (MST structure) RDP",
    "Core set: Item exposure control within a panel",
    "Core set: Sum of iif(theta=-1)",
    "Core set: Sum of iif(theta=0)",
    "Core set: Sum of iif(theta=1)"
  ),
  Attribute = c(
    "itemtype","time","time",
    "iif(theta=-1.15)","iif(theta=-0.67)","iif(theta=-0.32)",
    "iif(theta=0.32)","iif(theta=0.67)","iif(theta=1.15)",
    "stimtype","stimtype","stimtype",
    "content","content",
    "dok","dok",
    "difficulty","difficulty",
    "Stimulus item membership","Stimulus item membership",
    "itemtype","Enemy items membership",
    "stimcomplexity",
    "Item_id","IIF","IIF",
    "Item_id (item-itself set)",
    "iif(theta=-1)","iif(theta=0)","iif(theta=1)"
  ),
  Type = c(
    "Categorical","Quantitative","Quantitative",
    "Quantitative","Quantitative","Quantitative",
    "Quantitative","Quantitative","Quantitative",
    "Categorical","Categorical","Categorical",
    "Categorical","Categorical",
    "Categorical","Categorical",
    "Quantitative","Quantitative",
    "Logical","Logical",
    "Logical","Logical",
    "Quantitative",
    "Categorical","Quantitative","Quantitative",
    "Logical",
    "Quantitative","Quantitative","Quantitative"
  ),
  Application_Level = c(
    "Pathway-level","Pathway-level","Pathway-level",
    "Pathway-level","Pathway-level","Pathway-level",
    "Pathway-level","Pathway-level","Pathway-level",
    "Module-level","Module-level","Module-level",
    "Module-level","Module-level",
    "Module-level","Module-level",
    "Module-level","Module-level",
    "Module-level","Module-level",
    "Module-level","Pathway-level",
    "Module-level",
    "Module-level","Module-level","Module-level",
    "Panel-level",
    "Module-level","Module-level","Module-level"
  ),
  Operator = c(
    "(max)","(min)","(max)",
    "(min)","(min)","(min)",
    "(min)","(min)","(min)",
    "(exact)","(exact)","(exact)",
    "(min)","(max)",
    "(min)","(max)",
    "(min)","(max)",
    "(exact)","(range)",
    "(max)","(max)",
    "(range)",
    "(exact)","(range)","(range)",
    "(max)",
    "Absolute objective","Absolute objective","Absolute objective"
  ),
  Num_Constraints = c(
    4,4,4,2,2,2,2,2,2,
    2,2,2,
    20,20,
    15,15,
    4,4,
    30,240,
    120,40,
    240,
    5,2,2,
    1000,
    2,2,2
  ),
  Source = c(rep("Constraint",27), rep("Objective",3)),
  Status = c(
    "OPTIMAL","OPTIMAL","OPTIMAL",
    rep("INFEASIBLE",6),
    rep("OPTIMAL",21)
  ),
  stringsAsFactors = FALSE
)

kable(case2_singleblock,
      booktabs = TRUE,
      longtable = TRUE,
      caption = "Case 2: Infeasibility check for each specification") |>
  kable_styling(
    latex_options = c("hold_position"),
    full_width = FALSE,
    font_size = 10,
    position = "center") |>
  column_spec(1, width = "5cm") |>
  column_spec(2, width = "2cm") |>
  column_spec(3, width = "2cm") |>
  column_spec(4, width = "2cm") |>
  column_spec(5, width = "2cm") |>
  column_spec(6, width = "2cm") |>
  column_spec(7, width = "2cm") |>
  column_spec(8,width = "2cm")

## ----eval=FALSE---------------------------------------------------------------
# onepanel<-onepanel_spec(x = mst122,
#                         constraints = constraint_list,
#                         objective = unary_minimax)
# num_panels<-5
# mst_unique_stim<-solution_stimcount_con(x = mst122,operator = "=",
#                                         target_num = 4*num_panels)
# case3_model<-multipanel_spec(x = mst122,panel_model = onepanel,
#                              solution_con = list(mst_unique_stim),
#                              num_panels = num_panels,global_max_use = 2)
# # \dontrun{
# # ### Step 5: Execute assembly via solver
# # case3_result<-solve_model(model_spec = case3_model,
# #                           solver = "HiGHS",check_feasibility = FALSE,
# #                           time_limit = 60*5)
# #}

