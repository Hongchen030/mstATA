## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.5
)
library(knitr)
library(kableExtra)
library(ggplot2)
library(ggpubr)
library(mstATA)

## ----echo=FALSE---------------------------------------------------------------
#---- table data ----
tbl_attr <- data.frame(
  Attributes = c("Content area", "Item type", "DOK level","Response time","Discrimination","Difficulty",
                 rep("Information",9),
                 "Stimulus type","Stimulus complexity score"),
  Type = c(rep("Categorical",3),rep("Quantitative",3),
           rep("Quantitative",9),
           "Categorical", "Quantitative"),
  Description = c(
    "Four content areas. Num of items: (233, 265, 257, 245).",
    "Two item types: MC and TEI. Num of items: (800, 200).",
    "Three DOK levels. Num of items: (332, 332, 336).",
    "Mean: 62.43, SD: 18.13, Min: 25.64, Max: 150.41.",
    "Mean: 1.04, SD: 0.30, Min: 0.37, Max: 2.92.",
    "Mean: 0.01, SD: 0.96, Min: -3.87, Max: 3.15.",

    "$\\theta$ = -1.15, Mean: 0.18, SD: 0.12, Min: 0.00, Max: 0.92.",
    "$\\theta$ = -1, Mean: 0.19, SD: 0.12, Min: 0.01, Max: 0.83.",
    "$\\theta$ = -0.67, Mean: 0.21, SD: 0.14, Min: 0.01, Max: 1.31.",
    "$\\theta$ = -0.32, Mean: 0.23, SD: 0.15, Min: 0.02, Max: 2.04.",
    "$\\theta$ = 0, Mean: 0.24, SD: 0.15, Min: 0.02, Max: 2.00.",
    "$\\theta$ = 0.32, Mean: 0.23, SD: 0.14, Min: 0.02, Max: 1.32.",
    "$\\theta$ = 0.67, Mean: 0.21, SD: 0.14, Min: 0.01, Max: 0.97.",
    "$\\theta$ = 1, Mean: 0.19, SD: 0.13, Min: 0.01, Max: 0.91.",
    "$\\theta$ = 1.15, Mean: 0.18, SD: 0.12, Min: 0.01, Max: 0.88.",
    
    "Two stimulus types: text-based, graphic-based. Num of stimuli: (10, 20).",
    "Mean: 5.90, SD: 1.76, Min: 3.00, Max: 8.80."
  )
)
colnames(tbl_attr)<-c("Attribute","Type","Description")
# ---- render table ----
kable(
  tbl_attr,
  booktabs = TRUE,
  longtable = FALSE,
  escape = FALSE,
  caption = "Descriptive statistics for categorical and quantitative attributes") |>
  kable_styling(
    latex_options = c("hold_position"),
    full_width = FALSE,
    position = "center",
    font_size = 10
  ) |>
  column_spec(1, width = "3cm") |>
  column_spec(2, width = "2cm") |>
  column_spec(3, width = "10cm") |>
  pack_rows(group_label = "Item attributes",1,15)|>
  pack_rows(group_label = "Stimulus attributes",16,17)

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
    
    "maximin",
    "capped maximin",
    "weighted sum",
    "unary minimax",
    "binary minimax",
    "goal programming"
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

    "$\\max y \\quad \\text{s.t. } y \\le y_k \\le y + 0.5$",
    "$\\max y - \\delta \\quad \\text{s.t. } y \\le y_k \\le y + \\delta$",
    "$\\max y_1 +y_2 + y_3 \\quad \\text{s.t. } \\sum_i I_i(\\theta_k) x_{i,m=1} = y_k$",
    "$\\min\\; d \\quad \\text{s.t. }\\; \\lvert y_k - g_k \\rvert \\le d$",
    "$\\min\\; d^{+} + d^{-} \\quad \\text{s.t. }\\; g_k - y_k \\le d^{-} \\text{and } y_k - g_k \\le d^{+}$",
    "$\\min\\; d_1 + d_2 + d_3 \\quad \\text{s.t. }\\; \\lvert y_k - g_k \\rvert \\le d_k$"
  ),
  `Num of Constraints` = c(9,1000,4,8,6,6,
                           2,2,2,40,30,8,
                           30,240,120,40,
                           240,
                           6,6,3,6,6,6)
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
  pack_rows(group_label = "Relative Objective: Maximize the TIFs at -1, 0 and 1",18,20)|>
  pack_rows(group_label = "Absolute Objective: Minimize the deviation from target TIFs at -1, 0 and 1",21,23)

## -----------------------------------------------------------------------------
data("mixed_format_pool")
item_par_cols <-list("2PL"=c("discrimination","difficulty"))
# step 1: prepare item pool
low_abilities<-c(-1.15,-0.67,-0.32)
high_abilities<-c(0.32,0.67,1.15)
target_abilities<-c(-1,0,1)
theta_points<-c(low_abilities,high_abilities,target_abilities)
theta_information<-compute_iif(mixed_format_pool,
                               item_par_cols = item_par_cols,
                               theta = theta_points,
                               model_col = "model",D = 1)
mixed_format_pool[,paste0("iif(theta=",theta_points,")")]<-theta_information
enemyitem_set<-create_enemy_sets(mixed_format_pool$item_id,
                                 mixed_format_pool$enemyitem,
                                 sep_pattern = ",")
pivot_stim_map<-create_pivot_stimulus_map(mixed_format_pool,
                                          item_id_col = "item_id",
                                          stimulus = "stim",
                                          pivot_item = "pivot")
# step 2: specify mst structure
mst122<-mst_design(itempool = mixed_format_pool,item_id_col = "item_id",
                   design = "1-2-2",rdp = list(c(0),c(0)),
                   module_length = rep(12,5),
                   enemyitem_set = enemyitem_set,
                   pivot_stim_map = pivot_stim_map)
# step 3: identify hierarchical requirements

# step 4: translate to linear model
mst_structure<-mst_structure_con(x = mst122,info_tol = 0.1)
mst_noreuse<-panel_itemreuse_con(x = mst122,overlap = FALSE)
mst_tei<-test_itemcat_con(x = mst122,attribute = "itemtype",cat_levels = "TEI",
                          operator = "<=",target_num = 2,which_pathway = 1:4)
mst_time<-test_itemquant_range_con(x = mst122,attribute = "time",
                                   min = 20*60,max = 40*60,
                                   which_pathway = 1:4)
mst_tif_low1<-test_itemquant_con(x = mst122,
                                 attribute = "iif(theta=-1.15)",
                                 operator = ">=",
                                 target_value = 10,
                                 which_pathway = 1:2)
mst_tif_low2<-test_itemquant_con(x = mst122,
                                 attribute = "iif(theta=-0.67)",
                                 operator = ">=",
                                 target_value = 10,
                                 which_pathway = 1:2)
mst_tif_low3<-test_itemquant_con(x = mst122,
                                 attribute = "iif(theta=-0.32)",
                                 operator = ">=",
                                 target_value = 10,
                                 which_pathway = 1:2)
mst_tif_high1<-test_itemquant_con(x = mst122,
                                  attribute = "iif(theta=0.32)",
                                  operator = ">=",
                                  target_value = 10,
                                  which_pathway = 3:4)
mst_tif_high2<-test_itemquant_con(x = mst122,
                                  attribute = "iif(theta=0.67)",
                                  operator = ">=",
                                  target_value = 10,
                                  which_pathway = 3:4)
mst_tif_high3<-test_itemquant_con(x = mst122,
                                  attribute = "iif(theta=1.15)",
                                  operator = ">=",
                                  target_value = 10,
                                  which_pathway = 3:4)
mst_stimtype_s1<-test_stimcat_con(x = mst122,
                                  attribute = "stimtype",
                                  cat_levels = c("text-based","graphic-based"),
                                  operator = "=",
                                  target_num = 0,which_module = 1)
mst_stimtype_s2<-test_stimcat_con(x = mst122,
                                  attribute = "stimtype",
                                  cat_levels = "text-based",
                                  operator = "=",
                                  target_num = 1,which_module = 2:3)
mst_stimtype_s3<-test_stimcat_con(x = mst122,attribute = "stimtype",
                                  cat_levels = "graphic-based",
                                  operator = "=",
                                  target_num = 1,which_module = 4:5)
mst_content<-test_itemcat_range_con(x = mst122,
                                    attribute = "content",
                                    cat_levels = paste0("content ",1:4),
                                    target = 3,deviation = 1,
                                    which_module = 1:5)
mst_dok<-test_itemcat_range_con(x = mst122,
                                attribute = "dok",
                                cat_levels = paste0("dok ",1:3),
                                min = 3,max = 5,
                                which_module = 1:5)
mst_meandiff<-test_itemquant_range_con(x = mst122,
                                       attribute = "difficulty",
                                       target = c(-0.5,0.5,-1,1)*12,
                                       deviation = 0.05*12,
                                       which_module = 2:5)
mst_stimitem_s1<-stim_itemcount_con(x = mst122,
                                    min = 0,max = 0,
                                    which_module = 1)
mst_stimitem_s2_s3<-stim_itemcount_con(x = mst122,
                                       min = 4,max = NULL,
                                       which_module = 2:5)
mst_stimitemtype<-stim_itemcat_con(x = mst122,
                                   attribute = "itemtype",cat_levels = "TEI",
                                   operator = "<=",target_num = 1,
                                   which_module = 2:5)
mst_noenemy<-enemyitem_exclu_con(x = mst122)
mst_stimcomplexity<-stimquant_con(x = mst122,
                                  attribute = "stimcomplexity",
                                  min = 4.5,max = 8.5,
                                  which_module = 2:5)
constraint_list<-list(mst_structure,
                      mst_noreuse,
                      mst_tei,mst_time,
                      mst_tif_low1,mst_tif_low2,mst_tif_low3,
                      mst_tif_high1,mst_tif_high2,mst_tif_high3,
                      mst_stimtype_s1,mst_stimtype_s2,mst_stimtype_s3,
                      mst_content,mst_dok,mst_meandiff,
                      mst_stimitem_s1,mst_stimitem_s2_s3,
                      mst_stimitemtype,
                      mst_noenemy,
                      mst_stimcomplexity)

## -----------------------------------------------------------------------------
rel_obj1<-objective_term(x = mst122,attribute = "iif(theta=-1)",
                         applied_level = "Module-level",
                         which_module = 1,sense = "max")
rel_obj2<-objective_term(x = mst122,attribute = "iif(theta=0)",
                         applied_level = "Module-level",
                         which_module = 1,sense = "max")
rel_obj3<-objective_term(x = mst122,attribute = "iif(theta=1)",
                         applied_level = "Module-level",
                         which_module = 1,sense = "max")

### maximin
maximin<-maximin_obj(x = mst122,
                     multiple_terms = list(rel_obj1,rel_obj2,rel_obj3),
                     strategy_args = list(delta = 0.5))
maximin_model<-onepanel_spec(x = mst122,
                             constraints = constraint_list,
                             objective = maximin)
# \dontrun{
# ### Step 5: Execute assembly via solver
# maximin_result<-solve_model(model_spec = maximin_model,
#                             solver = "HiGHS",check_feasibility = FALSE,
#                             time_limit = 60*5)
# maximin_panel<-assembled_panel(x = mst122,result = maximin_result)
# ### Step 6: Diagnose infeasible model
# # There is an optimal solution. Skip this step.
# }

### capped maximin
capped_maximin<-capped_maximin_obj(x = mst122,
                                   multiple_terms = list(rel_obj1,rel_obj2,rel_obj3))
capped_maximin_model<-onepanel_spec(x = mst122,
                                    constraints = constraint_list,
                                    objective = capped_maximin)
# \dontrun{
# ### Step 5: Execute assembly via solver
# capped_maximin_result<-solve_model(model_spec = capped_maximin_model,
#                                    solver = "HiGHS",check_feasibility = FALSE,
#                                    time_limit = 60*5)
# capped_maximin_panel<-assembled_panel(x = mst122,result = capped_maximin_result)
# ### Step 6: Diagnose infeasible model
# # There is an optimal solution. Skip this step.
# }

###weighted sum
weighted_sum<-weighted_sum_obj(x = mst122,
                               multiple_terms = list(rel_obj1,rel_obj2,rel_obj3))
weighted_sum_model<-onepanel_spec(x = mst122,
                                  constraints = constraint_list,
                                  objective = weighted_sum)
# \dontrun{
# ### Step 5: Execute assembly via solver
# weighted_sum_result<-solve_model(model_spec = weighted_sum_model,
#                                  solver = "HiGHS",check_feasibility = FALSE,
#                                  time_limit = 60*5)
# weighted_sum_panel<-assembled_panel(x = mst122,result = weighted_sum_result)
# ### Step 6: Diagnose infeasible model
# # There is an optimal solution. Skip this step.
# }

abs_obj1<-objective_term(x = mst122,attribute = "iif(theta=-1)",
                         applied_level = "Module-level",
                         which_module = 1,sense = "min",
                         goal = 5)
abs_obj2<-objective_term(x = mst122,attribute = "iif(theta=0)",
                         applied_level = "Module-level",
                         which_module = 1,sense = "min",
                         goal = 5)
abs_obj3<-objective_term(x = mst122,attribute = "iif(theta=1)",
                         applied_level = "Module-level",
                         which_module = 1,sense = "min",
                         goal = 5)
## unary minimax
unary_minimax<-minimax_obj(x = mst122,
                           multiple_terms = list(abs_obj1,abs_obj2,abs_obj3),
                           strategy_args = list(mode = "one_dev"))
unary_minimax_model<-onepanel_spec(x = mst122,
                                   constraints = constraint_list,
                                   objective = unary_minimax)
# \dontrun{
# ### Step 5: Execute assembly via solver
# unary_minimax_result<-solve_model(model_spec = unary_minimax_model,
#                                   solver = "HiGHS",check_feasibility = FALSE,
#                                   time_limit = 60*5)
# unary_minimax_panel<-assembled_panel(x = mst122,result = unary_minimax_result)
# ### Step 6: Diagnose infeasible model
# # There is an optimal solution. Skip this step.
# }

## binary minimax
binary_minimax<-minimax_obj(x = mst122,
                            multiple_terms = list(abs_obj1,abs_obj2,abs_obj3),
                            strategy_args = list(mode = "two_dev"))
binary_minimax_model<-onepanel_spec(x = mst122,
                                    constraints = constraint_list,
                                    objective = binary_minimax)
# \dontrun{
# ### Step 5: Execute assembly via solver
# binary_minimax_result<-solve_model(model_spec = binary_minimax_model,
#                                    solver = "HiGHS",check_feasibility = FALSE,
#                                    time_limit = 60*5)
# binary_minimax_panel<-assembled_panel(x = mst122,result = binary_minimax_result)
# ### Step 6: Diagnose infeasible model
# # There is an optimal solution. Skip this step.
# }

## goal_programming
goal_programming<-goal_programming_obj(x = mst122,
                                       multiple_terms = list(abs_obj1,abs_obj2,abs_obj3),
                                       strategy_args = list(mode = "two_dev"))
goal_programming_model<-onepanel_spec(x = mst122,
                                      constraints = constraint_list,
                                      objective = goal_programming)
# \dontrun{
# ### Step 5: Execute assembly via solver
# goal_programming_result<-solve_model(model_spec = goal_programming_model,
#                                      solver = "HiGHS",check_feasibility = FALSE,
#                                      time_limit = 60*5)
# goal_programming_panel<-assembled_panel(x = mst122,result = goal_programming_result)
# ### Step 6: Diagnose infeasible model
# # There is an optimal solution. Skip this step.
# }

## ----echo=FALSE---------------------------------------------------------------
data("maximin_panel")
data("capped_maximin_panel")
data("weighted_sum_panel")
data("unary_minimax_panel")
data("binary_minimax_panel")
data("goal_programming_panel")

## ----echo=FALSE---------------------------------------------------------------
TEI_check <- data.frame(
  Formulation = c("Maximin",
                  "Capped Maximin",
                  "Weighted Sum",
                  "Unary Minimax",
                  "Binary Minimax",
                  "Goal Programming"),
  `S1R-S2E-S3E` = rep(2, 6),
  `S1R-S2H-S3E` = rep(2, 6),
  `S1R-S2E-S3H` = rep(2, 6),
  `S1R-S2H-S3H` = rep(2, 6),
  check.names = FALSE
)

kable(TEI_check,
      align = c("l", "r", "r", "r", "r"),
      caption = "Check: Number of TEIs per pathway")

## ----echo=FALSE---------------------------------------------------------------
Time_check <- data.frame(
  Formulation = c("Maximin",
                  "Capped Maximin",
                  "Weighted Sum",
                  "Unary Minimax",
                  "Binary Minimax",
                  "Goal Programming"),
  `S1R-S2E-S3E` = c(35.55, 38.42, 36.96, 35.03, 34.95, 37.38),
  `S1R-S2H-S3E` = c(35.24, 37.30, 35.87, 35.63, 36.32, 35.53),
  `S1R-S2E-S3H` = c(38.52, 36.50, 34.89, 33.28, 34.21, 37.71),
  `S1R-S2H-S3H` = c(38.21, 35.39, 33.80, 33.88, 35.58, 35.86),
  check.names = FALSE
)

kable(Time_check,
      digits = 2,
      align = c("l", "r", "r", "r", "r"),
      caption = "Check: Total expected response time (minutes) per pathway")


## ----echo=FALSE---------------------------------------------------------------
TIF_thr_check<-data.frame(Formulation = rep(c("Maximin",
                                              "Capped Maximin",
                                              "Weighted Sum",
                                              "Unary Minimax",
                                              "Binary Minimax",
                                              "Goal Programming"),6),
                          `S1R-S2E-S3E` = c(11.51,10.32,10.02,10.45,10.35,10.11,
                                            11.99,10.79,13.55,11.31,10.61,10.81,
                                            12.12,"10.90",16.06,11.56,10.72,11.12,
                                            rep("",18)),
                          `S1R-S2H-S3E` = c(10.01,"10.00",10.07,10.03,10.04,10.19,
                                            11.12,"10.70",13.69,11.08,10.47,10.82,
                                            "11.70","10.90",16.08,11.42,10.67,11.07,
                                            rep("",18)),
                          `S1R-S2E-S3H` = c(rep("",18),
                                            c("12.60",11.61,16.12,11.92,11.46,11.67,
                                              "12.30",11.54,13.98,11.66,11.53,11.55,
                                              "10.40",10.04,10.21,"10.00","10.10",10.01)),
                          `S1R-S2H-S3H` = c(rep("",18),
                                            c(12.88,11.65,16.03,11.96,11.62,11.67,
                                              12.74,11.55,13.95,11.76,11.78,11.62,
                                              10.83,10.02,10.37,10.11,10.38,10.16)),
                          check.names = FALSE)
kable(TIF_thr_check,
      digits = 2,
      caption = "Check: TIF threshold at target theta points per pathway")%>%
  pack_rows("theta = -1.15",1,6)%>%
  pack_rows("theta = -0.67",7,12)%>%
  pack_rows("theta = -0.32",13,18)%>%
  pack_rows("theta = 0.32",19,24)%>%
  pack_rows("theta = 0.67",25,30)%>%
  pack_rows("theta = 1.15",31,36)%>%
  column_spec(1,width = "8cm")%>%
  column_spec(2,width = "4cm")%>%
  column_spec(3,width = "4cm")%>%
  column_spec(4,width = "4cm")%>%
  column_spec(5,width = "4cm")

## ----echo=FALSE---------------------------------------------------------------
Content_check <- data.frame(
  Formulation = c("Maximin",
                  "Capped Maximin",
                  "Weighted Sum",
                  "Unary Minimax",
                  "Binary Minimax",
                  "Goal Programming"),
  S1R = c("(2,3,3,4)",
          "(2,4,3,3)",
          "(3,2,3,4)",
          "(2,4,3,3)",
          "(2,4,3,3)",
          "(2,3,4,3)"),
  S2E = c("(2,2,4,4)",
          "(2,2,4,4)",
          "(2,2,4,4)",
          "(4,2,3,3)",
          "(2,2,4,4)",
          "(2,4,4,2)"),
  S2H = c("(2,3,3,4)",
          "(3,3,4,2)",
          "(2,3,4,3)",
          "(3,4,2,3)",
          "(3,2,4,3)",
          "(2,2,4,4)"),
  S3E = c("(2,3,3,4)",
          "(2,4,4,2)",
          "(3,3,4,2)",
          "(2,3,4,3)",
          "(3,2,4,3)",
          "(2,3,4,3)"),
  S3H = c("(2,3,3,4)",
          "(2,3,3,4)",
          "(2,3,3,4)",
          "(3,2,3,4)",
          "(3,3,3,3)",
          "(2,4,2,4)"),
  check.names = FALSE
)

kable(Content_check,
      align = c("l", "c", "c", "c", "c", "c"),
      caption = "Check: Content distribution across stages")

## ----echo=FALSE---------------------------------------------------------------
DOK_check <- data.frame(
  Formulation = c("Maximin",
                  "Capped Maximin",
                  "Weighted Sum",
                  "Unary Minimax",
                  "Binary Minimax",
                  "Goal Programming"),
  S1R = c("(4,5,3)",
          "(4,5,3)",
          "(5,3,4)",
          "(4,4,4)",
          "(4,5,3)",
          "(4,3,5)"),
  S2E = c("(5,4,3)",
          "(3,4,5)",
          "(3,5,4)",
          "(3,5,4)",
          "(3,4,5)",
          "(4,3,5)"),
  S2H = c("(3,4,5)",
          "(3,4,5)",
          "(4,4,4)",
          "(5,3,4)",
          "(3,4,5)",
          "(3,5,4)"),
  S3E = c("(4,5,3)",
          "(4,4,4)",
          "(3,4,5)",
          "(3,5,4)",
          "(4,4,4)",
          "(3,4,5)"),
  S3H = c("(5,4,3)",
          "(4,4,4)",
          "(3,4,5)",
          "(5,4,3)",
          "(3,5,4)",
          "(3,5,4)"),
  check.names = FALSE
)

kable(DOK_check,
      align = c("l", "c", "c", "c", "c", "c"),
      caption = "Check: DOK distribution across stages")

## ----echo=FALSE---------------------------------------------------------------
Meandiff_check <- data.frame(
  Formulation = c("Maximin",
                  "Capped Maximin",
                  "Weighted Sum",
                  "Unary Minimax",
                  "Binary Minimax",
                  "Goal Programming"),
  S2E = c(-0.46, -0.52, -0.53, -0.49, -0.46, -0.45),
  S2H = c(0.50, 0.53, 0.53, 0.46, 0.50, 0.48),
  S3E = c(-0.97, -0.96, -1.00, -1.02, -0.98, -1.00),
  S3H = c(1.04, 0.98, 1.02, 0.99, 1.03, 1.03),
  check.names = FALSE
)

kable(Meandiff_check,
      digits = 2,
      align = c("l", "r", "r", "r", "r"),
      caption = "Check: Mean difficulty for adaptive modules")

## ----echo=FALSE---------------------------------------------------------------
Stim_check<-data.frame(Formulation = rep(c("Maximin",
                                           "Capped Maximin",
                                           "Weighted Sum",
                                           "Unary Minimax",
                                           "Binary Minimax",
                                           "Goal Programming"),each = 5),
                       Module = rep(c("S1R","S2E","S2H","S3E","S3H"),6),
                       `Stimulus Name` = c("",paste("stim",c(13,5,6,25)),
                                           "",paste("stim",c(13,29,23,6)),
                                           "",paste("stim",c(13,5,25,6)),
                                           "",paste("stim",c(29,5,6,19)),
                                           "",paste("stim",c(13,29,23,6)),
                                           "",paste("stim",c(13,29,2,6))),
                       `Stimulus Type` = rep(c("","text","text","graphic","graphic"),6),
                       `Stimulus Complexity Score` = c("","5.80","7.60","4.60","7.20",
                                                       "","5.80","7.80","6.00","4.60",
                                                       "","5.80","7.60","7.20","4.60",
                                                       "","7.80","7.60","4.60","6.60",
                                                       "","5.80","7.80","6.00","4.60",
                                                       "","5.80","7.80","5.40","4.60"),
                       `Num of Selected Items`=rep(c(0,4,4,4,4),6),
                       check.names = FALSE)
Stim_check<-Stim_check[,2:6]
kable(Stim_check,
      escape = FALSE,
      align = "cccccc",
      caption = "Stimulus-related requirements check in Demonstration 3")%>%
  pack_rows("Maximin Formulation",1,5)%>%
  pack_rows("Capped Maximin Formulation",6,10)%>%
  pack_rows("Weighted Sum Formulation",11,15)%>%
  pack_rows("Unary Minimax Formulation",16,20)%>%
  pack_rows("Binary Minimax Formulation",21,25)%>%
  pack_rows("Goal Programming Formulation",26,30)%>%
  column_spec(1,width = "8cm")%>%
  column_spec(2,width = "4cm")%>%
  column_spec(3,width = "4cm")%>%
  column_spec(4,width = "4cm")%>%
  column_spec(5,width = "4cm")

## ----echo=FALSE,message=FALSE-------------------------------------------------
p1<-plot_panel_tif(assembled_panel = maximin_panel,
                   item_par_cols = list("2PL" = c("discrimination", "difficulty")),
                   theta = seq(-3,3,0.1),model_col = "model",D = 1,
                   unit = "pathway") +
  geom_vline(xintercept = c(-1.15, -0.67, -0.32,0.32,0.67,1.15),
             linetype = "dashed",color = "black",linewidth = 0.7) +
  labs(title = "Maximin") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))+
  scale_x_continuous(breaks = seq(-3, 3, by = 1))+ 
  scale_y_continuous(limits = c(0, 20),breaks = seq(0,20,5)) +
  scale_color_discrete(labels = c("M-E-E" = "S1R-S2E-S3E","M-E-H" = "S1R-S2E-S3H","M-H-E" = "S1R-S2H-S3E","M-H-H" = "S1R-S2H-S3H"))
  
p2<-plot_panel_tif(assembled_panel = capped_maximin_panel,
                   item_par_cols = list("2PL" = c("discrimination", "difficulty")),
                   theta = seq(-3,3,0.1),model_col = "model",D = 1,
                   unit = "pathway") +
  geom_vline(xintercept = c(-1.15, -0.67, -0.32,0.32,0.67,1.15),
             linetype = "dashed",color = "black",linewidth = 0.7) +
  labs(title = "Capped Maximin") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))+
  scale_x_continuous(breaks = seq(-3, 3, by = 1))+ 
  scale_y_continuous(limits = c(0, 20),breaks = seq(0,20,5)) +
  scale_color_discrete(labels = c("M-E-E" = "S1R-S2E-S3E","M-E-H" = "S1R-S2E-S3H","M-H-E" = "S1R-S2H-S3E","M-H-H" = "S1R-S2H-S3H"))

p3<-plot_panel_tif(assembled_panel = weighted_sum_panel,
                   item_par_cols = list("2PL" = c("discrimination", "difficulty")),
                   theta = seq(-3,3,0.1),model_col = "model",D = 1,
                   unit = "pathway") +
  geom_vline(xintercept = c(-1.15, -0.67, -0.32,0.32,0.67,1.15),
             linetype = "dashed",color = "black",linewidth = 0.7)+
  labs(title = "Weighted Sum") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))+
  scale_x_continuous(breaks = seq(-3, 3, by = 1))+ 
  scale_y_continuous(limits = c(0, 20),breaks = seq(0,20,5)) +
  scale_color_discrete(labels = c("M-E-E" = "S1R-S2E-S3E","M-E-H" = "S1R-S2E-S3H","M-H-E" = "S1R-S2H-S3E","M-H-H" = "S1R-S2H-S3H"))

p4<-plot_panel_tif(assembled_panel = unary_minimax_panel,
                   item_par_cols = list("2PL" = c("discrimination", "difficulty")),
                   theta = seq(-3,3,0.1),model_col = "model",D = 1,
                   unit = "pathway") +
  geom_vline(xintercept = c(-1.15, -0.67, -0.32,0.32,0.67,1.15),
             linetype = "dashed",color = "black",linewidth = 0.7)+
  labs(title = "Unary Minimax") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))+
  scale_x_continuous(breaks = seq(-3, 3, by = 1))+ 
  scale_y_continuous(limits = c(0, 20),breaks = seq(0,20,5)) +
  scale_color_discrete(labels = c("M-E-E" = "S1R-S2E-S3E","M-E-H" = "S1R-S2E-S3H","M-H-E" = "S1R-S2H-S3E","M-H-H" = "S1R-S2H-S3H"))

p5<-plot_panel_tif(assembled_panel = binary_minimax_panel,
                   item_par_cols = list("2PL" = c("discrimination", "difficulty")),
                   theta = seq(-3,3,0.1),model_col = "model",D = 1,
                   unit = "pathway") +
  geom_vline(xintercept = c(-1.15, -0.67, -0.32,0.32,0.67,1.15),
             linetype = "dashed",color = "black",linewidth = 0.7)+
  labs(title = "Binary Minimax") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))+
  scale_x_continuous(breaks = seq(-3, 3, by = 1))+ 
  scale_y_continuous(limits = c(0, 20),breaks = seq(0,20,5)) +
  scale_color_discrete(labels = c("M-E-E" = "S1R-S2E-S3E","M-E-H" = "S1R-S2E-S3H","M-H-E" = "S1R-S2H-S3E","M-H-H" = "S1R-S2H-S3H"))

p6<-plot_panel_tif(assembled_panel = goal_programming_panel,
                   item_par_cols = list("2PL" = c("discrimination", "difficulty")),
                   theta = seq(-3,3,0.1),model_col = "model",D = 1,
                   unit = "pathway") +
  geom_vline(xintercept = c(-1.15, -0.67, -0.32,0.32,0.67,1.15),
             linetype = "dashed",color = "black",linewidth = 0.7)+
  labs(title = "Goal Programming") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))+
  scale_x_continuous(breaks = seq(-3, 3, by = 1))+ 
  scale_y_continuous(limits = c(0, 20),breaks = seq(0,20,5)) +
  scale_color_discrete(labels = c("M-E-E" = "S1R-S2E-S3E","M-E-H" = "S1R-S2E-S3H","M-H-E" = "S1R-S2H-S3E","M-H-H" = "S1R-S2H-S3H"))
ggarrange(p1,p2,p3,p4,p5,p6,common.legend = TRUE,ncol = 3,nrow = 2,legend = "bottom")


## ----echo=FALSE,message=FALSE-------------------------------------------------
p7<-plot_panel_tif(assembled_panel = maximin_panel,
                   item_par_cols = list("2PL" = c("discrimination", "difficulty")),
                   theta = seq(-3,3,0.1),model_col = "model",D = 1,
                   unit = "module") +
  geom_vline(xintercept = 0,
             linetype = "dashed",color = "black",linewidth = 0.7) +
  labs(title = "Maximin") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))+
  scale_x_continuous(breaks = seq(-3, 3, by = 1))+ 
  scale_y_continuous(limits = c(0, 12),breaks = seq(0,12,3))+
  scale_color_discrete(labels = c("S1M" = "S1R","S2E" = "S2E","S2H" = "S2H","S3E" = "S3E","S3H" = "S3H"))

p8<-plot_panel_tif(assembled_panel = capped_maximin_panel,
                   item_par_cols = list("2PL" = c("discrimination", "difficulty")),
                   theta = seq(-3,3,0.1),model_col = "model",D = 1,
                   unit = "module") +
  geom_vline(xintercept = 0,
             linetype = "dashed",color = "black",linewidth = 0.7) +
  labs(title = "Capped Maximin") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))+
  scale_x_continuous(breaks = seq(-3, 3, by = 1))+ 
  scale_y_continuous(limits = c(0, 12),breaks = seq(0,12,3))+
  scale_color_discrete(labels = c("S1M" = "S1R","S2E" = "S2E","S2H" = "S2H","S3E" = "S3E","S3H" = "S3H"))

p9<-plot_panel_tif(assembled_panel = weighted_sum_panel,
                   item_par_cols = list("2PL" = c("discrimination", "difficulty")),
                   theta = seq(-3,3,0.1),model_col = "model",D = 1,
                   unit = "module") +
  geom_vline(xintercept = 0,
             linetype = "dashed",color = "black",linewidth = 0.7)+
  labs(title = "Weighted Sum") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))+
  scale_x_continuous(breaks = seq(-3, 3, by = 1))+ 
  scale_y_continuous(limits = c(0, 12),breaks = seq(0,12,3))+
  scale_color_discrete(labels = c("S1M" = "S1R","S2E" = "S2E","S2H" = "S2H","S3E" = "S3E","S3H" = "S3H"))

p10<-plot_panel_tif(assembled_panel = unary_minimax_panel,
                   item_par_cols = list("2PL" = c("discrimination", "difficulty")),
                   theta = seq(-3,3,0.1),model_col = "model",D = 1,
                   unit = "module") +
  geom_vline(xintercept = 0,
             linetype = "dashed",color = "black",linewidth = 0.7)+
  labs(title = "Unary Minimax") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))+
  scale_x_continuous(breaks = seq(-3, 3, by = 1))+ 
  scale_y_continuous(limits = c(0, 12),breaks = seq(0,12,3))+
  scale_color_discrete(labels = c("S1M" = "S1R","S2E" = "S2E","S2H" = "S2H","S3E" = "S3E","S3H" = "S3H"))

p11<-plot_panel_tif(assembled_panel = binary_minimax_panel,
                   item_par_cols = list("2PL" = c("discrimination", "difficulty")),
                   theta = seq(-3,3,0.1),model_col = "model",D = 1,
                   unit = "module") +
  geom_vline(xintercept = 0,
             linetype = "dashed",color = "black",linewidth = 0.7)+
  labs(title = "Binary Minimax") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))+
  scale_x_continuous(breaks = seq(-3, 3, by = 1))+ 
  scale_y_continuous(limits = c(0, 12),breaks = seq(0,12,3))+
  scale_color_discrete(labels = c("S1M" = "S1R","S2E" = "S2E","S2H" = "S2H","S3E" = "S3E","S3H" = "S3H"))

p12<-plot_panel_tif(assembled_panel = goal_programming_panel,
                   item_par_cols = list("2PL" = c("discrimination", "difficulty")),
                   theta = seq(-3,3,0.1),model_col = "model",D = 1,
                   unit = "module") +
  geom_vline(xintercept = 0,
             linetype = "dashed",color = "black",linewidth = 0.7)+
  labs(title = "Goal Programming") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))+
  scale_x_continuous(breaks = seq(-3, 3, by = 1))+ 
  scale_y_continuous(limits = c(0, 12),breaks = seq(0,12,3))+
  scale_color_discrete(labels = c("S1M" = "S1R","S2E" = "S2E","S2H" = "S2H","S3E" = "S3E","S3H" = "S3H"))
ggarrange(p7,p8,p9,p10,p11,p12,common.legend = TRUE,ncol = 3,nrow = 2,legend = "bottom")

## ----echo=FALSE---------------------------------------------------------------
Obj_check <- data.frame(
  Formulation = c("Maximin",
                  "Capped Maximin",
                  "Weighted Sum",
                  "Unary Minimax",
                  "Binary Minimax",
                  "Goal Programming"),
  theta_m1 = c(4.97, 4.88, 4.36, 4.90, 4.88, 5.00),
  theta_0  = c(5.41, 4.94, 10.32, 5.09, 4.94, 5.03),
  theta_1  = c(4.96, 4.87, 5.93, 4.88, 4.87, 4.77)
)

kable(Obj_check,
      digits = 2,
      escape = FALSE,
      align = c("l", "r", "r", "r"),
      col.names = c("Formulation", "\\(\\theta = -1\\)", "\\(\\theta = 0\\)", "\\(\\theta = 1\\)"),
      caption = "Check: Realized objective values at -1, 0 and 1 for S1R module")

## ----echo=FALSE---------------------------------------------------------------
tab <- data.frame(
  Formulation = c("Maximin",
                  "Capped Maximin",
                  "Weighted Sum",
                  "Unary Minimax",
                  "Binary Minimax",
                  "Goal Programming"),
  `Num of Constraints` = c(1793,1793,1790,
                           1793,1793,1793),
  `Num of Obj Variable` = c(1,2,3,
                       1,2,6),
  Obj_Val = c("4.960", "4.809 = 4.874 - 0.065", "20.608 = 4.357 + 10.324 +5.927", 
              "0.118", "0.126 = 0.126 + 0", "0.253 = 0 + 0 + 0 + 0.028 + 0.225 + 0"),
  RunTime_Seconds = c(1.645,3.716,0.953,8.817,3.188,5.358),
  check.names = FALSE
)

kable(tab,
      digits = 3,
      align = c("l", "c", "c","c","c"),
      caption = "Comparison of objective values and solver runtime across multiple-objective formulations.")

## ----echo=FALSE---------------------------------------------------------------
maximin_eval<-analytic_mst_precision(design = "1-2-2",rdps = list(0,0),
                                     assembled_panel = maximin_panel,
                                     item_par_cols = item_par_cols,
                                     model_col = "model",D = 1,theta = seq(-3,3,0.1),
                                     range_tcc = c(-5,5))$Panel_1$eval_tb
cappedmaximin_eval<-analytic_mst_precision(design = "1-2-2",rdps = list(0,0),
                                           assembled_panel = capped_maximin_panel,
                                           item_par_cols = item_par_cols,
                                           model_col = "model",D = 1,theta = seq(-3,3,0.1),
                                           range_tcc = c(-5,5))$Panel_1$eval_tb
weightedsum_eval<-analytic_mst_precision(design = "1-2-2",rdps = list(0,0),
                                         assembled_panel = weighted_sum_panel,
                                         item_par_cols = item_par_cols,
                                         model_col = "model",D = 1,theta = seq(-3,3,0.1),
                                         range_tcc = c(-5,5))$Panel_1$eval_tb
unaryminimax_eval<-analytic_mst_precision(design = "1-2-2",rdps = list(0,0),
                                          assembled_panel = unary_minimax_panel,
                                          item_par_cols = item_par_cols,
                                          model_col = "model",D = 1,theta = seq(-3,3,0.1),
                                          range_tcc = c(-5,5))$Panel_1$eval_tb
binaryminimax_eval<-analytic_mst_precision(design = "1-2-2",rdps = list(0,0),
                                           assembled_panel = binary_minimax_panel,
                                           item_par_cols = item_par_cols,
                                           model_col = "model",D = 1,theta = seq(-3,3,0.1),
                                           range_tcc = c(-5,5))$Panel_1$eval_tb
goalprog_eval<-analytic_mst_precision(design = "1-2-2",rdps = list(0,0),
                                      assembled_panel = goal_programming_panel,
                                      item_par_cols = item_par_cols,
                                      model_col = "model",D = 1,theta = seq(-3,3,0.1),
                                      range_tcc = c(-5,5))$Panel_1$eval_tb

eval_plot<-rbind(maximin_eval,cappedmaximin_eval,weightedsum_eval,
                 unaryminimax_eval,binaryminimax_eval,goalprog_eval)
eval_plot[,"Formulation"]<-rep(c("Maximin","Capped Maximin","Weighted Sum",
                                 "Unary Minimax","Binary Minimax","Goal Programming"),
                               each = nrow(maximin_eval))
eval_plot[,"Objective"]<-"Relative Objectives"
eval_plot[eval_plot$Formulation%in%c("Unary Minimax","Binary Minimax","Goal Programming"),"Objective"]<-"Absolute Objectives"
eval_plot$Objective<-as.factor(eval_plot$Objective)
ggplot(eval_plot, aes(x = theta, y = bias, color = Formulation,linetype = Objective)) +
  geom_line() +
  labs(x = expression(theta), y = "Bias") +
  scale_x_continuous(breaks = seq(-3, 3, by = 1)) +
  theme_bw() +
  theme(
    strip.background = element_rect(
      fill = "grey85",
      color = NA
    ),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )+labs(title = "Conditional bias (Recursion-based analytical approach)")
ggplot(eval_plot, aes(x = theta, y = csem, color = Formulation,linetype = Objective)) +
  geom_line() +
  labs(x = expression(theta), y = "CSEM") +
  scale_x_continuous(breaks = seq(-3, 3, by = 1)) +
  theme_bw() +
  theme(
    strip.background = element_rect(
      fill = "grey85",
      color = NA
    ),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )+labs(title = "Conditional SEM (Recursion-based analytical approach)")

## ----echo=FALSE---------------------------------------------------------------
Formulation<-c("Maximin","Capped Maximin","Weighted Sum",
               "Unary Minimax","Binary Minimax","Goal Programming")
report<-data.frame(Formulation = rep(Formulation,each = 5),
                   `Pass Rate (%)` = rep(c(20,30,50,70,80),6),
                   `False Positive` = NA_real_,
                   `False Negative` = NA_real_,
                   `Classification Accuracy` = NA_real_,
                   `Classification Consistency` = NA_real_,
                   check.names = FALSE)
testpopulation<-gen_weight(theta = seq(-3,3,0.1),dist = "norm")
decision<-c(-0.8416,-0.5244,0,0.5244,0.8416)
for(formulation_id in 1:6){
  formulation<-Formulation[formulation_id]
  rowstart<-(1+5*(formulation_id-1))
  if(formulation=="Maximin"){
    dat<-maximin_eval
  }else if(formulation=="Capped Maximin"){
    dat<-cappedmaximin_eval
  }else if(formulation=="Weighted Sum"){
    dat<-weightedsum_eval
  }else if(formulation=="Unary Minimax"){
    dat<-unaryminimax_eval
  }else if(formulation=="Binary Minimax"){
    dat<-binaryminimax_eval
  }else{
    dat<-goalprog_eval
  }
  for(theta_id in seq_along(decision)){
    theta<-decision[theta_id]
    result<-analytic_mst_classification(decision_theta_cuts = theta,
                                        eval_tb = dat,theta_weight = testpopulation)
    confusion<-result$confusion
    false_positive<-confusion[1,2]
    false_negative<-confusion[2,1]
    marginal<-result$marginal
    classification_accuracy<-marginal$accuracy[3]
    classification_consistency<-marginal$consistency[3]
    report[rowstart+(theta_id-1),3:6]<-c(false_positive,false_negative,
                               classification_accuracy,classification_consistency)*100
  }
}
report[,3:6]<-round(report[,3:6],digits = 2)
kable(report[,c(2,3:6)],
      digits = 3,
      align = c("l", "c", "c","c","c"),
      caption = "Calssification evaluation across multiple-objective formulations.")%>%
  pack_rows("Maximin Formulation",1,5)%>%
  pack_rows("Capped Maximin Formulation",6,10)%>%
  pack_rows("Weighted Sum Formulation",11,15)%>%
  pack_rows("Unary Minimax Formulation",16,20)%>%
  pack_rows("Binary Minimax Formulation",21,25)%>%
  pack_rows("Goal Programming Formulation",26,30)%>%
  column_spec(1,width = "4cm")%>%
  column_spec(2,width = "4cm")%>%
  column_spec(3,width = "4cm")%>%
  column_spec(4,width = "4cm")%>%
  column_spec(5,width = "4cm")

