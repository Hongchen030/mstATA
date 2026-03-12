## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.5
)
library(knitr)
library(mstATA)
library(highs)
library(ggplot2)

## ----echo=FALSE---------------------------------------------------------------
tab <- data.frame(
  Attribute = c("Discrimination",
                "Difficulty",
                "Guessing",
                "Response Time",
                "Word Counts"),
  Level = c("Item", "Item", "Item", "Item", "Stimulus"),
  Mean = c(0.92, -0.01, 0.10, 120.11, 123.47),
  SD = c(0.19, 0.97, 0.04, 35.01, 35.01),
  Min = c(0.51, -3.24, 0.01, 60, 52),
  Max = c(1.59, 2.14, 0.26, 180, 199)
)

kable(tab,
      caption = "Descriptive statistics for item and stimulus quantitative attributes.",
      col.names = c("Attribute", "Level", "Mean", "SD", "Min", "Max"),
      digits = 2,
      align = c("l", "l", "r", "r", "r", "r"))


## -----------------------------------------------------------------------------
data("reading_itempool")
REE<-c(-1.39,-0.97,-0.68)
REM<-c(-0.43,-0.21,0)
RHM<-c(0,0.21,0.43)
RHH<-c(0.68,0.97,1.39)
theta_values<-unique(c(REE,REM,RHM,RHH))
item_par_cols<-list("3PL"=c("discrimination","difficulty","guessing"))
theta_iif<-compute_iif(reading_itempool,
                       item_par_cols = item_par_cols,
                       theta = theta_values,model_col = "model",
                       D = 1.7)
reading_itempool[,paste0("iif(theta=",theta_values,")")]<-theta_iif
enemyitem_set<-create_enemy_sets(reading_itempool$item_id,
                                 reading_itempool$enemy_item)
enemystim_set<-create_enemy_sets(reading_itempool$stimulus,
                                 reading_itempool$enemy_stimulus)
pivot_stim_map<-create_pivot_stimulus_map(reading_itempool,
                                          item_id_col = "item_id",
                                          stimulus = "stimulus",
                                          pivot_item = "pivot_item")

## -----------------------------------------------------------------------------
mst_123 <- mst_design(itempool = reading_itempool,item_id_col = "item_id",
                      design = "1-2-3",rdp = list(c(0),c(-0.43,0.43)),
                      exclude_pathways = c("1-1-3","1-2-1"),
                      module_length = c(12,12,12,12,12,12),
                      enemyitem_set = enemyitem_set,
                      enemystim_set = enemystim_set,
                      pivot_stim_map = pivot_stim_map)

## -----------------------------------------------------------------------------
mst_structure<-mst_structure_con(x = mst_123,info_tol = 0.1)
mst_noreuse<-panel_itemreuse_con(x = mst_123,overlap = FALSE)
mst_noenemyitem<-enemyitem_exclu_con(x = mst_123)
mst_noenemystim<-enemystim_exclu_con(x = mst_123)
mst_content<-test_itemcat_range_con(x = mst_123,attribute = "content",
                                    cat_levels = paste0("content",1:4),
                                    min = 7,max = 11,
                                    which_pathway = 1:4)
mst_tei<-test_itemcat_con(x = mst_123,attribute = "itemtype",
                          cat_levels = "TEI",
                          operator = "=",target_num = 2,
                          which_module = 1:6)
mst_passtype<-test_stimcat_con(x = mst_123,attribute = "stimulus_type",
                               cat_levels = c("history","social studies"),
                               operator = "=",target_num = 1,
                               which_module = 1:6)
mst_time<-test_itemquant_range_con(x = mst_123,attribute = "time",
                                   min = 110*12,max = 130*12,
                                   which_module = 1:6)
mst_stimitem<-stim_itemcount_con(x = mst_123,min = 4,max = 8,
                                 which_module = 1:6)
mst_stimquant<-stimquant_con(x = mst_123,attribute = "stimulus_words",
                             min = 90,max = 150,
                             which_module = 1:6)
obj1<-objective_term(x = mst_123,attribute = "iif(theta=-1.39)",
                     applied_level = "Pathway-level",
                     which_pathway = 1,sense = "max")
obj2<-objective_term(x = mst_123,attribute = "iif(theta=-0.97)",
                     applied_level = "Pathway-level",
                     which_pathway = 1,sense = "max")
obj3<-objective_term(x = mst_123,attribute = "iif(theta=-0.68)",
                     applied_level = "Pathway-level",
                     which_pathway = 1,sense = "max")
obj4<-objective_term(x = mst_123,attribute = "iif(theta=-0.43)",
                     applied_level = "Pathway-level",
                     which_pathway = 2,sense = "max")
obj5<-objective_term(x = mst_123,attribute = "iif(theta=-0.21)",
                     applied_level = "Pathway-level",
                     which_pathway = 2,sense = "max")
obj6<-objective_term(x = mst_123,attribute = "iif(theta=0)",
                     applied_level = "Pathway-level",
                     which_pathway = 2,sense = "max")
obj7<-objective_term(x = mst_123,attribute = "iif(theta=0)",
                     applied_level = "Pathway-level",
                     which_pathway = 3,sense = "max")
obj8<-objective_term(x = mst_123,attribute = "iif(theta=0.21)",
                     applied_level = "Pathway-level",
                     which_pathway = 3,sense = "max")
obj9<-objective_term(x = mst_123,attribute = "iif(theta=0.43)",
                     applied_level = "Pathway-level",
                     which_pathway = 3,sense = "max")
obj10<-objective_term(x = mst_123,attribute = "iif(theta=0.68)",
                      applied_level = "Pathway-level",
                     which_pathway = 4,sense = "max")
obj11<-objective_term(x = mst_123,attribute = "iif(theta=0.97)",
                      applied_level = "Pathway-level",
                     which_pathway = 4,sense = "max")
obj12<-objective_term(x = mst_123,attribute = "iif(theta=1.39)",
                      applied_level = "Pathway-level",
                     which_pathway = 4,sense = "max")
mst_obj<-capped_maximin_obj(x = mst_123,
                            multiple_terms = list(obj1,obj2,obj3,
                                                  obj4,obj5,obj6,
                                                  obj7,obj8,obj9,
                                                  obj10,obj11,obj12),
                            strategy_args = list(proportions = rep(c(1,1.5,1),4)))
mst_model<-onepanel_spec(x = mst_123,
                         constraints = list(mst_structure,mst_noreuse,
                                            mst_content,mst_noenemyitem,mst_noenemystim,
                                            mst_tei,mst_passtype,mst_time,
                                            mst_stimitem,
                                            mst_stimquant),
                         objective = mst_obj)


## -----------------------------------------------------------------------------
# It is not executed in the vignette to avoid long build times.
# \dontrun{
# mst_result<-solve_model(model_spec = mst_model,solver = "HiGHS",time_limit = 5*60)
# reading_panel<-assembled_panel(x = mst_123,result = mst_result)
# }

## ----echo=FALSE---------------------------------------------------------------
data("reading_panel")
# RDP information check
RDP_check<-rbind(report_test_tif(assembled_panel = reading_panel,
                                 theta = 0,
                                 item_par_cols = item_par_cols,
                                 model_col = "model",D = 1.7,
                                 which_module = 2:3),
                 report_test_tif(assembled_panel = reading_panel,
                                 theta = -0.43,
                                 item_par_cols = item_par_cols,
                                 model_col = "model",D = 1.7,
                                 which_module = 4:5),
                 report_test_tif(assembled_panel = reading_panel,
                                 theta = 0.43,
                                 item_par_cols = item_par_cols,
                                 model_col = "model",D = 1.7,
                                 which_module = 5:6))
kable(RDP_check,
      caption = "Routing decision points information check",
      digits = 2,
      align = c("l","r", "r", "r"))

Content_check<-report_test_itemcat(assembled_panel = reading_panel,
                                   attribute = "content",
                                   cat_levels = paste0("content",1:4),
                                   which_pathway = 1:4)
kable(Content_check,
      caption = "Number of items per content check")

time_check<-report_test_itemquant(assembled_panel = reading_panel,
                                  attribute = "time",
                                  statistic = "average",
                                  which_module = 1:6)
kable(time_check,
      caption = "Average response time per item check")

pathway_tifcheck <- data.frame(theta = c(-1.39, -0.97, -0.68, -0.43, -0.21, 0, 0,
                                         0.21, 0.43, 0.68, 0.97, 1.39),
                               pathway_id = c("M-E-E", "M-E-E", "M-E-E",
                                              "M-E-M", "M-E-M", "M-E-M",
                                              "M-H-M", "M-H-M", "M-H-M",
                                              "M-H-H", "M-H-H", "M-H-H"),
                               must_greater_than = c(8.941002, 13.4115, 8.941002,
                                                     8.941002, 13.4115, 8.941002,
                                                     8.941002, 13.4115, 8.941002,
                                                     8.941002, 13.4115, 8.941002),
                               realized_information = c(11.64047, 13.4115, 13.72838,
                                                        13.24364, 13.48899, 13.29367,
                                                        13.25245, 13.4666, 13.35719,
                                                        13.72612, 13.47349, 11.21304),
                               must_lower_than = c(13.72838, 18.19888, 13.72838,
                                                   13.72838, 18.19888, 13.72838,
                                                   13.72838, 18.19888, 13.72838,
                                                   13.72838, 18.19888, 13.72838))

kable(pathway_tifcheck,
      caption = "Pathway-level information requirements and realized information at selected ability levels.",
      digits = 3,
      align = c("r","l","r", "r", "r"))
plot_panel_tif(assembled_panel = reading_panel,item_par_cols = item_par_cols,
               model_col = "model",D = 1.7,theta = seq(-3,3,0.1),unit = "pathway")

