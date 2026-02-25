test_itempool <- readRDS(test_path("test_itempool.rds"))

pivot_stim_map<-create_pivot_stimulus_map(test_itempool,item_id_col = "item_id",
                                          stimulus = "stimulus",pivot_item = "pivot_item")
enemyitem_set<-create_enemy_sets(test_itempool$item_id,test_itempool$enemy_item,sep_pattern = ",")
enemystim_set<-create_enemy_sets(test_itempool$stimulus,test_itempool$enemy_stimulus,sep_pattern = ",")

make_test_mstATA_BU<-function(){
  mst_design(itempool = test_itempool,
             design = "1-3-3",
             rdp = list(c(-0.5,0.5),c(-1,1)),
             module_length = c(4,2,2,2,2,2,2),
             pivot_stim_map = pivot_stim_map,
             enemyitem_set = enemyitem_set,
             enemystim_set = enemystim_set)
}

make_test_mstATA_TD<-function(){
  mst_design(itempool = test_itempool,
             design = "1-3-3",
             pathway_length = 8,
             exclude_pathway = c("1-1-3","1-3-1"),
             pivot_stim_map = pivot_stim_map,
             enemyitem_set = enemyitem_set,
             enemystim_set = enemystim_set)
}


