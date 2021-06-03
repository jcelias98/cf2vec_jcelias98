library(cvTools)
library(labelrank)
library(Kendall)
library(reshape2)

source("labelrankingforests-master/RankTrees/RankTrees.R")
source("labelrankingforests-master/RankTrees/PredRTrees.R")
source("labelrankingforests-master/RFR.R")
source("LR_tuning.R")
source("LR_evaluation.R")
source("auxiliary.R")

#impact on the baselevel experiments
#due to the huge amount of results available, one has selected a cf2vec representation (i.e. embeddings_grid_search/sample_100_wl6/sample_graphs_dims_30_epochs_2000_lr_0.3_batch_256_negsample_10_embeddings.csv")
run_experiment <- function(type){
  
  if(type == "IR"){
    B <- read.csv("metafeatures_landmarkers/B_IR_final.csv", sep=";")  #landmarkers datasets have NAs
    targets <- read.csv("targets/IR.csv", sep=";")
    goal = "max"
  }
  else{
    B <- read.csv("metafeatures_landmarkers/B_RP_final.csv", sep=";")
    targets <- read.csv("targets/RP.csv", sep=";")
    goal = "min"
  }
  
  A <- read.csv("metafeatures_statistical/mf_final.csv", sep=";")
  C <- read.csv("metafeatures_graph/graph_metafeatures_final.csv", sep=",")
  D <- mergeUnifiedDataset(A,B,C)  
  rm(A,B,C)
  
  cf4vec <- read.csv("embeddings_grid_search/sample_100_wl6/sample_graphs_dims_30_epochs_2000_lr_0.3_batch_256_negsample_10_embeddings.csv", header = F)
  tmp <- colnames(cf4vec)
  tmp[1] <- "dataset"
  colnames(cf4vec) <- tmp

  node2vec <- read.csv("embeddings_grid_search/sample_100_wl6/embeddings_node2vec_dims_30.csv", header = F)
  tmp <- colnames(node2vec)
  tmp[1] <- "dataset"
  colnames(node2vec) <- tmp

  struct2vec <- read.csv("embeddings_grid_search/sample_100_wl6/embeddings_struct2vec_dims_30.csv", header = F)
  tmp <- colnames(struct2vec)
  tmp[1] <- "dataset"
  colnames(struct2vec) <- tmp

  AWE <- read.csv("embeddings_grid_search/sample_100_wl6/embeddings_AWE_dims_30.csv", header = F)
  tmp <- colnames(AWE)
  tmp[1] <- "dataset"
  colnames(AWE) <- tmp
  
  keepcolumn <- c("dataset","performance")
  perf_values <- targets[,which(colnames(targets) %in% keepcolumn)]
  
  metafeatures_result <- rank_evaluation_baselevel_impact(D,targets,perf_values,goal,"MF")
  baseline_result <- rank_evaluation_baselevel_impact(D,targets,perf_values,goal,"MF",onlyBaseline = T)
  cf4vec_result <- rank_evaluation_baselevel_impact(cf4vec,targets,perf_values,goal,"cf4vec")
  node2vec_result <- rank_evaluation_baselevel_impact(node2vec,targets,perf_values,goal,"node2vec")
  struct2vec_result <- rank_evaluation_baselevel_impact(struct2vec,targets,perf_values,goal,"struct2vec")
  AWE_result <- rank_evaluation_baselevel_impact(AWE,targets,perf_values,goal,"AWE")
  
  
  list(metafeatures_result,baseline_result,cf4vec_result,node2vec_result, struct2vec_result,AWE_result)
}

IR <- run_experiment("IR")
RP <- run_experiment("RP")

save(IR, RP, file="results/impact_baselevel_performance.Rda", version=2)