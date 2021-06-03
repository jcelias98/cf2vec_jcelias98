library(reshape2)
library(ggplot2)
################ prepare data.frame

params2Vec <- function(val){
  val <- gsub("sample_graphs","",val)
  tmp <- unlist(strsplit(toString(val),"[_]"))
  res <- c(as.numeric(tmp[3]),as.numeric(tmp[5]),as.numeric(tmp[7]),as.numeric(tmp[9]),as.numeric(tmp[11]))
  res
}

createDF <- function(IR,RP,wl,removeOthers = TRUE){
  final <- rbind(
    apply(IR,2,mean),
    apply(RP,2,mean)
  )
  row.names(final) <- c("IR","RP")
  final <- t(final)
  
  extra_cols <- rbind(
    rep(0,5),rep(0,5)
  )
  
  for(i in 3:dim(final)[1]){
    extra_cols <- rbind(extra_cols,params2Vec(row.names(final)[i]))
  }
  
  
  
  colnames(extra_cols) <- c("dims","epochs","lr","batch","negsample")
  final <- cbind(final,extra_cols)
  final <- as.data.frame(final)
  final$dims <- as.factor(final$dims)
  final$epochs <- as.factor(final$epochs)
  final$lr <- as.factor(final$lr)
  final$batch <- as.factor(final$batch)
  final$negsample <- as.factor(final$negsample)
  final$wl <- wl
  
  if(removeOthers){
    final <- final[-which(rownames(final) == "MF"),]
    final <- final[-which(rownames(final) == "baseline"),]
  }
  final 
}

load("embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl3.Rda")
df1 <- createDF(IR,RP,3)

load("embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl4.Rda")
df2 <- createDF(IR,RP,4)

load("embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl5.Rda")
df3 <- createDF(IR,RP,5)

load("embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl6.Rda")
df4 <- createDF(IR,RP,6)

load("embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl7.Rda")
df5 <- createDF(IR,RP,7)

load("embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl8.Rda")
df6 <- createDF(IR,RP,8)


final <- rbind(df1,df2,df3,df4,df5,df6)
final$wl <- as.factor(final$wl)

rm(df1,df2,df3,df4)


### manually selected best
best <- final[  with(final, order(-IR, -RP)), ][2,]  #manually found 
load("embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl6.Rda")
best_IR <- IR$sample_graphs_dims_30_epochs_2000_lr_0.3_batch_256_negsample_10_embeddings.csv
best_RP <- RP$sample_graphs_dims_30_epochs_2000_lr_0.3_batch_256_negsample_10_embeddings.csv

mf_IR <- IR$MF
mf_RP <- RP$MF

avg_IR<- IR$baseline
avg_RP<- RP$baseline

load("embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl6_novo2.Rda")
best_IR_node2vec <- IR$embeddings_node2vec_dims_30.csv
best_RP_node2vec <- RP$embeddings_node2vec_dims_30.csv

best_IR_struct2vec <- IR$embeddings_struct2vec_dims_30.csv
best_RP_struct2vec <- RP$embeddings_struct2vec_dims_30.csv

best_IR_AWE <- IR$embeddings_AWE_dims_30.csv
best_RP_AWE <- RP$embeddings_AWE_dims_30.csv

tmp <- data.frame(
  cf4vec <- unlist(lapply(1:length(best_IR),function(x){mean(c(best_IR[x],best_RP[x]))})),
  Metalearning <- unlist(lapply(1:length(mf_IR),function(x){mean(c(mf_IR[x],mf_RP[x]))})),
  AverageRankings <- unlist(lapply(1:length(avg_IR),function(x){mean(c(avg_IR[x],avg_RP[x]))})),
  node2vec <- unlist(lapply(1:length(best_IR_node2vec),function(x){mean(c(best_IR_node2vec[x],best_RP_node2vec[x]))})),
  struct2vec <- unlist(lapply(1:length(best_IR_struct2vec),function(x){mean(c(best_IR_struct2vec[x],best_RP_struct2vec[x]))})),
  AWE <- unlist(lapply(1:length(best_IR_AWE),function(x){mean(c(best_IR_AWE[x],best_RP_AWE[x]))}))
)

colnames(tmp) <- c("cf2vec","CM","baseline", "node2vec", "struct2vec", "AWE")

library(scmamp)
plotCD (tmp, alpha=0.05, cex=1.25)


##################################333


createSimpleGraphic <- function(tmp){
  
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#9999CC", "#66CC99")
  
  g <- ggplot(tmp, aes(x=algorithm,y=performance,group=algorithm,fill=algorithm)) +
    scale_fill_manual(values=cbPalette) +
    geom_bar(stat = "identity", width = 0.7, position = "dodge") +
    guides(fill = guide_legend(title = "Meta-algorithms"))+
    facet_grid(. ~ strategy) +
    ylab("Kendall's tau") +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(text= element_text(size = 16)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  g
  
}

tmp_ir <- data.frame(
  AVG = avg_IR,
  LR_CM = mf_IR,
  LR_CF2VEC = best_IR,
  LR_NODE2VEC = best_IR_node2vec,
  LR_STRUC2VEC = best_IR_struct2vec,
  LR_AWE = best_IR_AWE 
)
colnames(tmp_ir) <- c("AVG","LR+CM","LR+cf2vec", "LR+node2vec", "LR+struct2vec", "LR+AWE")

tmp_rp <- data.frame(
  AVG = avg_RP,
  LR_CM = mf_RP,
  LR_CF2VEC = best_RP,
  LR_NODE2VEC= best_RP_node2vec,
  LR_STRUC2VEC = best_RP_struct2vec,
  LR_AWE = best_RP_AWE
)

colnames(tmp_rp) <- c("AVG","LR+CM","LR+cf2vec", "LR+node2vec","LR+struct2vec", "LR+AWE")

#kendall's tau
new_tmp_ir <- apply(tmp_ir,2,mean)
new_tmp_ir <- melt(new_tmp_ir)
new_tmp_ir$metatarget <- "IR"
new_tmp_ir$algorithm <- rownames(new_tmp_ir)

new_tmp_rp <- apply(tmp_rp,2,mean)
new_tmp_rp <- melt(new_tmp_rp)
new_tmp_rp$metatarget <- "RP"
new_tmp_rp$algorithm <- rownames(new_tmp_rp)

new_tmp <- rbind(new_tmp_ir,new_tmp_rp)
colnames(new_tmp) <- c("performance","strategy","algorithm")
createSimpleGraphic(new_tmp)
