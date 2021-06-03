library(plyr)
library(reshape2)
library(ggplot2)


load("results/impact_baselevel_performance.Rda")


make_graph <- function(data, metric){
  
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#9999CC", "#66CC99")

  
  ggplot(data=data, aes(x=algorithms, y=value, group=variable, color=variable)) +
    geom_line() +
    geom_point() +
    xlab("Algorithms") + ylab("Lift (%)") +
    scale_x_continuous(breaks=seq(0, 10, 1)) +
    facet_grid(. ~ strategy, scales = "free") +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(text= element_text(size = 16)) +
    theme(legend.title=element_blank()) +
    scale_color_manual(values = cbPalette) +
    theme(axis.title.x=element_blank())
  
}

normalizeDF <- function(dt,inverted){
  
  data_graph <- as.data.frame(apply(dt, 1, function(dt,min_value,max_value){
      if (inverted) {
        return ((1-(dt-min_value)/(max_value-min_value))*100)
      }
      else {
        return (((dt-min_value)/(max_value-min_value))*100)
      }
  }, min_value=min(dt), max_value=max(dt)))
  
  row.names(data_graph) <- colnames(dt)
  
  data_graph
}


createData <- function(x,inverted, strategy){
  
  number_labels <- length(unlist(x[[1]]))
  
  data_graph <- data.frame(
    'baseline' = unlist(x[[2]]),
    cf4vec = unlist(x[[3]]),
    node2vec = unlist(x[[4]]),
    struct2vec = unlist(x[[5]]),
    AWE = unlist(x[[6]]),
    Metalearning = unlist(x[[1]])
  )
  rownames(data_graph) <- NULL
  
  data_graph <- as.data.frame(t(normalizeDF(data_graph,inverted)))
  data_graph$algorithms <- c(1:number_labels)
  
  
  data_graph <- melt(data_graph, id=c("algorithms"))
  data_graph$strategy <- strategy
  
  data_graph
}

df1 <- createData(IR,F,"IR")
df2 <- createData(RP,T,"RP")

final <- rbind(df1,df2)

levels(final$variable) <- c(levels(final$variable),"graph2vec","AR", "CM","node2vec", "struct2vec", "AWE") 
final$variable[final$variable == 'cf4vec'] <- 'graph2vec'
final$variable[final$variable == 'AR'] <- 'AR'
final$variable[final$variable == 'Metalearning'] <- 'CM'
final$variable[final$variable == 'node2vec'] <- 'node2vec'
final$variable[final$variable == 'struct2vec'] <- 'struct2vec'
final$variable[final$variable == 'AWE'] <- 'AWE'

make_graph(final)


