library(recommenderlab)
library(foreach)
library(e1071) 
library(entropy)
library(ineq)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Combine <- function(data,mf,name){
  mf[paste(name,"max",sep="_")] = max(data)
  mf[paste(name,"min",sep="_")] = min(data)
  mf[paste(name,"mean",sep="_")] = mean(data)
  mf[paste(name,"sd",sep="_")] = sd(data)
  mf[paste(name,"median",sep="_")] = median(data)
  mf[paste(name,"mode",sep="_")] = Mode(data)
  mf[paste(name,"entropy",sep="_")] = entropy(data)
  mf[paste(name,"gini",sep="_")] = ineq(data,type="Gini")
  mf[paste(name,"skewness",sep="_")] = skewness(data)
  mf[paste(name,"kurtosis",sep="_")] = kurtosis(data)
}

CombineRow <- function(data,mf){
  Combine(unlist(rowRatingsFunc(matrix,max)),mf,"rowRatings_max")
  Combine(unlist(rowRatingsFunc(matrix,min)),mf,"rowRatings_min")
  Combine(unlist(rowRatingsFunc(matrix,mean)),mf,"rowRatings_mean")
  #Combine(unlist(rowRatingsFunc(matrix,sd)),mf,"rowRatings_sd")
  Combine(unlist(rowRatingsFunc(matrix,median)),mf,"rowRatings_median")
  Combine(unlist(rowRatingsFunc(matrix,Mode)),mf,"rowRatings_mode")
  #Combine(unlist(rowRatingsFunc(matrix,entropy)),mf,"rowRatings_entropy")
  #Combine(unlist(rowRatingsFunc(matrix,ineq)),mf,"rowRatings_gini")
  #Combine(unlist(rowRatingsFunc(matrix,skewness)),mf,"rowRatings_skewness")
  #Combine(unlist(rowRatingsFunc(matrix,kurtosis)),mf,"rowRatings_kurtosis")
  Combine(unlist(rowRatingsFunc(matrix,sum)),mf,"rowRatings_sum")
  Combine(unlist(rowRatingsFunc(matrix,length)),mf,"rowRatings_count")
}

CombineCol<- function(data,mf){
  Combine(unlist(colRatingsFunc(matrix,max)),mf,"colRatings_max")
  Combine(unlist(colRatingsFunc(matrix,min)),mf,"colRatings_min")
  Combine(unlist(colRatingsFunc(matrix,mean)),mf,"colRatings_mean")
  #Combine(unlist(colRatingsFunc(matrix,sd)),mf,"colRatings_sd")
  Combine(unlist(colRatingsFunc(matrix,median)),mf,"colRatings_median")
  Combine(unlist(colRatingsFunc(matrix,Mode)),mf,"colRatings_mode")
  #Combine(unlist(colRatingsFunc(matrix,entropy)),mf,"colRatings_entropy")
  #Combine(unlist(colRatingsFunc(matrix,ineq)),mf,"colRatings_gini")
  #Combine(unlist(colRatingsFunc(matrix,skewness)),mf,"colRatings_skewness")
  #Combine(unlist(colRatingsFunc(matrix,kurtosis)),mf,"colRatings_kurtosis")
  Combine(unlist(colRatingsFunc(matrix,sum)),mf,"colRatings_sum")
  Combine(unlist(colRatingsFunc(matrix,length)),mf,"colRatings_count")
}

rowRatingsFunc <- function(matrix,FUN){
  foreach(i = 1:as.numeric(dim(matrix)[1])) %do% FUN(getRatings(matrix[i,]))  
}

colRatingsFunc <- function(matrix,FUN){
  foreach(i = 1:as.numeric(dim(matrix)[2])) %do% FUN(getRatings(matrix[,i]))
}

userMatrix <- function(matrix, FUN){
  result <- new("dgeMatrix")
  foreach(i = 1:as.numeric(dim(matrix)[1])) %do% {
    foreach(j = 1:as.numeric(dim(matrix)[1])) %do% {
      result[i,j] <- FUN(matrix[i,],matrix[,j])
    }
  }
  return (result) 
}

saveToFile <- function(hash_mf,name,dataset){
  val = dataset
  for(i in keys(hash_mf)){
    #print(i)
    val = paste(val,hash_mf[[i]],sep=";")
  }
  write(x=val,file=name,append=TRUE,sep=";")
}

saveHeader <- function(hash_mf,name){
  val = "dataset"
  for(i in keys(hash_mf)){
    #print(i)
    val = paste(val,i,sep=";")
  }
  write(x=val,file=name,append=TRUE,sep=";")
}


Combine_StrategyC <- function(data,mf,name){
  mf[paste(name,"gini",sep="")] = ineq(data,type="Gini")
  mf[paste(name,"skewness",sep="")] = skewness(data)
}


