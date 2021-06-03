library(recommenderlab)
library(hash)
library(e1071) 
library(foreach)
library(doMC)
registerDoMC(3)
#source("/home/tiago-inesc/Dropbox/doutoramento/source_code/meta_level/meta_empirical_study/auxiliary.R")
source("auxiliary_statistical.R")
#data_folder <- "/home/tiago-inesc/Dropbox/doutoramento/source_code/meta_level/cf_journal/datasets/"

#Cunha2016
strategyA <- function(data, matrix, new_matrix){
  mf <- hash()
  mf$nusers <- as.numeric(dim(matrix)[1])
  mf$nitems <- as.numeric(dim(matrix)[2])
  mf$nratings <- as.numeric(nratings(matrix))
  mf$sparsity <- 1 - mf$nratings / (mf$nusers*mf$nitems)  #\cite{Massa2007,Huang2008a} 
  
  #DF statistics - POST-PROCESSING
  Combine(data[,3],mf,"ratings") #getRatings(matrix) does not work well with large datasets
  
  #DF-ROW statistics
  Combine(rowCounts(matrix),mf,"rowCounts")      #number of ratings per row
  Combine(rowMeans(matrix),mf,"rowMeans")        #row-wise rating means (excluding empty-cells)
  Combine(rowSums(new_matrix),mf,"rowSums")      #sum of all ratings per row
  
  #DF-COL statistics
  Combine(colCounts(matrix),mf,"colCounts")      #number of ratings per column
  Combine(colMeans(matrix),mf,"colMeans")        #column-wise rating means (excluding empty-cells)
  Combine(colSums(new_matrix),mf,"colSums")      #sum of all ratings per column
  
  print(mf)
  
  if(!header1){
    saveHeader(mf,"mf_A.csv")
    header1 <<- TRUE
  }
  saveToFile(mf,"mf_A.csv", files[i])
}

#Matuszyk2014
strategyB <- function(data, matrix, new_matrix){
  #sample users
  nusers <- as.numeric(dim(matrix)[1])
  all_users <- unique(data[,1])
  
  if(threshold_user<=nusers)
    users <- sample(all_users, threshold_user) 
  else
    users <- all_users
  #print(users)
  
  #assign users to items
  assignments <- hash()
  assignments_count <- hash()
  lapply(users,
         function(user){
           assignments[user] = data[which(data[,1]==user),2]
           assignments_count[user] = length(data[which(data[,1]==user),2])
         })
  #print(assignments)
  #print(assignments_count)
  
  #assign users to preference classes
  preference_classes = hash()
  max_class = max(values(assignments_count))

  lapply(keys(assignments_count),function(key){
    val = toString(values(assignments_count, keys=key)[[1]])

    if(has.key(val, preference_classes)){
      preference_classes[val] = c(values(preference_classes, keys = val), key)
    }
    else{
      preference_classes[val] = c(key)
    }
  })
  #print(preference_classes)
  
  #create co-ratings matrix equivalence classes x equivalence classes
  coratings <- matrix(NA,max_class,max_class)
  
  #assign the average number of co-rated items in common among its users
  lapply(1:max_class, function(class1){
    lapply(1:max_class, function(class2){
      
      if(is.na(coratings[class1,class2]) && is.na(coratings[class2,class1])){
      
        #get users
        users_class1 = c()
        users_class2 = c()
        
        if(has.key(toString(class1),preference_classes))
          users_class1 = values(preference_classes, keys=toString(class1))
        
        if(has.key(toString(class2),preference_classes))
          users_class2 = values(preference_classes, keys=toString(class2))
  
        if(length(users_class1) != 0 && length(users_class2) != 0){
        
        #compare each user of different class
        co_rated = mean(unlist(lapply(users_class1, function(user1){
          lapply(users_class2, function(user2){
            #items of each user
            items_users1 = values(assignments, keys=user1)
            items_users2 = values(assignments, keys=user2)
  
            #count number of elements in common
            length(intersect(items_users1,items_users2))
          })
        })))
  
        #print(co_rated)
        coratings[class1,class2]<<-co_rated
        coratings[class2,class1]<<-co_rated
          
        }
      }
    })
  })
  
  #remove NA rows/columns
  tmp = data.frame(coratings)
  tmp1<-tmp[,-which(apply(tmp,2,function(x)all(is.na(x))))]
  coratings_df <-as.matrix(tmp1[-which(apply(tmp1,1,function(x)all(is.na(x)))),])
  #print(coratings_df)
  
  #extract metafeatures
  mf = hash()
  
  nusers <- as.numeric(dim(matrix)[1])
  nitems <- as.numeric(dim(matrix)[2])
  nratings <- as.numeric(nratings(matrix))
  mf$sparsity <- 1 - nratings / (nusers*nitems)  #\cite{Massa2007,Huang2008a} 
  
  mf$entropy <- entropy(coratings_df)
  mf$gini <- ineq(coratings_df,type="Gini")
  
  print (mf)
  
  if(!header2){
    saveHeader(mf,"mf_B.csv")
    header2 <<- TRUE
  }
  saveToFile(mf,"mf_B.csv", files[i])
}

#Adomavicius2012
strategyC <- function(data, matrix, new_matrix){
  mf = hash()
  
  nusers <- as.numeric(dim(matrix)[1])
  nitems <- as.numeric(dim(matrix)[2])
  nratings <- as.numeric(nratings(matrix))
  mf$density <- nratings / (nusers*nitems) 
  
  Combine_StrategyC(rowCounts(matrix),mf,"user.count.")
  Combine_StrategyC(colCounts(matrix),mf,"item.count.")
  mf$variance = var(data[,3])
  
  print(mf)
  
  if(!header3){
    saveHeader(mf,"mf_C.csv")
    header3 <<- TRUE
  }
  saveToFile(mf,"mf_C.csv", files[i])
}

#Griffith2012
strategyD <- function(data, matrix, new_matrix){
  
  mf = hash()
  
  mf$user.count.mean = mean(rowCounts(matrix)) #validado #1
  mf$user.mean.mean = mean(rowMeans(matrix)) #validado #2
  mf$item.count.mean = mean(colCounts(matrix)) #validado #7
  mf$item.mean.mean = mean(colMeans(matrix)) #validado #8
  
  #METAFEATURES WITH SAMPLE OF USERS
  nusers <- as.numeric(dim(matrix)[1])
  all_users <- unique(data[,1])
  if(threshold_user<=nusers)
    users <- sample(all_users, threshold_user) 
  else
    users <- all_users
  #print(users)
  
  #user.number_neighbours.mean
  data_sample <- data[which(data[,1] %in% users),]
  rownames(data_sample) <- c()
  items <- unique(data_sample[,2])
  #print(data_sample)
  matrix_sample <- as(data_sample, "realRatingMatrix")
  matrix_sample_base <- as(as(matrix_sample, "dgCMatrix"),"matrix")   #package Matrix
  #print(matrix_sample_base)
  
  #user.standard deviation.mean
  user_ratings <- hash()
  lapply(users,function(user){
           user_ratings[toString(user)] = data_sample[which(data_sample[,1]==toString(user)),3]
         })
  tmp = unlist(
        lapply(users, function(key){
          val <- values(user_ratings, keys=toString(key))
          sd(val)
        }
  ))
  mf$user.standard_deviation.mean = mean(tmp, na.rm=TRUE) #validado #3
  
  
  pearson <- matrix(NA, length(users), length(users))
  lapply(1:length(users), function(user1){
    lapply(1:length(users), function(user2){
      values_user1 <- matrix_sample_base[rownames(matrix_sample_base)[user1], ]
      values_user2 <- matrix_sample_base[rownames(matrix_sample_base)[user2], ]
      pearson[user1,user2] <<- abs(cor(values_user1,values_user2,method="pearson"))
    })
  })
  #print(pearson)
  
  neighbors = hash()
  lapply(1:length(rownames(matrix_sample_base)), function(user){ 
    val <- as.numeric(pearson[user,])
    selected_indexes <- unlist(lapply(val, function(x){
      x > 0.1 #value dictated by original paper
    }))
    current_user = rownames(matrix_sample_base)[user]
    neighbors_user = rownames(matrix_sample_base)[selected_indexes]
    neighbors_user = neighbors_user[neighbors_user != current_user]
    if(!identical(neighbors_user, character(0))){
      neighbors[current_user] = neighbors_user 
    }
  })
  #print(neighbors)
  
  #user.number_neighbours.mean
  mf$user.number_neighbours.mean <- mean(unlist(lapply(1:length(users), function(user){ #validado #4
    if(has.key(toString(users[user]),neighbors)){
      neighbors_user = values(neighbors, keys=users[user])
      length(neighbors_user)
    }
    else {
      0
    }
  })))
  
  #user.average_similarity.mean
  mf$user.average_similarity.mean <- mean(unlist(lapply(1:length(users), function(user){ #validado #5
    mean(sort(as.numeric(pearson[user,]), decreasing = T)[1:10])  #chose 10 instead of 30
  })))
  
  #user.clustering_coefficient.mean
  tmp <- unlist(lapply(1:length(rownames(matrix_sample_base)), function(i){
    user = rownames(matrix_sample_base)[i]
    if(has.key(user,neighbors)){
      neighbors_user = values(neighbors, keys=user)
      n = length(neighbors_user)

      connections = hash()
      lapply(neighbors_user, function(neighbor){
        if(has.key(neighbor,neighbors)){
          new_neighbors = values(neighbors, keys=neighbor)
          res = intersect(new_neighbors,neighbors_user)

          if(!identical(res, character(0))){
            connections[neighbor] = res
          }
        }
      })
      #print(connections)
      actual <- length(unique(c(keys(connections), unlist(values(connections))))) #any connection counts!
      (2*actual+n) / (n*n - n)
    }
  }))
  mf$user.clustering_coefficient.mean <- mean(replace(tmp, tmp==Inf, 0))    #validado #6
  
  #user.TFIDF.mean 
  IDF <- as.numeric(colCounts(matrix_sample))
  mf$user.TFIDF.mean <- mean(unlist(lapply(1:length(rownames(matrix_sample_base)), function(index){  #validado #9
    val <- as.numeric(matrix_sample_base[index,])
    val[which(val > 0)] <- 1  
    sum(val/IDF)
  })))
  
  #user_coratings.jaccard.mean
  user_items <- hash()
  lapply(users,function(user){
           user_items[toString(user)] = data_sample[which(data_sample[,1]==toString(user)),2]
         })
  #print(user_items)
  
  mf$user_coratings.jaccard.mean <- mean(unlist(lapply(users, function(user1){  #validado #10
    lapply(users, function(user2){
      if(user1 != user2){
        user1_items = values(user_items, keys=user1)
        user2_items = values(user_items, keys=user2)

        items_intersection = length(intersect(user1_items,user2_items))
        items_union = length(union(user1_items,user2_items))
        items_intersection/items_union
      }
      else {
        0
      }
    })
  })))
  
  #item.entropy.mean
  item_ratings <- hash()
  lapply(items,function(item){
    item_ratings[toString(item)] = data_sample[which(data_sample[,2]==toString(item)),3]
  })
  #print(item_ratings)
  
  mf$item.entropy.mean <- mean(unlist(lapply(items, function(item){ #validado #11
    val <- values(item_ratings, keys=item)
    entropy(val)
  })))
  
  print(mf)

  if(!header4){
    saveHeader(mf,"mf_D.csv")
    header4 <<- TRUE
  }
  saveToFile(mf,"mf_D.csv", files[i])
  
}


#Ekstrand2012
strategyE <- function(data, matrix, new_matrix){
  mf = hash()
  all_users <- unique(data[,1])
  nusers <- as.numeric(dim(matrix)[1])
  if(threshold_user<=nusers)
    users <- sample(all_users, threshold_user) 
  else
    users <- all_users
  
  data_sample <- data[which(data[,1] %in% users),]
  rownames(data_sample) <- c()
  
  user_ratings <- hash()
  lapply(users,function(user){
    user_ratings[toString(user)] = data_sample[which(data_sample[,1]==toString(user)),3]
  })

  mf$user.count.mean <- mean(rowCounts(matrix))
  mf$user.mean.mean <- mean(rowMeans(new_matrix))
  mf$user.variance.mean <- mean(unlist(lapply(users, function(user){
    val <- values(user_ratings,keys=user)
    var(val)
  })), na.rm=TRUE)
  
  print(mf)
  
  if(!header5){
    saveHeader(mf,"mf_E.csv")
    header5 <<- TRUE
  }
  saveToFile(mf,"mf_E.csv", files[i])
  
}


################ MAIN

header1=FALSE
header2=FALSE
header3=FALSE
header4=FALSE
header5=FALSE

timeA = c()
timeB = c()
timeC = c()
timeD = c()
timeE = c()

threshold_user = 10 #0.1*length(all_users)

# files <- dir(data_folder)
# for(i in 1:length(files)){ 
#   print(files[i]) 
  
  #data <- read.csv(paste(data_folder, "yahoo-music.csv", sep=""),sep=";")
  data <- read.csv("yahoo-music.csv",sep=";")
  matrix <- as(data, "realRatingMatrix")  #package recommenderlab
  new_matrix <- as(matrix, "dgCMatrix")   #package Matrix

  tStartA <- proc.time()
  strategyA(data, matrix, new_matrix)
  tFinishA <- proc.time()
  timeA = c(timeA, as.numeric((tFinishA - tStartA)["elapsed"]))
  
  # tStartB <- proc.time()
  # strategyB(data, matrix, new_matrix)
  # tFinishB <- proc.time()
  # timeB = c(timeB, as.numeric((tFinishB - tStartB)["elapsed"]))
  # 
  # tStartC <- proc.time()
  # strategyC(data, matrix, new_matrix)
  # tFinishC <- proc.time()
  # timeC = c(timeC, as.numeric((tFinishC - tStartC)["elapsed"]))
  # 
  # tStartD <- proc.time()
  # strategyD(data, matrix, new_matrix)
  # tFinishD <- proc.time()
  # timeD = c(timeD, as.numeric((tFinishD - tStartD)["elapsed"]))
  # 
  # tStartE <- proc.time()
  # strategyE(data, matrix, new_matrix)
  # tFinishE <- proc.time()
  # timeE = c(timeE, as.numeric((tFinishE - tStartE)["elapsed"]))
# }

print(timeA)
# print(timeB)
# print(timeC)
# print(timeD)
# print(timeE)
# 
# time_results <- data.frame(strategies = c("A","B","C","D","E"),
#                            total = c(sum(timeA),sum(timeB),sum(timeC),sum(timeD),sum(timeE)),
#                            average = c(mean(timeA),mean(timeB),mean(timeC),mean(timeD),mean(timeE)))
# 
# print(time_results)




