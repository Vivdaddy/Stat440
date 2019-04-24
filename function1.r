
'%!in%' <- function(x,y)!('%in%'(x,y))

find_complete_subgraphs <- function(data, min.max = c(451,452)){
  # data <- graph_from_data_frame(data, directed = F)
  
  graph <- list(data)
  complete_subgraphs <- list(list())
  vertices_csg <- list(list())
  leftout <- list(c())
    
  iter <<- 1
  # this while loop is defined for some sort of stopping criteria
  
  while(min.max[1] > 2){
  # while(length(unique(c(graph[[iter]][,1], graph[[iter]][,2]))) > 2000){
  # while(min.max[2] > 100){
    complete_subgraphs[[iter]] <- list()
    while(length(complete_subgraphs[[iter]]) == 0){
      complete_subgraphs[[iter]] <- cliques(graph_from_data_frame(graph[[iter]], directed=F), min = min.max[1], max=min.max[2])
      min.max <- min.max-1
    }

    # complete_subgraphs updates before the other two lists
    # over any one iteration, the length of the graph list is ahead of other two lists by 1
    if(length(complete_subgraphs[[iter]]) > 1){
      warning("special case")
      graph[[(iter+1)]] <- list()
      vertices_csg[[iter]] <- list()
      leftout[[iter]] <- list()
      for(i in 1:length(complete_subgraphs[[iter]])){
        max.ind <- i
        # get vertices for maximal clique     #tryCatch(
        vert <- as.numeric(vertices(complete_subgraphs[[iter]][[max.ind]])[[1]]) # ,error=function(e) stop("COCK SUCK"))
        vertices_csg[[iter]][[i]] <- unique(c(graph[[iter]][,1], graph[[iter]][,2]))[vert] #, error=function(e) return(list(graph[-1], complete_subgraphs, vertices_csg)))
        
        # we want the subsetted dataframes in this special case to subset eachother, so as to follow the main iterative algorithm in order to coincide with the topological properties of the graphs that we wish to utilize
        temp2 <- c()
        for(j in 1:i){
          # tryCatch(temp2 <- unique(c(temp2, vertices_csg[[iter]][[j]])), error=function(e) return(list(graph[-1], complete_subgraphs, vertices_csg)))
          temp2 <- c(temp2, vertices_csg[[iter]][[j]])
        }
        temp2 <- unique(temp2)
        
        # tryCatch(graph[[(iter+1)]][[i]] <- graph[[iter]][-which(graph[[iter]][,1] %in% temp2 | graph[[iter]][,2] %in% temp2), ], error=function(e) return(list(graph[-1], complete_subgraphs, vertices_csg)))
        graph[[(iter+1)]][[i]] <- graph[[iter]][-which(graph[[iter]][,1] %in% temp2 | graph[[iter]][,2] %in% temp2), ]

        temp3 <- unique(c(graph[[(iter)]][,1], graph[[(iter)]][,2])) # this is all of the nodes in the graph that just got subsetted
        temp3 <- temp3[which(temp3 %!in% unique(c(graph[[(iter+1)]][[i]][,1], graph[[(iter+1)]][[i]][,2])))] # this is all of the nodes that we left behind: the vertices and some first neighbors who didnt neighbor anyone else
        leftout[[iter]] <- temp3[which(temp3 %!in% vertices_csg[[iter]][[i]])] #these are those neighbors who were left behind
      }
      # we want to leave the function at this point:
      return(list(graph[-1], complete_subgraphs, vertices_csg, leftout))
    }
    
    max.ind <- 1
    # for(i in 1:length(complete_subgraphs[[iter]])){
    #   if(length(complete_subgraphs[[iter]][[i]]) >= length(complete_subgraphs[[iter]][[max.ind]])){
    #     max.ind <- i
    #   }
    # }
    vert <- as.numeric(vertices(complete_subgraphs[[iter]][[max.ind]])[[1]]) # get vertices for maximal clique
    vertices_csg[[iter]] <- unique(c(graph[[iter]][,1], graph[[iter]][,2]))[vert]
        
    # We want an edgelist (with edges) for all of these vertices:
    graph[[(iter+1)]] <- graph[[iter]][-which(graph[[iter]][,1] %in% vertices_csg[[iter]] | graph[[iter]][,2] %in% vertices_csg[[iter]]), ]  # graph[[(iter+1)]] <- graph[[(iter+1)]][-which(graph[[(iter+1)]][,2] %in% vertices_csg[[iter]]), ]
    
    temp1 <- unique(c(graph[[(iter)]][,1], graph[[(iter)]][,2])) # this is all of the nodes in the graph that just got subsetted
    temp1 <- temp1[which(temp1 %!in% unique(c(graph[[(iter+1)]][,1], graph[[(iter+1)]][,2])))] # this is all of the nodes that we left behind: the vertices and some first neighbors who didnt neighbor anyone else
    leftout[[iter]] <- temp1[which(temp1 %!in% vertices_csg[[iter]])] #these are those neighbors who were left behind
    # warning(length(unique(c(graph[[(iter)]][,1], graph[[(iter)]][,2]))) - length(unique(c(graph[[(iter+1)]][,1], graph[[(iter+1)]][,2]))) - length(vertices_csg[[iter]]))
    
    iter <<- iter + 1
    # if(length(unique(c(graph[[iter]][,1], graph[[iter]][,2]))) == (nrow(graph[[iter]])*2)){
    #   # stopping criteria
    #   break
    # }
    warning("iter")
  }
  return(list(graph[-1], complete_subgraphs, vertices_csg, leftout))
}


plz <- find_complete_subgraphs(data2)
warnings()

nrow(plz[[1]][[5]][[1]])
nrow(plz[[1]][[5]][[2]])
length(unique(c(plz[[1]][[5]][[2]][,1], plz[[1]][[5]][[2]][,2])))
length(unique(c(plz[[1]][[5]][[1]][,1], plz[[1]][[5]][[1]][,2])))
length(unique(c(plz[[1]][[5]][[2]][,1], plz[[1]][[5]][[2]][,2], plz[[1]][[5]][[1]][,1], plz[[1]][[5]][[1]][,2])))
# ^ we see that its subsetted, as it should have done in the instance of a "special case"

while(T){
  plz
}

yay2 <- find_complete_subgraphs(plz[[1]][[5]][[1]], min.max = c(185,186))

length(yay[[3]][[5]])

unique.pairs <- c(0)
for(i in 2:length(yay[[3]][[5]])){
  unique.pairs <- c(unique.pairs, sum(yay[[3]][[5]][[1]] %in% yay[[3]][[5]][[i]]))
}
# 1 doesnt have anything in common with the rest!
for(i in 3:length(yay[[3]][[5]])){
  unique.pairs <- c(unique.pairs, sum(yay[[3]][[5]][[2]] %in% yay[[3]][[5]][[i]]))
}
for(i in 4:length(yay[[3]][[5]])){
  unique.pairs <- c(unique.pairs, sum(yay[[3]][[5]][[3]] %in% yay[[3]][[5]][[i]]))
}
for(i in 5:length(yay[[3]][[5]])){
  unique.pairs <- c(unique.pairs, sum(yay[[3]][[5]][[4]] %in% yay[[3]][[5]][[i]]))
}
unique.pairs <- c(unique.pairs, sum(yay[[3]][[5]][[5]] %in% yay[[3]][[5]][[6]]))
unique.pairs



hap1 <- find_complete_subgraphs(yay2[[1]][[6]][[6]], min.max = c(98,99))
hap2 <- find_complete_subgraphs(hap1[[1]][[4]][[2]], min.max = c(82,83))

length(unique(c(hap1[[1]][[4]][[2]][,1], hap1[[1]][[4]][[2]][,2])))
