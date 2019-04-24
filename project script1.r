
data1 <- read.csv("file:///C:/Users/ryan voyack/Documents/PSU (courses,etc) & Act-Sci (exams, etc)/Courses Undergrad (R; STAT)/Stat 440/Stat440/data/1-edges-2008.csv", header = F)
data2 <- read.csv("file:///C:/Users/ryan voyack/Documents/PSU (courses,etc) & Act-Sci (exams, etc)/Courses Undergrad (R; STAT)/Stat 440/Stat440/data/2-edges-2008.csv", header = F)
data3 <- read.csv("file:///C:/Users/ryan voyack/Documents/PSU (courses,etc) & Act-Sci (exams, etc)/Courses Undergrad (R; STAT)/Stat 440/Stat440/data/3-edges-2008.csv", header = F)
data4 <- read.csv("file:///C:/Users/ryan voyack/Documents/PSU (courses,etc) & Act-Sci (exams, etc)/Courses Undergrad (R; STAT)/Stat 440/Stat440/data/4-edges-2008.csv", header = F)
data5 <- read.csv("file:///C:/Users/ryan voyack/Documents/PSU (courses,etc) & Act-Sci (exams, etc)/Courses Undergrad (R; STAT)/Stat 440/Stat440/data/5-edges-2008.csv", header = F)
nodes_vector <- read.csv("file:///C:/Users/ryan voyack/Documents/PSU (courses,etc) & Act-Sci (exams, etc)/Courses Undergrad (R; STAT)/Stat 440/Stat440/data/nodes-2008.csv", header=F)




# install.packages("GGally")
# devtools::install_github("briatte/ggnet")
# install.packages("network")
# install.packages("sna")
library("GGally")
library("ggnet")
library("sna")
library("network")
library("ggplot2")
library("magrittr")
library(igraph)
#### https://briatte.github.io/ggnet/ ####


hi <- matrix(
  c(1,2,3,4,
    2,4,5,6,
    3,5,4,0,
    4,6,0,1), 4, 4)
ggnet2(network(hi, ignore.eval=F, names.eval="weights"), edge.size="weights", label=T, directed=F)

hi2 <- network(hi, ignore.eval=F, names.eval="weights", directed=F)
net <- graph_from_incidence_matrix(hi2)

yo <- layout.circle(net)
plot(net, layout=yo, edge.size="weights", label=T)



# test1 <- data.frame(data2[,1][which(data2[,1] < 10)], data2[,2][which(data2[,1] < 10)], data2[,3][which(data2[,1] < 10)])
# 
# el <- data.frame(test1[,1], test1[,2])
# g <- graph_from_edgelist(as.matrix(el))
# # el[,1]=as.character(el[,1]) #Because the vertex IDs in this dataset are numbers, we make sure igraph knows these should be treated as characters. Otherwise, it'll create problems (see page on data import)
# # el[,2]=as.character(el[,2])
# # el=as.matrix(el) #igraph needs the edgelist to be in matrix format
# # g=graph.edgelist(el[,1:2]) #We first greate a network from the first two columns, which has the list of vertices
# E(g)$weight=as.numeric(test1[,3]) #We then add the edge weights to this network by assigning an edge attribute called 'weight'
# yo <- layout.circle(g)
# # ggnet2(g, mode=yo, edge.size=E(g)$weight)
# 
# net <- network(x=as.matrix(test1), names.eval="weights", matrix.type="edgelist", ignore.eval=F, directed=F)
# # ggnet2(net, edge.size="weights", label=T)







nodes <- unique(c(data2[,1],data2[,2]))
test1 <- data2[c(which(data2[,1] %in% nodes[1:50] & data2[,2] %in% nodes[1:50])), ] 
z <- sort(unique(c(test1[,1],test1[,2])))

net <- network(x=as.matrix(test1), names.eval="weights", matrix.type="edgelist", ignore.eval=F, directed = F)
network.size(net) #...
## HERE, WE RENAME THE NODES SO THAT THEYRE ASCENDING FROM 1 BECAUSE FOR SOME REASON ggnet2 WILL RANDOMLY PLOT ALL (NON-EXISTENT) NODES WITH INTEGER NAMES UP TO AND UNTIL THE LAST NODE NAME
for(i in 1:length(z)){
  temp <- z[i]
  test1[,1][which(test1[,1]==temp)] <- i
  test1[,2][which(test1[,2]==temp)] <- i
}
## OR WE COULD DO THIS:
## (no we cant)
# test1[,1] <- as.character(test1[,1])
# test1[,2] <- as.character(test1[,2])
rownames(test1) <- NULL
colnames(test1)[3] <- "weights"
net <- network(x=as.matrix(test1), names.eval="weights", matrix.type="edgelist", ignore.eval=F, directed=F)
ggnet2(net, edge.size="weights", label=T, edge.color="red")
network.size(net)




#### begin analysis; inspect data sets ####




### http://www.shizukalab.com/toolkits/sna/weighted-edges ###
### http://kateto.net/networks-r-igraph ###
### http://kateto.net/network-visualization ###
### https://igraph.org/r/doc/as_incidence_matrix.html ###


dim(nodes_vector)

min(nodes_vector[,1])
max(data2[,1])
length(unique(data2[,1]))
length(unique(data2[,2]))

# data1 <- read.csv("file:///C:/Users/ryan voyack/Documents/PSU (courses,etc) & Act-Sci (exams, etc)/Courses Undergrad (R; STAT)/Stat 440/Stat440/data/1-edges-2008.csv")

dim(data1)
data1[,3]


# max number of rows possible in the csv's given to us
(15088*15089)/2-1
(15088 + 2)/2*15087 
temp <- 0
for(i in 2:15088){
  temp <- temp + i
  if(i==15088){
    print(temp)
  }
}
# max number of rows possible in the csv's given to us



getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(data1[,1])
sum(data1[,1]==162) # user 162 has most interactions with other users, at a frequecny of 523 other users
getmode(data2[,1])
sum(data2[,1]==109)
min(unique(data3[,1]))
min(data4[,1])
min(nodes_vector)





##### logistic regression to use 3 and 5 as predictors to predict 1 ####


data1[,4] <- "data1"
data3[,4] <- "data3"
data4[,4] <- "data4"

temp <- rbind(data3, data4, data1)
temp2 <- paste0(temp[,1], ",", temp[,2])
temp[,5] <- temp2
colnames(temp)[3] <- "value"
colnames(temp)[4] <- "data"
colnames(temp)[5] <- "pair"
temp <- temp[,c(3,4,5)]
head(temp)
library(tidyr)
reg.data <- spread(temp, key=data, value=value)
head(reg.data, n=20)

reg.data[which(is.na(reg.data[,2])),2] <- 0
reg.data[which(is.na(reg.data[,3])),3] <- 0
reg.data[which(is.na(reg.data[,4])),4] <- 0

length(which(reg.data[,2] != 0 & reg.data[,3] != 0 & reg.data[,4] != 0)) #how many rows are completely non-zero


# set aside a "training" portion of the data set
set.seed(440)
samp <- sample(c(1:nrow(reg.data)), nrow(reg.data)*.75, replace = F, prob = NULL)
train <- reg.data[samp, ]

n <- nrow(train)
np <- mean(train[,"data1"])
npq <- var(train[,"data1"])

((1-np/n)*np - npq) # expected variance given mean under binomial distribution - variance

# we see this is very close, so we fit binomial family (logistic) regression,
# and we dont need to count for over dispersion as our response is almost perfectly normal


fit2 <- glm(data1 ~ data3 + data4, family=binomial, data=train)
summary(fit2)

fit1 <- glm(data1 ~ data4, family=binomial, data=train)
summary(fit1)

fit0 <- glm(data1 ~ 1, family=binomial, data=train)
anova(fit0, fit1, fit2, test = "LRT")
# we conclude that the model with data4 as a predictor only is better because of this drop in deviance test p-value (using LRT)





# first, lets consider how the entire data frame shapes up
sum(reg.data[,"data1"]==0)/nrow(reg.data) # we see that 98.91% of the data has zeros for the response variable, "data1"
# our response variable is what we would call a rare event, so we could use a logistic regresson model that penalizes for this, such as firth's method
# ^ just to point out this special case


#### now, use predict equation to see model performance####
glm.probs <- predict(fit2, type="response")
glm.train.pred <- ifelse(glm.probs > 0.5, 1, 0)


table(glm.train.pred, train[,"data1"])
mean(glm.train.pred==train[,"data1"])
(sensitivity <- 349/(349+57471))
(specificity <- 5230724/(1058+5230724))


#### Now we test our model!####
test <- reg.data[-c(samp), ]
# use predict equation
glm.probs <- predict(fit2, newdata=test, type="response")
glm.test.pred <- ifelse(glm.probs > 0.5, 1, 0)


table(glm.test.pred, test[,"data1"])
mean(glm.test.pred==test[,"data1"])
(sensitivity <- 128/(128+18817))
(specificity <- 1743911/(345+1743911))



#### Do entire data fit ####
fit <- glm(data1 ~ data3 + data4, family=binomial, data=reg.data)
glm.probs <- predict(fit, type="response")
glm.pred <- ifelse(glm.probs > 0.5, 1, 0)


table(glm.pred, reg.data[,"data1"])
mean(glm.pred==reg.data[,"data1"])
(sensitivity <- 477/(477+76288)) #probability that a test result (prediction) will be positive when the disease is present (1 when 1)
(specificity <- 6974636/(1402+6974636)) #probability that a test result will be negative when the disease is not present (0 when 0)



#### FIRTH FOR RARE EVENTS ####
library(logistf)
# only works for data set the size of our "test" set
fitf <- logistf(formula = data1 ~ data3 + data4, data=test, firth=T)
summary(fitf) # significant
glm.probs <- fitf$predict
glm.pred <- ifelse(glm.probs > 0.5, 1, 0)


table(glm.pred, test[,"data1"])
mean(glm.pred==test[,"data1"])
(sensitivity <- 125/(125+18820)) #probability that a test result (prediction) will be positive when the disease is present (1 when 1)
(specificity <- 1743925/(331+1743925)) #probability that a test result will be negative when the disease is not present (0 when 0)
# ^ now we *know* this model has bad predictive power (after doing og model and firth's)

# library(pROC)
# ROC <- pROC::roc(reg.data$data1 ~ predict(fit2, type="response"))
# plot(ROC)
# library(AUC)
# auc(AUC::roc(reg.data$data1, predict(fit2, type="response")))



#### Network analysis ####

# data1 !

degree <- degree(graph_from_data_frame(data1))
# length(unique(c(data1[,1], data1[,2])))
plot.ts(degree)
ind <- as.numeric(names(which(max(degree)==degree)))

# check to see if the degree function works/see what it does
yes <- 0
for(i in 1:nrow(data1)){
  if(ind %in% data1[i,]){
    yes <- yes + 1
  }
}
sum(data1[,1] == ind) + sum(data1[,2] == ind)
# maybe we take the top 10 dudes here and see whats good with them

top.dogs <- as.character(as.numeric(names(rev(sort(degree))[1:20])))
plot.ts(degree[top.dogs])

dogs <- matrix(0, ncol=3)
for(i in 1:nrow(data1)){
  if(rev(sort(top.dogs %in% as.numeric(data1[i,])))){
    dogs <- rbind(dogs, as.numeric(data1[i,]))
  }
}
dogs <- dogs[-1,]

bet <- betweenness(graph_from_data_frame(as.matrix(dogs)), directed = F) # DONT DO THIS WITH DATA1 BECAUSE [,3] IS ALL 1'd - THIS IS NOT AN EDGELIST WITH EDGE SIZES
bet[which(as.character(top.dogs) %in% names(bet))]
# ^ this is misleading and not useful



rownames(dogs) <- NULL
colnames(dogs)[3] <- "weights"
net <- network(x=as.matrix(dogs), names.eval="weights", matrix.type="edgelist", ignore.eval=F, directed = F)
network.size(net)
ggnet2(net, edge.size="weights", label=T, edge.color="steelblue")
max(c(dogs[,1], dogs[,2]))

# HERE, WE RENAME THE NODES SO THAT THEYRE ASCENDING FROM 1 BECAUSE FOR SOME REASON ggnet2 WILL RANDOMLY PLOT ALL (NON-EXISTENT) NODES WITH INTEGER NAMES UP TO AND UNTIL THE LAST NODE NAME
z <- sort(unique(c(dogs[,1], dogs[,2])))
test1 <- dogs
temp <- 0
top.dog.ids <- c()
for(i in 1:length(z)){
  temp <- z[i]
  if(i <= 3478){
    test1[,1][which(test1[,1]==temp)] <- i
    test1[,2][which(test1[,2]==temp)] <- i
  }else if(length(which(test1[,2]==temp))==0){
    warning(i, z[i])
  }else{
    test1[,2][which(test1[,2]==temp)] <- i
  }
  if(z[i] %in% as.numeric(top.dogs)){top.dog.ids <- c(top.dog.ids, i)}
}
# network.size(net)
# length(unique(c(test1[,1], test1[,2])))
# max(unique(c(test1[,1], test1[,2])))
# unique(unique(c(test1[,1], test1[,2])) == 1:3597)
# unique(network.vertex.names(net) == 1:3597) # what we did worked

rownames(test1) <- NULL
colnames(test1)[3] <- "weights"
net <- network(x=as.matrix(test1), names.eval="weights", matrix.type="edgelist", ignore.eval=F, directed=F)
net %v% "id" <- ifelse(c(1:nrow(test1)) %in% top.dog.ids, "top", "not")
ggnet2(net, edge.size="weights", label=F, size=ifelse(net %v% "id" == "top", 9, 1), color=ifelse(net %v% "id" == "not", "red", "grey"), edge.color="steelblue")






#### network eda (? ig) ####
#move towards analysis of nodes


# WHAT DOES IT MEAN THAT THESE YOUTUBE USERS HAVE "FRIENDS", 
# I THOUGHT ALL YOU COULD DO ON YOUTUBE IS SUBSCRIBE? 
# ARE FRIENDS PEOPLE THAT SUBSCRIBE TO EACHOTHER? 
# I DONT KNOW AND I FEAR THAT BECAUSE OF THE SAMPLE STYLE 
# (HOW THE FRIENDS ARE FROM THE 880000 AND NOT THE 15088) WE CANT FIND OUT.

# ^ RESOLVED
# being a "friend" means that the two users are eachothers "contacts", in that they can privately message eachother.

# BUT THIS IS WEIRD BECAUSE IN DATA1, THIS IS THE "CONTACT NETWORK", WHICH IS A TECHNICAL TERM DEFINED IN GRAPH THEORY (MEANING THE MATRIX DESCRIBING IF THE TWO NODES *INTERACTED*)
# SO IN THIS CASE IT COULD MEAN THAT THE USERS ARE EACHOTHERS'S CONTACTS IN THE YOUTUBE CHANNEL SENSE, OR IN SOME SORT OF OTHER REGARD SUCH THAT THEY *INTERACT* IN SOME DIFFERENT (UNDEFINED) MANNER. 

# look at user #10
yes2 <- matrix(0, ncol=3)
# we could write the loops this way because the data sets are sorted numerically (ascending) by the first column,
# and because the networks (datasets) are all symmetric. This saves use alot of time
for(i in 1:min(which(data2[,1]>10))){
  if(10 %in% data2[i,-3]){
    yes2 <- rbind(yes2, data2[i,])
  }
}
yes3 <- matrix(0, ncol=3)
for(i in 1:min(which(data3[,1]>10))){
  if(10 %in% data3[i,-3]){
    yes3 <- rbind(yes3, data3[i,])
  }
}
yes4 <- matrix(0, ncol=3)
for(i in 1:min(which(data4[,1]>10))){
  if(10 %in% data4[i,-3]){
    yes4 <- rbind(yes4, data4[i,])
  }
}
#use these data frames to look at what anyone user may be up to in this network (here we look at 10, who is represented in all three "data" datasets - not all users are represented in all "data" datasets)



# above, when i tried to visualize the graph, I may have come into a road block because i was using data2, a huge edgelist. 
# If i use data1, which has very few nonzero edges, maybe it will render
sum(data1[,3]) # because its only 1s and 0s

library(igraph)
## HERE, WE RENAME THE NODES SO THAT THEYRE ASCENDING FROM 1 BECAUSE FOR SOME REASON ggnet2 WILL RANDOMLY PLOT ALL (NON-EXISTENT) NODES WITH INTEGER NAMES UP TO AND UNTIL THE LAST NODE NAME
z <- sort(unique(c(data1[,1], data1[,2])))
test1 <- data1
for(i in 1:length(z)){
  temp <- z[i]
  test1[,1][which(test1[,1]==temp)] <- i
  test1[,2][which(test1[,2]==temp)] <- i
}
rownames(test1) <- NULL
colnames(test1)[3] <- "weights"
net <- network(x=as.matrix(test1), names.eval="weights", matrix.type="edgelist", ignore.eval=F) # directed = F) # that results in an error for some reason
ggnet2(net, edge.size="weights", label=F, edge.color="red", size=2)

network.size(net)
length(unique(c(test1[,1], test1[,2])))
max(unique(c(test1[,1], test1[,2])))
unique(sort(unique(c(test1[,1], test1[,2]))) == 1:13723) # what we did worked


# d38.1 <- rbind(data1[which(data1[,1]==38),], data1[which(data1[,2]==38),])


# https://cran.r-project.org/web/packages/network/network.pdf
# https://briatte.github.io/ggnet/#node-color-and-size




#### how many friends do these guys actually have ? ####

# data1 # friends within the 15088 # call these the "IN"
# data2 # friends not including the 15088 (out of the 848,003) # call these the "OUT"

# of course we can assume that there's no double counting in the data2 matrix, lets see if that is the case:
sum(data2[,3])
# we see that there is double counting, but not a lot (necessarily)
rate <- sum(data2[,3]) / (848003 - 15088) # we see that (and this is simple measure) each of the OUT seems to be double counted, on average, 6.3286 times


deg <- degree(graph_from_data_frame(data2))
length(unique(c(data2[,1], data2[,2]))) # we see that we got what we wanted from the degree function

hist(deg, breaks=100)
max(deg)

friends^-1





###

# clique_num(graph_from_data_frame(data2, directed=F))
a <- cliques(graph_from_data_frame(data2, directed=F), min=450, max=451)
# it looks like the largest clique is 451, theres only one with that size. This is the largest one.

# f <- get.data.frame(a[[1]], what = "vertices")
max.ind <- 1
for(i in 1:452){
  if(length(a[[i]]) >= length(a[[max.ind]])){
    max.ind <- i
  }
}
length(a[[max.ind]])
# we now know which clique is the maximal clique

vert <- as.numeric(vertices(a[[max.ind]])[[1]]) # get vertices for maximal clique
length(vert)
sum(vert %in% unique(c(data2[,1], data2[,2]))) # != 451
# we see that vertices() returns index numbers!
vertices.451 <- unique(c(data2[,1], data2[,2]))[vert]


tit <- unique(c(which(data2[,1] %in% vertices.451), which(data2[,2] %in% vertices.451)))
hi <- 0
for(i in c(1:452)[-3]){
  ugh <- unique(c(which(data2[,1] %in% a[[i]]), which(data2[,2] %in% a[[i]])))
  hi <- hi + ifelse(length(sort(ugh)%in%sort(tit)) == length(ugh), 1, 0)
}
hi == 451
# ^ Here we see that every clique obtained from the function is a sub-clique of the biggest clique, a[[3]] ; a[[max.ind]]



# our complete graph has 451 vertices, so there are
(y = 451*(451-1)/2) # edges, or, rows in the data2 dataframe that comprise the complete graph
(x = nrow(data2) - y) # how many edges should be in left over graph (dataframe)
# lets get rid of these edges
length(which((data2[,1] %in% vertices.451) & (data2[,2] %in% vertices.451))) == y
temp <- data2[- which((data2[,1] %in% vertices.451) & (data2[,2] %in% vertices.451)), ]
# ^ getting rid of the edges interferes with what other cliques might be, so we need to get rid of these 451 vertices' first neighbors too

temp <- data2[unique(c(which(data2[,1] %in% vertices.451), which(data2[,2] %in% vertices.451))),]
# ^ This includes the CSG and its neighbors, the csg has perimeter (say) with length = 451 nodes.
length(unique(c(temp[,1], temp[,2]))) # there are 8728-451 nodes outside of it that they neighbor. 
temp1 <- unique(c(temp[,1], temp[,2]))
# We want an edgelist with all of these nodes:
neighbor.node.net <- data2[-which(data2[,1] %in% vertices.451), ]
neighbor.node.net <- neighbor.node.net[-which(neighbor.node.net[,2] %in% vertices.451), ]
# neighbor.node.net <- neighbor.node.net[-(unique(c(which(neighbor.node.net[,1] %in% vertices.451), which(neighbor.node.net[,2] %in% vertices.451)))), ]
length(unique(c(data2[,1], data2[,2]))) #recall, originally, there were an original number of nodes:
length(unique(c(neighbor.node.net[,1], neighbor.node.net[,2]))) #and there are now this many nodes:
length(unique(c(data2[,1], data2[,2]))) - length(unique(c(neighbor.node.net[,1], neighbor.node.net[,2]))) # such that we see that there were 10 nodes who only had a sub graphs with the corresponding 451 vertices, we can find these later.



## WE KINDA REPEAT WHAT WE DID BEFORE
# b <- cliques(graph_from_data_frame(neighbor.node.net, directed=F), min = 430, max=449) # returns 0
# b <- cliques(graph_from_data_frame(neighbor.node.net, directed=F), min = 400, max=429) # returns 0
# b <- cliques(graph_from_data_frame(neighbor.node.net, directed=F), min = 390, max=399) # returns 0
b <- cliques(graph_from_data_frame(neighbor.node.net, directed=F), min = 387, max=389) # returns 0


## WE KINDA REPEAT WHAT WE DID BEFORE


max.ind <- 1
for(i in 1:389){
  if(length(b[[i]]) >= length(b[[max.ind]])){
    max.ind <- i
  }
}
length(b[[max.ind]])
# we now know which clique is the maximal clique
# again, all other cliques have length 387, which makes sense

vert <- as.numeric(vertices(b[[max.ind]])[[1]]) # get vertices for maximal clique
length(vert)
vertices.388 <- unique(c(neighbor.node.net[,1], neighbor.node.net[,2]))[vert]


# tit <- unique(c(which(neighbor.node.net[,1] %in% vertices.388), which(neighbor.node.net[,2] %in% vertices.388)))
# hi <- 0
# for(i in c(1:389)[-max.ind]){
#   ugh <- unique(c(which(neighbor.node.net[,1] %in% vertices.388), which(neighbor.node.net[,2] %in% vertices.388)))
#   hi <- hi + ifelse(length(sort(ugh)%in%sort(tit)) == length(ugh), 1, 0)
# }
# hi == 388
# # ^ Here we see that every clique obtained from the function is a sub-clique of the biggest clique, a[[3]] ; a[[max.ind]]


# our complete graph has 388 vertices, so there are
(y = 388*(388-1)/2) # edges, or, rows in the neighbor.node.net dataframe that comprise the complete graph
(x = nrow(neighbor.node.net) - y) # how many edges should be in left over graph (dataframe)
# lets get rid of these edges
length(which((neighbor.node.net[,1] %in% vertices.388) & (neighbor.node.net[,2] %in% vertices.388))) == y
temp <- neighbor.node.net[- which((neighbor.node.net[,1] %in% vertices.388) & (neighbor.node.net[,2] %in% vertices.388)), ]
# ^ getting rid of the edges interferes with what other cliques might be, so we need to get rid of these 451 vertices' first neighbors too

temp <- neighbor.node.net[unique(c(which(neighbor.node.net[,1] %in% vertices.388), which(neighbor.node.net[,2] %in% vertices.388))),]
# ^ This includes the CSG and its neighbors, the csg has perimeter (say) with length = 451 nodes.
length(unique(c(temp[,1], temp[,2]))) # there are 7198-388 nodes outside of it that they neighbor. 
temp1 <- unique(c(temp[,1], temp[,2]))
# We want an edgelist with all of these nodes:
neighbor.node.net2 <- neighbor.node.net[-which(neighbor.node.net[,1] %in% vertices.388), ]
neighbor.node.net2 <- neighbor.node.net2[-which(neighbor.node.net2[,2] %in% vertices.388), ]

length(unique(c(neighbor.node.net[,1], neighbor.node.net[,2]))) #recall, originally, there were an original number of nodes:
length(unique(c(neighbor.node.net2[,1], neighbor.node.net2[,2]))) #and there are now this many nodes:
length(unique(c(neighbor.node.net[,1], neighbor.node.net[,2]))) - length(unique(c(neighbor.node.net2[,1], neighbor.node.net2[,2]))) 
# such that we see that there were 10 nodes who only had a sub graphs with the corresponding 451 vertices, we can find these later.

# now we see what limits to set for  (???)

## WE KINDA REPEAT WHAT WE DID BEFORE
# c <- cliques(graph_from_data_frame(neighbor.node.net2, directed=F), min = 350, max=387) # returns 0
# c <- cliques(graph_from_data_frame(neighbor.node.net2, directed=F), min = 340, max=349) # returns 0
# c <- cliques(graph_from_data_frame(neighbor.node.net2, directed=F), min = 320, max=339) # returns 0
# c <- cliques(graph_from_data_frame(neighbor.node.net2, directed=F), min = 300, max=319) # returns 0
# c <- cliques(graph_from_data_frame(neighbor.node.net2, directed=F), min = 270, max=299) # returns 0
c <- cliques(graph_from_data_frame(neighbor.node.net2, directed=F), min = 268, max=269) # returns 0


max.ind <- 1
for(i in 1:270){
  if(length(c[[i]]) >= length(c[[max.ind]])){
    max.ind <- i
  }
}
length(c[[max.ind]])
# we now know which clique is the maximal clique
# again, all other cliques have length 387, which makes sense

vert <- as.numeric(vertices(c[[max.ind]])[[1]]) # get vertices for maximal clique
length(vert)
vertices.269 <- unique(c(neighbor.node.net2[,1], neighbor.node.net2[,2]))[vert]


# tit <- unique(c(which(neighbor.node.net2[,1] %in% vertices.269), which(neighbor.node.net2[,2] %in% vertices.269)))
# hi <- 0
# for(i in c(1:389)[-max.ind]){
#   ugh <- unique(c(which(neighbor.node.net2[,1] %in% vertices.269), which(neighbor.node.net2[,2] %in% vertices.269)))
#   hi <- hi + ifelse(length(sort(ugh)%in%sort(tit)) == length(ugh), 1, 0)
# }
# hi == 388
# # ^ Here we see that every clique obtained from the function is a sub-clique of the biggest clique, a[[3]] ; a[[max.ind]]


# our complete graph has 388 vertices, so there are
(y = 269*(269-1)/2) # edges, or, rows in the neighbor.node.net dataframe that comprise the complete graph
(x = nrow(neighbor.node.net2) - y) # how many edges should be in left over graph (dataframe)
# lets get rid of these edges
length(which((neighbor.node.net2[,1] %in% vertices.269) & (neighbor.node.net2[,2] %in% vertices.269))) == y
temp <- neighbor.node.net2[- which((neighbor.node.net2[,1] %in% vertices.269) & (neighbor.node.net2[,2] %in% vertices.269)), ]
# ^ getting rid of the edges interferes with what other cliques might be, so we need to get rid of these 451 vertices' first neighbors too

temp <- neighbor.node.net2[unique(c(which(neighbor.node.net2[,1] %in% vertices.269), which(neighbor.node.net2[,2] %in% vertices.269))),]
# ^ This includes the CSG and its neighbors, the csg has perimeter (say) with length = 451 nodes.
length(unique(c(temp[,1], temp[,2]))) # there are 7198-388 nodes outside of it that they neighbor. 
temp1 <- unique(c(temp[,1], temp[,2]))
# We want an edgelist with all of these nodes:
neighbor.node.net3 <- neighbor.node.net2[-which(neighbor.node.net2[,1] %in% vertices.269), ]
neighbor.node.net3 <- neighbor.node.net3[-which(neighbor.node.net3[,2] %in% vertices.269), ]

length(unique(c(neighbor.node.net2[,1], neighbor.node.net2[,2]))) #recall, originally, there were an original number of nodes:
length(unique(c(neighbor.node.net3[,1], neighbor.node.net3[,2]))) #and there are now this many nodes:
length(unique(c(neighbor.node.net2[,1], neighbor.node.net2[,2]))) - length(unique(c(neighbor.node.net3[,1], neighbor.node.net3[,2]))) 





