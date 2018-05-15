rm(list = ls())
library(igraph)
library(dplyr)
library(stringi)
library(reshape2)
setwd("C:/Users/Meg Ashby/Desktop/CMS350C")
options(stringsAsFactors = F)

friendship <-read.csv(file = "FRIENDSHIP.csv", header = TRUE)
friendshipmat <- filter(friendship) %>% select(-matches('Node'))
friendshipmat <- as.matrix(friendshipmat)

gfriendship <-graph.adjacency(friendshipmat, weighted = NULL, mode = 'directed')

reports <-read.csv(file = "REPORTS_TO.csv", header = TRUE)
reportsmat <- filter(reports) %>% select(-matches('Node'))
reportsmat <-as.matrix(reportsmat)
greports <-graph.adjacency(reportsmat, weighted = NULL, mode = 'directed')

attrib <-read.csv(file = "High_Tec_Attributes.csv", header = TRUE)
datamix <-match(V(gfriendship)$name, attrib$Node)
datamix2 <-match(V(greports)$name, attrib$Node)

V(gfriendship)$age <-attrib$AGE[datamix]
V(gfriendship)$tenure <-attrib$TENURE[datamix]
V(gfriendship)$level <-attrib$LEVEL[datamix]
V(gfriendship)$dept <-attrib$DEPT[datamix]

V(greports)$age <-attrib$AGE[datamix]
V(greports)$tenure <-attrib$TENURE[datamix]
V(greports)$level <-attrib$LEVEL[datamix]
V(greports)$dept <-attrib$DEPT[datamix]

plot(greports, vertex.color = V(greports)$dept)

#summary(gfriendship)
plot(gfriendship, vertex.color = V(gfriendship)$dept)

dept1 <-induced_subgraph(gfriendship, V(gfriendship)$dept == 1, impl = c("copy_and_delete"))
dept2 <-induced_subgraph(gfriendship, V(gfriendship)$dept == 2, impl = c("copy_and_delete"))
dept3 <-induced_subgraph(gfriendship, V(gfriendship)$dept == 3, impl = c("copy_and_delete"))
dept4 <-induced_subgraph(gfriendship, V(gfriendship)$dept == 4, impl = c("copy_and_delete"))

trans1 <-transitivity(dept1, type = c('local'), isolates = 'zero')
trans2 <-transitivity(dept2, type = c('local'), isolates = 'zero')
trans3 <-transitivity(dept3, type = c('local'), isolates = 'zero')
trans4 <-transitivity(dept4, type = c('local'), isolates = 'zero')
mean(trans1)
mean(trans2)
mean(trans3)
mean(trans4)

kc <-coreness(gfriendship, mode = 'in')
plot(gfriendship, vertex.color = kc)

nbtwn<-betweenness(gfriendship, directed = TRUE)
V(gfriendship)$nbtwn <- nbtwn

plot(gfriendship, vertex.color = V(gfriendship)$dept, vertex.size = nbtwn/2)

degall <-degree(gfriendship, mode = c('in'))
plot(gfriendship, vertex.color = V(gfriendship)$dept, vertex.size = degall*6)

assortativity(gfriendship, V(gfriendship)$dept, directed=TRUE)
assortativity(greports, V(greports)$dept, directed = TRUE)

library(sna)
gcor(reportsmat, friendshipmat)
