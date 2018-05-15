rm(list = ls())
library(igraph)
library(dplyr)
library(stringi)
library(reshape2)
setwd("C:/Users/Meg Ashby/Desktop/CMS350C")
options(stringsAsFactors = F)

dat<-read.csv(file = "Freeman's_EIES_2.csv", header = TRUE)

#head(dat)

mat1 <- filter(dat) %>% select(-matches('Node'))
mat <- as.matrix(mat1)
graphobj <- graph.adjacency(mat, weighted = TRUE, mode = 'undirected')


att<-read.csv(file = "Freeman's_EIES_Attribute.csv")
head(att)
datamix <- match(V(graphobj)$name, att$Node)

V(graphobj)$citations = att$Citations
V(graphobj)$discipline = att$Discipline

plot(graphobj, vertex.label = V(graphobj)$name, 
layout=layout.fruchterman.reingold, 
vertex.color = V(graphobj)$discipline,
edge.width = E(graphobj)$weight)

clustering <- transitivity(graphobj, type = c("local"))

clusteringmat <- as.matrix(clustering)
V(graphobj)$clust <-clusteringmat

plot(graphobj, vertex.label = V(graphobj)$name, 
layout=layout.fruchterman.reingold, 
vertex.color = V(graphobj)$discipline,
vertex.size = V(graphobj)$clust*10,
edge.width = E(graphobj)$weight)

graphobj2 <-graphobj

E(graphobj2)[E(graphobj2)$weight == 1]$weight <- 0
E(graphobj2)[E(graphobj2)$weight == 2]$weight <- 0
#E(graphobj2)[E(graphobj2)$weight == 3]$weight <- 0

graphobj2 <- delete_edges(graphobj2, which(E(graphobj2)$weight==0))

plot(graphobj2, vertex.label = V(graphobj2)$name, 
layout=layout.fruchterman.reingold(graphobj2, niter=5000),
displaylabels = F,
vertex.color = V(graphobj2)$discipline,
vertex.size = V(graphobj2)$citations/2,
edge.width = E(graphobj2)$weight)

l2 <- layout.fruchterman.reingold(graphobj3, niter=5000)

plot(graphobj2, vertex.label = V(graphobj2)$name, 
layout=l2,
vertex.label.cex=0.75, 
vertex.label.font=2,
vertex.color = V(graphobj2)$discipline,
vertex.size = V(graphobj3)$citations/2,
edge.width = E(graphobj3)$ebtwn /10,
rescale = FALSE, 
ylim=c(15,22),
xlim=c(17,28),
 asp = 0)

l <- layout.fruchterman.reingold(graphobj2, niter=5000)

plot(graphobj2, layout=l, 
	vertex.color = V(graphobj2)$discipline,
     vertex.label.cex=0.75, 
     vertex.label.font=2,
     vertex.shape="circle", 
     vertex.size=V(graphobj2)$clust*10, 
     vertex.label.color="black", 
     edge.width=E(graphobj2)$weight)


edges <-as.data.frame(get.edgelist(graphobj2))
ebtwn<-edge_betweenness(graphobj2, directed = TRUE)
edges$etbwn <-ebtwn
E(graphobj2)$ebtwn <-ebtwn

plot(graphobj2, layout=l, 
	vertex.color = V(graphobj2)$discipline,
     vertex.label.cex=0.75, 
     vertex.label.font=2,
     vertex.shape="circle", 
     vertex.size=V(graphobj2)$clust*10, 
     vertex.label.color="black", 
     edge.width=E(graphobj2)$ebtwn /2)

graphobj3 <-graphobj

E(graphobj3)[E(graphobj3)$weight == 1]$weight <- 0
E(graphobj3)[E(graphobj3)$weight == 2]$weight <- 0
E(graphobj3)[E(graphobj3)$weight == 3]$weight <- 0

graphobj3 <- delete_edges(graphobj3, which(E(graphobj3)$weight==0))

plot(graphobj3, vertex.label = V(graphobj3)$name, 
layout=layout.fruchterman.reingold(graphobj3, niter=5000),
vertex.label.cex=0.75, 
vertex.label.font=2,
vertex.color = V(graphobj3)$discipline,
vertex.size = V(graphobj3)$citations/2,
edge.width = E(graphobj3)$ebtwn /10,)

l <- layout.fruchterman.reingold(graphobj3, niter=5000)

edges <-as.data.frame(get.edgelist(graphobj3))
ebtwn<-edge_betweenness(graphobj3, directed = TRUE)
edges$etbwn <-ebtwn
E(graphobj3)$ebtwn <-ebtwn

plot(graphobj3, layout=l, 
	vertex.color = V(graphobj3)$discipline,
     vertex.label.cex=0.75, 
     vertex.label.font=2,
     vertex.shape="circle", 
     vertex.size=10, 
     vertex.label.color="black", 
     edge.width=E(graphobj3)$ebtwn /10)

plot(graphobj2, layout=l, 
	vertex.color = V(graphobj2)$discipline,
     vertex.label.cex=0.75, 
     vertex.label.font=2,
     vertex.shape="circle", 
     vertex.size=10, 
     vertex.label.color="black", 
     edge.width=E(graphobj2)$ebtwn /10)

mean(E(graphobj2)$ebtwn)
sd(E(graphobj2)$ebtwn)
max(E(graphobj2)$ebtwn)

mean(E(graphobj3)$ebtwn)
sd(E(graphobj3)$ebtwn)
max(E(graphobj3)$ebtwn)


