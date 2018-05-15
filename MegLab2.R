rm(list = ls())
library(igraph)
library(dplyr)
library(stringi)
library(reshape2)

setwd("C:/Users/Meg Ashby/Desktop/CMS350C")
options(stringsAsFactors = F)

padgb <- read.csv(file = 'padgb.csv', header = TRUE)
padgm <- read.csv(file = 'padgm.csv', header = TRUE)
padatt <- read.csv(file = 'padatt.csv', header = TRUE)

businessMat1 <-filter(padgb) %>% select(-matches('Node'))
marriageMat1 <-filter(padgm) %>% select(-matches('Node'))
families <-filter(padgb) %>% select(matches('Node'))

businessMat <- as.matrix(businessMat1)
marriageMat <- as.matrix(marriageMat1)

businessgraph <-graph.adjacency(businessMat, weighted = NULL, mode = 'undirected')
marriagegraph <-graph.adjacency(marriageMat, weighted = NULL, mode = 'undirected')

datamix <-match(V(businessgraph)$name, padatt$Node)
V(businessgraph)$wealth <- padatt$WEALTH[datamix]
V(businessgraph)$priors <- padatt$X.PRIORS[datamix]
V(businessgraph)$ties <- padatt$X.TIES[datamix]

datamix2 <-match(V(businessgraph)$name, padatt$Node)
V(marriagegraph)$wealth <- padatt$WEALTH[datamix]
V(marriagegraph)$priors <- padatt$X.PRIORS[datamix]
V(marriagegraph)$ties <- padatt$X.TIES[datamix]

businessdegree <- degree(businessgraph, mode = c("all"))
marriagedegree <- degree(marriagegraph, mode = c("all"))

businessdepend <-power_centrality(businessgraph, loops = FALSE, exponent = -.05, rescale = FALSE, spars = FALSE)
marriagedepend <-power_centrality(marriagegraph, loops = FALSE, exponent = -.05, rescale = FALSE, spars = FALSE)

businesspower <- power_centrality(businessgraph, loops = FALSE, exponent = 0.5,
            rescale = FALSE, sparse=FALSE)
marriagepower <- power_centrality(marriagegraph, loops = FALSE, exponent = 0.5,
            rescale = FALSE, sparse=FALSE)

closenessbusiness <- closeness(businessgraph)
closenessmarriage <- closeness(marriagegraph)

eigenbusiness <-eigen_centrality(businessgraph)$vector
eigenmarriage <-eigen_centrality(marriagegraph)$vector

betweenbusiness <-betweenness(businessgraph, directed = FALSE)
betweenmarriage<-betweenness(marriagegraph, directed = FALSE)

library(sna)

flowbetweenbusiness <- flowbet(businessMat1)
flowbetweenmarriage <- flowbet(marriageMat1)
detach("package:sna", unload=TRUE)

reach2=function(x){
  r=vector(length=vcount(x))
  for (i in 1:vcount(x)){
    n=neighborhood(x,2,nodes=i)
    ni=unlist(n)
    l=length(ni)
    r[i]=(l)/vcount(x)}
  r}
businessreach2 <- reach2(businessgraph)
marriagereach2 <- reach2(marriagegraph)

businessdata<-data.frame(V(businessgraph)$name)
businessdata$priors <-V(businessgraph)$priors
businessdata$depend <- businessdepend
businessdata$power <-businesspower
businessdata$closeness <- closenessbusiness
businessdata$eigenvectorcentrality <- eigenbusiness
businessdata$between <- betweenbusiness
businessdata$degree <-businessdegree
businessdata$reach2 <- businessreach2
businessdata$flow <- flowbetweenbusiness

marriagedata<-data.frame(V(marriagegraph)$name)
marriagedata$priors <-V(marriagegraph)$priors
marriagedata$depend <- marriagedepend
marriagedata$power <-marriagepower
marriagedata$closeness <- closenessmarriage
marriagedata$eigenvectorcentrality <- eigenmarriage
marriagedata$between <- betweenmarriage
marriagedata$degree <-marriagedegree
marriagedata$reach2 <- marriagereach2
marriagedata$flow <-flowbetweenmarriage

library(ggplot2)

p <- ggplot(marriagedata, aes(priors, eigenvectorcentrality, color = degree)) 
p + geom_point(shape = 16, size = 5)
p + geom_text(label = V(marriagegraph)$name)


p <- ggplot(marriagedata, aes(priors, abs(power), color = degree)) 
p + geom_point(shape = 16, size = 5)
p + geom_text(label = V(marriagegraph)$name)


V(businessgraph)$size <- V(businessgraph)$priors
plot(businessgraph)

V(marriagegraph)$size <- V(marriagegraph)$priors/2
plot(marriagegraph)

businessdata
marriagedata



