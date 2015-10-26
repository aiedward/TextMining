rm(list=ls())

#package check & install & load
libraryList <- c("dplyr","stringi","tm","reshape","slam","igraph")

for(lib in libraryList){
  package.checking <- find.package(lib,quiet=TRUE)
  if(length(package.checking) == 0){
    install.packages(lib)
  }
}
require(dplyr)
require(stringi)
require(tm)
require(reshape)
require(slam)
require(igraph)
require(network)

load(file="D:/Rwork/newSP_ALL_997_50_clustering_LDA_Result.RData")

##Build a Graph
makeNetwork <- function(networkMatrix){
  g <- graph.adjacency(networkMatrix, weighted=T, mode = "undirected")
  g <- simplify(g)
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  
  set.seed(3952)
  layout1 <- layout.kamada.kawai(g)
  layout2 <- layout.graphopt(g)
  plot.igraph(g, layout=layout2, edge.width=E(g)$weight, main="TopicKeword Network", vertex.size=10*sqrt(hub.score(g)$vector))
  
  write.graph(g,file="D:/Rwork/newSixPocket4.ncol",format="ncol")
  
}

############################################
## Read in TM results
############################################


## TM results into document keywords matrices
corp<-Corpus(DataframeSource(tmKeyword))
tdm<-TermDocumentMatrix(corp, control=list(removeNumbers=TRUE, wordLengths=c(2,Inf)))
tdm<-removeSparseTerms(tdm,0.8)
termDocMatrix <- as.matrix(tdm)
termDocMatrix[termDocMatrix>=1] <- 1
termMatrix <- termDocMatrix %*% t(termDocMatrix)

term_topic <- terms(lda_tm,100)

##Topic term extraction
temp <- NULL
output <- NULL

for(i in 1:ncol(term_topic)){
  for(j in 1:nrow(term_topic)){
    temp$topicNo<-i
    temp$keyword <- term_topic[j,i]
    output <- rbind(output,temp)
  }
}
outputDf <- as.data.frame(output)
impTerm <- unlist(outputDf$keyword)
impTerm <- as.data.frame(impTerm)
impTerm <- unique(impTerm)
impTermDf <- as.data.frame(impTerm)


##Make topicTDM
topicTDM <- posterior(lda_tm)$term
topicTdmDf <- as.data.frame(topicTDM)
topicTdmDf <- subset(topicTdmDf, select=c(impTermDf[,1]))

smallTopicTDM <- topicTdmDf[,apply(topicTdmDf,2,max)>0.02]

smallTopicMatrix <- as.matrix(smallTopicTDM)
smallTopicMatrix[smallTopicMatrix>=0.0001] <- 1
smallTopicMatrix[smallTopicMatrix<0.0001] <- 0

#키워드 Network
smallTopicMatrix1 <-  t(smallTopicMatrix) %*% smallTopicMatrix

#토픽 Network
smallTopicMatrix2 <-  smallTopicMatrix %*% t(smallTopicMatrix)

makeNetwork(smallTopicMatrix1)

makeNetwork(smallTopicMatrix2)




