
source("/home/ruser/TextPrism/RSource/ML_functions.R")
source("/home/ruser/TextPrism/RSource/createNamJson.R")

require(dplyr)
require(stringi)
require(tm)
require(reshape)
require(slam)
require(SnowballC)
require(topicmodels)
require(RODBC)
require(RODBCext)
require(servr)
require(LDAvis)
require(igraph)
require(network)
require(sna)
require(ggplot2)
require(GGally)

makeCorpus <- function(inputQuery, MSC){
  
  start.time <- Sys.time()
  
  ############################################
  ## Read in TM results
  ############################################
  #DB Connection
  print("Connect DB")
  conn <- odbcConnect('ccaMart',uid='trendtracker',pwd='#tt1234')
  #conn2 <- odbcConnect('smaali',uid='namyun',pwd='dighdi12')
  #conn <- odbcConnect('Alibaba',uid='trendtracker',pwd='#tt1234')
  print("Loading Data from DB")
  #select crawl_data_id, keyword, role, ranking from t_tp_result_rank_adhoc where user = "navien";
  
  tm<-sqlQuery(conn,inputQuery)
  odbcClose(conn)
  
  tmKeyword <- fn_tm_keys(tm)
  print(paste("Total Document :",nrow(tmKeyword)))
  
  ####################################
  ## Manual Spam Check
  ####################################
  if(MSC){
    spamDocId <- read.table(file="spamDocId.txt", header=TRUE)
    spamCheck <- tmKeyword$crawl_data_id %in% spamDocId$spamDocId
    tmKeyword <- tmKeyword[!spamCheck,]
  }
  
  ##Duplication Check
  dupCheck <- duplicated(tmKeyword[,2])
  tmKeyword <- tmKeyword[!dupCheck,]
  
  print(paste("Target Document :",nrow(tmKeyword)))
  
  end.time <- Sys.time()
  runtime <- end.time - start.time
  print("DB Loading Time: ")
  print(runtime)
  return(tmKeyword)
}

ldaAnalysis <- function(tmKeyword, sparse, name, k, numTermByTopic, visual){
  
  start.time <- Sys.time()
  
  sparseRe <- gsub("0.","_",sparse)
  
  corp<-Corpus(DataframeSource(tmKeyword))
  dtm<-DocumentTermMatrix(corp, control=list(removeNumbers=TRUE, wordLengths=c(2,Inf)))
  dtm <- removeSparseTerms(dtm, as.numeric(sparse))
  
  ##Remove low tf-idf col and row
  term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
  new_dtm <-dtm[,term_tfidf >= 0.1]
  new_dtm <-new_dtm[row_sums(new_dtm)>0,]
  
  ############################################
  ## Running LDA
  ############################################
  print("Start LDA")
  SEED <-2010
  
  lda_tm <- LDA(new_dtm, control=list(seed=SEED), k=as.numeric(k))
  
  doc_topic <- topics(lda_tm,1)
  
  ### Call makeVisualization ###
  makeVisualization(visual, lda_tm, doc_topic, corp, new_dtm, name, sparseRe, k)
  
  end.time <- Sys.time()
  runtime <- end.time - start.time
  print("Processing Time: ")
  
  return(lda_tm)
}

makeLdaOutput <- function(lda_tm, sparse, name, k, numTermByTopic, visual){
  
  start.time <- Sys.time()
  
  sparseRe <- gsub("0.","_",sparse)
  
  doc_topic <- topics(lda_tm,1)
  term_topic <- terms(lda_tm, numTermByTopic)
  
  write.table(term_topic, paste("./output/",name,sparseRe,"_",k,"_LDA_Result.csv",sep=""),sep=",", row.names=FALSE)
  
  end.time <- Sys.time()
  runtime <- end.time - start.time
  print("Visualization Time: ")
  print(runtime)
  
  return(term_topic)
}


makeVisualization <- function(visual, lda_tm, doc_topic, corp, new_dtm, name, sparseRe, k){
  #########################################
  ## Make visualization
  #########################################
  if(visual){
    # phi is probabilities of the topics for each of the terms
    phi <- posterior(lda_tm)$terms %>% as.matrix
    
    # theta is probabilities of the topics for each the document
    theta <- posterior(lda_tm)$topics %>% as.matrix
    #phi2 <- phi[ ,order(as.integer(colnames(phi)))]
    vocab <- colnames(phi)
    
    doc_length <- vector()
    doc_topic_df<-as.data.frame(doc_topic)
    
    #get document length
    for( i in as.numeric(row.names(doc_topic_df))){
      temp <- corp[[i]]$content
      doc_length <- c(doc_length, nchar(temp[2]))
    }
    
    temp_frequency <- as.matrix(new_dtm)
    
    freq_matrix <- data.frame(ST = colnames(temp_frequency),
                              Freq = colSums(temp_frequency))
    
    json_lda <- createNamJson(phi = phi, theta = theta,
                              vocab = vocab,
                              doc.length = doc_length,
                              term.frequency = freq_matrix$Freq)
    
    #serVis(json_lda, out.dir = paste("/home/ruser/TextPrism/LDAvis_Result/",name,sparseRe,"_",k,sep=""), open.browser = FALSE)
    ##release to TOMCAT
    serVis(json_lda, out.dir = paste("/data001/tomcat/webapps/",name,sparseRe,"_",k,sep=""), open.browser = FALSE)
  }
}

makeNetwork <- function(networkMatrix){
  g <- graph.adjacency(networkMatrix, weighted=T, mode = "undirected")
  g <- simplify(g)
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  
  set.seed(3952)
  layout1 <- layout.kamada.kawai(g)
  layout2 <- layout.graphopt(g)
  plot.igraph(g, layout=layout1,edge.width=2, main="TopicKeword Network", vertex.size=10*sqrt(hub.score(g)$vector))
  
  #write.graph(g,file ="test.ncol",format="ncol")
}


#####################################################################################
shinyServer(function(input, output, session) {
  
  getCorpus <- reactive({
    if(input$inputSql != 'Insert SQL!!'){
      isolate({
        withProgress({
          setProgress(message = "Loading corpus...")
          corpusResult <- makeCorpus(input$inputSql, input$MSC)
        })
      })
    }else{
      corpusResult <- NULL
    }
    
    return(corpusResult)
  })
  
  getLdaResult <- reactive({
    corpusResult <- getCorpus()
    if(!is.null(corpusResult)){
      isolate({
        withProgress({
          setProgress(message = "Processing corpus...")
          lda_tm <- ldaAnalysis(corpusResult, input$sparse, input$projectName, input$clusters, input$terms, input$visual)
          
        })
      })
    }else{
      lda_tm <- NULL
    }
    
  })
  
  getLdaOutput <- reactive({
    lda_tm <- getLdaResult()
    if(!is.null(lda_tm)){
      isolate({
        withProgress({
          setProgress(message = "Processing corpus...")
          term_topic <- makeLdaOutput(lda_tm, input$sparse, input$projectName, input$clusters, input$terms, input$visual)
        })
      })
    }
  })

  
  getTdm <- reactive({
    ##topicTDM
    lda_tm <- getLdaResult()
    if(!is.null(lda_tm)){
      topicTDM <- posterior(lda_tm)$term
      
      smallTopicTDM <- topicTDM[,colSums(topicTDM)>0.01]
      
      smallTopicMatrix <- as.matrix(smallTopicTDM)
      smallTopicMatrix[smallTopicMatrix>=0.01] <- 1
      smallTopicMatrix[smallTopicMatrix<0.01] <- 0
      
      smallTopicMatrix <-  t(smallTopicMatrix) %*% smallTopicMatrix
      #smallTopicMatrix2 <-  smallTopicMatrix %*% t(smallTopicMatrix)
      
      ## Make Network
      net <- network(smallTopicMatrix, directed=FALSE)
      #net %v% "mode" <- ifelse(betweenness(net)>10, "big", "small")
      col = c("small" = "grey", "big" = "gold")
      #ggnet2(net, mode = "kamadakawai", label=TRUE, color = "mode", palette = col)
      ggnet2(net, mode = "kamadakawai", label=TRUE)
      
    }

  })
  
  #################
  #### UI Part ####
  #################
  output$console <- renderPrint({
    count <- getLdaResult()
  })
  
  output$visual <- renderPrint({
    if(input$inputSql != 'Insert SQL!!'){
      if(input$visual){
        sparseRe <- gsub("0.","_",input$sparse)
        ldaResultURL <- cat(paste("http://165.243.188.249:8080/",input$projectName,sparseRe,"_",input$clusters,sep=""))
      }
      else{
        ldaResultURL <- cat("Visualization wasn't selected.")
      }
      
    }
    else{
      ldaResultURL <- cat("Display visualization URL after processing.")
    }

  })
  
  output$lda <- renderTable({
    getLdaOutput()
  })
  
  output$network <- renderPlot({
    getTdm()
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      #paste('data-', Sys.Date(), '.csv', sep='')
      sparseRe <- gsub("0.","_",input$sparse)
      paste(input$projectName,sparseRe,"_",input$clusters,"_LDA_Result.csv",sep="")
      },
    
    content = function(file) {
      write.csv(getLdaOutput(), file)
      }
    )
})
