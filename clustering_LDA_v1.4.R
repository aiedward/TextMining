rm(list=ls())

start.time <- Sys.time()
source("./ML_functions.R")

#package check & install & load
libraryList <- c("dplyr","stringi","tm","caret","reshape","lsa","RODBC","RODBCext","topicmodels","servr","LDAvis")

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
require(SnowballC)
require(topicmodels)
require(RODBC)
require(RODBCext)
require(servr)
require(LDAvis)

############################################
## Read in TM results
############################################

#get parameters
arg <- commandArgs()
name <- arg[6] #analysis name
sparse <- arg[7]
sparseRe <- gsub("0.","_",sparse)
k<- arg[8] #number of topic

visual <- "NO_Visual"
if(is.na(arg[9]))
{
  print("NO Visualization")
}else
{
  visual <- arg[9]
} 

print(visual)
print(name)
print(paste("Sparse Term Ratio :",sparse))
print(paste("Number of Cluster:",k))
print(Sys.time())

#DB Connection
print("Connect DB")
conn <- odbcConnect('smartSMA_Development_New',uid='trendtracker',pwd='#tt1234')
print("Loading Data from DB")

#stringQuery <- paste('select crawl_data_id, keyword, ranking as count, role from trendtracker.t_tp_result_rank  where role <> "blank" and user="', name,'";',sep="")

#stringQuery <- paste('select a.crawl_data_id, a.keyword, a.ranking as count, a.role FROM trendtracker.t_tp_result_rank_hyun a, trendtracker.t_svm_result_adhoc b where a.crawl_data_id = b.crawl_data_id and b.pred="nonspam" and b.user="', name,'";',sep="")

stringQuery <- paste('select a.crawl_data_id, a.keyword, a.ranking as count, a.role FROM trendtracker.t_tp_result_rank_hyun a, trendtracker.t_svm_result_adhoc b where a.crawl_data_id = b.crawl_data_id and b.pred="nonspam" and (b.user="newSixPocket4" OR b.user="newSP_old");',sep="")

tm<-sqlQuery(conn,stringQuery)

#tm<-sqlQuery(conn, 'select crawl_data_id, keyword, ranking as count, role from trendtracker.t_tp_result_rank  where user="home_blog" and role <> "blank";')
odbcClose(conn)

tm<-subset(tm,subset=(tm$keyword!="메르스" & tm$keyword!="이동하다" & tm$keyword!="언론사" & tm$keyword!="뉴스" & tm$keyword!="재배포" & tm$keyword!="금지" & tm$keyword!="관련" & tm$keyword!="아니다" & tm$keyword!="지도앱"& tm$keyword!="검색"& tm$keyword!="po"& tm$keyword!="point"))

## TM results into document keywords matrices
print("Make DTM")

tmKeyword <- fn_tm_keys(tm)
print(paste("Total Document :",nrow(tmKeyword)))

##Manual Spam Check
spamDocId <- read.table(file="spamDocId.txt", header=TRUE)
spamCheck <- tmKeyword$crawl_data_id %in% spamDocId$spamDocId
tmKeyword <- tmKeyword[!spamCheck,]

##Duplication Check
dupCheck <- duplicated(tmKeyword[,2])
tmKeyword <- tmKeyword[!dupCheck,]

print(paste("Target Document :",nrow(tmKeyword)))
corp<-Corpus(DataframeSource(tmKeyword))
dtm<-DocumentTermMatrix(corp, control=list(removeNumbers=TRUE, wordLengths=c(2,Inf)))
dtm <- removeSparseTerms(dtm, as.numeric(sparse))

term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))

new_dtm <-dtm[,term_tfidf >= 0.1]
new_dtm <-new_dtm[row_sums(new_dtm)>0,]

############################################
## Make Topic
############################################
print("Start LDA")
SEED <-2010

lda_tm <- LDA(new_dtm, control=list(seed=SEED), k=as.numeric(k))

doc_topic <- topics(lda_tm,1)
term_topic <- terms(lda_tm,100)


write.table(term_topic, paste("./",name,sparseRe,"_",k,"_LDA_Result.csv",sep=""),sep=",", row.names=FALSE)


#Doc_topic result making
doc_topic_df <- as.data.frame(doc_topic)
doc_topic_df$rown <- as.numeric(row.names(doc_topic_df))
tmKeyword$rown <- as.numeric(row.names(tmKeyword))

#doc prob make
docProb <- posterior(lda_tm)$topics %>% as.matrix
docProb_df <- as.data.frame(docProb)
docProb_df$rown <- as.numeric(row.names(docProb_df))
max<-NULL
for(i in 1:nrow(docProb_df)){
	max<-rbind(max,max(docProb[i,]))
}
docProb_df$maxProb<-max

id_topic <- merge(doc_topic_df, docProb_df, by="rown")
id_topic <- merge(id_topic, tmKeyword, by="rown")
id_topic <- subset(id_topic,select=c("rown","doc_topic","crawl_data_id","maxProb"))

write.table(id_topic, paste("./",name,sparseRe,"_",k,"_raw","_LDA_Result.csv",sep=""),sep=",", row.names=FALSE)

outPutForQV <- fn_LDA_Result_for_QV(term_topic)
write.table(outPutForQV, paste("./",name,sparseRe,"_",k,"_QV","_LDA_Result.csv",sep=""),sep=",", row.names=FALSE)


#Make visualization
if(visual == "TRUE"){
	phi <- posterior(lda_tm)$terms %>% as.matrix
	theta <- posterior(lda_tm)$topics %>% as.matrix
	vocab <- colnames(phi)

	doc_length <- vector()
	doc_topic_df<-as.data.frame(doc_topic)

	for( i in as.numeric(row.names(doc_topic_df))){
	  temp <- corp[[i]]$content
	  doc_length <- c(doc_length, nchar(temp[2]))
	}

	temp_frequency <- inspect(new_dtm)

	freq_matrix <- data.frame(ST = colnames(temp_frequency),
	                          Freq = colSums(temp_frequency))

	json_lda <- createJSON(phi = phi, theta = theta,
        	               vocab = vocab,
                	       doc.length = doc_length,
	                       term.frequency = freq_matrix$Freq)

	serVis(json_lda, out.dir = paste("./LDAvis_Result/",name,sparseRe,"_",k,sep=""), open.browser = FALSE)
}
end.time <- Sys.time()
svmtraintime <- end.time - start.time

print("Analysis Time:")
print(svmtraintime)
print("LDA Complete")

rm(tm)
rm(start.time)
rm(end.time)

save.image(file=paste(name,sparseRe,"_",k,"_clustering_LDA_Result.RData",sep=""))
