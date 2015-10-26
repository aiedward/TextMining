## TM_Keywords

fn_tm_keys <- function(tmResult){
 tm_keys <-
  tmResult %>% 
  group_by(crawl_data_id) %>%
  mutate(kw=stri_dup(stri_c(keyword," "),count)) %>%
  summarise(keywords=stri_c(kw,collapse=" "))
 return(tm_keys)
}

## TM_Roles

fn_tm_roles <- function(tmResult){
 tm_roles <-
  tmResult %>%
  group_by(crawl_data_id, role) %>%
  summarise(sumCount=sum(count))
 
 tm_roles <- cast(tm_roles, crawl_data_id ~ role, sum)
 tm_roles <- subset(tm_roles, select=c(crawl_data_id,A1,A2,AJ,AZ,NB,NN, NZ,url,VB))
 return(tm_roles)
}

## TM_Doc_Type
fn_tm_spam_yn <- function(tmResult){
 tm_doc_type <- tmResult %>% group_by(crawl_data_id, spam_yn) %>% summarise(sumCount=sum(count))
 tm_doc_type <- subset(tm_doc_type, select=(-sumCount))
 return(tm_doc_type)
}

## Make DTM
fn_makeDTM <- function(tm_keys,sparseTerm){
 corp <- Corpus(DataframeSource(tm_keys))
 dtm <- DocumentTermMatrix(corp, 
                           control=list(removeNumbers=TRUE, 
                                        wordLengths=c(2,Inf)))
 dtm <- removeSparseTerms(dtm, sparseTerm)
 dtmDf <- as.data.frame(as.matrix(dtm))
 return(dtmDf)
}

## Make TF-IDF DTM
fn_makeTfIdfDTM <- function(tm_keys,sparseTerm){
 corp <- Corpus(DataframeSource(tm_keys))
 dtm <- DocumentTermMatrix(corp,
                           control=list(weighting=weightTfIdf,
                                        removeNumbers=TRUE,
                                        wordLengths=c(2,Inf)))
 dtm <- removeSparseTerms(dtm, sparseTerm)
 dtmDf <- as.data.frame(as.matrix(dtm))
 return(dtmDf)
}


##LDA_Result_change_for Qlikview
fn_LDA_Result_for_QV <- function(term_topic){
 temp <-NULL
 output<-NULL

 for(i in 1:ncol(term_topic)){
   for(j in 1:nrow(term_topic)){
    temp$topicNo <- i
    temp$keyword <- term_topic[j,i]
    output <-  rbind(output,temp)
    }
  }
 
 return(output)
}

##LDA_Result_change_for Qlikview
fn_LDA_term_Result_for_QV <- function(phi){
 temp <-NULL
 output<-NULL

 for(i in 1:ncol(phi)){
   for(j in 1:nrow(phi)){
    temp$topicNo <- j
    temp$keyword <- noquote(colnames(phi)[i])
    temp$termProb <- phi[j,i]
    output <-  rbind(output,temp)
    }
  }
 return(output)
}

