rm(list=ls())

start.time <- Sys.time()
source("/home/ruser/TextPrism/ML_functions.R")

#package check & install & load
libraryList <- c("dplyr","stringi","tm","caret","reshape","RODBC","RODBCext")

for(lib in libraryList){
  package.checking <- find.package(lib,quiet=TRUE)
  if(length(package.checking) == 0){
    install.packages(lib)
  }
}
require(dplyr)
require(stringi)
require(tm)
require(caret)
require(reshape)
require(RODBC)
require(RODBCext)
#require(e1071)

############################################
## Read in TM results
############################################
conn <- odbcConnect('smartSMA_Development_New',uid='trendtracker',pwd='#tt1234')

tm<-sqlQuery(conn,'SELECT crawl_data_id, crawled_date, keyword, ranking as count, role, spam_yn FROM trendtracker.t_tp_result_rank_train where role <> "$$";')

odbcClose(conn)

## TM results into document keywords matrices
tmKeyword <- fn_tm_keys(tm)
#tmRoles <- fn_tm_roles(tm)
tmDocType <- fn_tm_spam_yn(tm)

############################################
## Make DTM
############################################
dtm_df <- fn_makeDTM(tmKeyword,0.99)

dtm_df$crawl_data_id <- tmKeyword$crawl_data_id

## Append document type information
total <- merge(dtm_df, tmDocType, by="crawl_data_id")
#total <- merge(total, tmRoles, by="crawl_data_id")
noDocidTotal <- select(total, -(crawl_data_id))

############################################
## Train models
############################################
print("Creating Model")
cvtrain <- trainControl(method="cv", number=5)
svmModel <- train(spam_yn ~ ., data=noDocidTotal, method="svmLinear",
                   trControl=trainControl(method="cv", number=3),
                   tuneGrid=data.frame(.C=c(0.01)),
                   metric="Accuracy",
                   preProc=c("center", "scale"))             


end.time <- Sys.time()
svmtraintime <- end.time - start.time

print("Modeling Time:")
print(svmtraintime)
print("Modeling Complete")

rm(tm)
#rm(tmKeyword)
#rm(tmRoles)
rm(tmDocType)
rm(dtm_df)
rm(total)
rm(start.time)
rm(end.time)

save.image(file="model_svm_db_final_099.RData")
