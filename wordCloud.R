getwd()
setwd("/Users/kimnamyoun/GitHub/TextMining/")
localeToCharset()

tm <- read.csv("/Users/kimnamyoun/GitHub/out_resume.csv", encoding = "EUC-KR")
head(tm)
source("./ML_functions.R")
tmKeyword <- fn_tm_keys(tm)
corp<-Corpus(DataframeSource(tmKeyword))
dtm<-DocumentTermMatrix(corp, control=list(removeNumbers=TRUE, wordLengths=c(2,Inf)))
dtm <- removeSparseTerms(dtm, as.numeric(0.999))


require(wordcloud)
library(RColorBrewer)
library(extrafont)
font_import()
loadfonts(device="postscript")
display.brewer.all() 
pal <- brewer.pal(9, "BuGn")
wordcloud(tm$keyword,tm$count,colors=pal,family="AppleGothic")



