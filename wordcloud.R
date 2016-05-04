library(dplyr)
library(wordcloud)

tm <- read.csv("/home/ruser/TextPrism/RSource/out_resume.csv", encoding = "UTF-8")
head(tm)
#Pre Processing
tm$keyword <- replace(tm$keyword, (tm$keyword %in% "역활"), "마케팅")
tm$keyword <- replace(tm$keyword, (tm$keyword %in% "lg"), "LG전자")
tm$keyword <- replace(tm$keyword, (tm$keyword %in% "엘지"), "LG전자")
tm$keyword <- replace(tm$keyword, (tm$keyword %in% "전자"), "LG전자")
tm$keyword <- replace(tm$keyword, (tm$keyword %in% "다양"), "마케팅")
tm$keyword <- replace(tm$keyword, (tm$keyword %in% "존슨"), "머신러닝")
tm$keyword <- replace(tm$keyword, (tm$keyword %in% "2010"), "머신러닝")
tm$keyword <- replace(tm$keyword, (tm$keyword %in% "도출"), "머신러닝")





tm2 <- tm %>% group_by(keyword) %>% summarise(freq=sum(count))
tm3 <- tm2[order(-tm2$freq),]
head(tm3,30)
stopword <- c("2013","12","sm","01","03","04","05","07","02","2009","20","09","16","10","21","11","06","26","08")
tm4<- tm3[!(tm3$keyword %in% stopword),]
head(tm4)


pal<-brewer.pal(9,"Set2")
wordcloud(tm4$keyword, tm4$freq, min.freq =2, colors = pal, rot.per = 0.25, random.order = F, scale = c(5,1))
