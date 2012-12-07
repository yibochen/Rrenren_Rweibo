


source('f_weibo_getweibo_search.r')

# 登录
ch0 <- f_weibo_login('myemail', 'mypwd')

# 获取微博搜索数据
weibo_0 <- f_weibo_get(cH=ch0, mykeyword='华院数云')

save(weibo_0, file='weibo_saved.RData')


sort(table(weibo_0$nickname), dec=T)[1:20]

require(rmmseg4j)
weibo_data <- weibo_0$content
weibo_data <- gsub('[0-9a-zA-Z]', ' ', weibo_data)
docs <- mmseg4j(weibo_data, reload=T)

require(tm)
cps <- Corpus(VectorSource(docs))
cps <- tm_map(cps, stripWhitespace)
dtm <- DocumentTermMatrix(cps, 
control=list(wordLengths=c(2,Inf)))
inspect(dtm[100:110,100:110])
findFreqTerms(dtm, 10)
findAssocs(dtm, '百分点', 0.8)
inspect(removeSparseTerms(dtm, 0.9)[1:20,])
myDic <- Dictionary(c('科技','数据','分析','软件','华院','数云','百分点','智能','系统'))
dtm9 <- DocumentTermMatrix(cps, 
control=list(wordLengths=c(2,Inf), dictionary=myDic))

require(topicmodels)
dim(dtm)
summary(col_sums(dtm))
term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
summary(term_tfidf)
dtm1 <- dtm[, term_tfidf >= 0.2]
dtm2 <- dtm1[row_sums(dtm1) > 0, ]
summary(col_sums(dtm))
tp <- LDA(dtm2, k=3, control=list(seed=2012))
topic <- topics(tp, 1)
term <- terms(tp, 10)




