

source('f_weibo_getweibo_content.r')

# 登录
ch0 <- f_weibo_login('myemail', 'mypwd')
# ch1 <- f_weibo_login('myemail2', 'mypwd2')

# 获取微博数据（这里只做了我自己的版本，10000是个足够大的数字）
weibo_10000_0 <- f_weibo_get(cH=ch0, N=10000, hisnick='chenyibo')
# weibo_10000_1 <- f_weibo_get(cH=ch1, N=10000, hisnick='chenyibo')
# 这两个结果有一点点点差异，目前看来，貌似是显示给自己的微博比较全。
# all(weibo_10000_0 %in% weibo_10000_1)
# all(weibo_10000_1 %in% weibo_10000_0)

save(weibo_10000_0, file='weibo_saved.RData')

load('weibo_saved.RData')

# 分词
require(rmmseg4j)
weibo_data <- sapply(strsplit(weibo_10000_0, '//'), '[', 1)
weibo_data <- weibo_data[weibo_data != '' & !is.na(weibo_data)]
f_cut <- function(x){
  unlist(strsplit(mmseg4j(x), ' '))
}
words <- unlist(lapply(weibo_data, f_cut))
words <- words[words != '转发']

# 统计词频
hisnick <- 'chenyibo'
words_freq <- sort(table(words), dec=T)
words_names <- names(words_freq)
words_length <- nchar(words_names)
words_df <- data.frame(words_names=words_names, words_freq=words_freq, words_length=words_length)
# 只做两三个字的词，简单一点。。。
# words_df <- words_df[words_df$words_length %in% c(2,3), ]

# 加载搜狗实验室的词频文件
SogouLabDic <- readLines('SogouLabDic.dic')
SogouLabDic <- paste(iconv(SogouLabDic, 'GBK', 'UTF-8'), ' ')
SogouLabDic <- data.frame(do.call(rbind, strsplit(SogouLabDic, '\t')), stringsAsFactors=F)
names(SogouLabDic)[1] <- 'words_names'
SogouLabDic_match <- SogouLabDic[SogouLabDic[,1] %in% words_df$words_names, ]
SogouLabDic_match$X2 <- as.numeric(SogouLabDic_match$X2)

words_df2 <- merge (words_df, SogouLabDic_match, by='words_names', all.x=T)
# 可以筛选名词和动词。不过似乎没有必要，因为形容词副词什么的也能够体现用词风格嘛
# words_df2 <- words_df2[grep('^[NV],$',words_df2$X3), ]

# 匹配不到的扔掉
words_df3 <- words_df2[!is.na(words_df2$X2), ]
words_df3$words_freq2 <- words_df3$words_freq * log(max(words_df3$X2)/words_df3$X2)

# 做词云（这个包貌似对中文支持不是很好）
require(wordcloud)
png(paste('weibo_wordcloud_', Sys.Date(), '_', hisnick, '.png', sep=''),width=500,height=500)
par(mar=c(0,0,0,0))
wordcloud(words_df3$words_names, words_df3$words_freq2, min.freq=0, scale=c(6,1), 
          max.words=100, random.order=F, colors=rainbow(100,v=0.8,end=0.5), rot.per=0)
dev.off()

