

# 微博关键词及时间分布
f_weibo_app1 <- function(hisnick='chenyibo'){
  load(paste('weibo_saved_', hisnick, '.RData', sep=''))
  
  # 分词
  require(rmmseg4j)
  weibo_data <- sapply(strsplit(weibo_get$weibo_data$weibo_content, '//'), '[', 1)
  weibo_data <- weibo_data[weibo_data != '' & !is.na(weibo_data)]
  f_cut <- function(x){
    unlist(strsplit(mmseg4j(x), ' '))
  }
  words <- unlist(lapply(weibo_data, f_cut))
  words <- words[words != '转发']
  
  # 统计词频
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
  png(paste('weibo_wordcloud_', hisnick, '_', Sys.Date(), '.png', sep=''),width=500,height=500)
  par(mar=c(0,0,0,0))
  wordcloud(words_df3$words_names, words_df3$words_freq2, min.freq=0, scale=c(6,1), 
            max.words=100, random.order=F, colors=rainbow(100,v=0.8,end=0.5), rot.per=0)
  dev.off()
  
  png(paste('weibo_content_', hisnick, '_', Sys.Date(), '.png', sep=''),width=1000,height=1000)
  par(mfrow=c(2,2), mar=c(1,1,3,1), yaxt='n', 
      cex.main=2.5, col.main=rgb(0,0,0.5), cex.axis=2, bg=rgb(0.6,1,1))
  weibo_time <- strptime(substr(weibo_get$weibo_data$weibo_time, 12, 16), format='%H:%M')
  hist(weibo_time, breaks='hours', 
       main=paste('TA主要在神马时间发微博？', sep=''), 
       col=rgb(0,0.5,1), border=rgb(0,0.5,1), xlab=NULL, ylab=NULL)
  weibo_time <- strptime(weibo_get$weibo_data$weibo_time, format='%Y-%m-%d %H:%M')
  hist(weibo_time, breaks='months', 
       main=paste('TA的微博是不是越发越勤快？', sep=''), 
       col=rgb(0,0.5,1), border=rgb(0,0.5,1), xlab=NULL, ylab=NULL)
  wordcloud(words_df3$words_names, words_df3$words_freq2, min.freq=0, scale=c(7,1), 
            max.words=100, random.order=F, colors=rainbow(100,start=0.5,end=1), rot.per=0)
  plot(0, 0, type='n', xlim=c(0,100), ylim=c(0,100), axes=F)
  text(50, 50, paste(weibo_get$hisnick, weibo_get$nick, sep='\n'), 
       cex=6, font=2, col=rgb(0,0.3,1))
  dev.off()
}

