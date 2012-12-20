

# 微博关键词及时间分布
f_weibo_app1 <- function(hisnick='chenyibo', 
                         scale_a=7, scale_b=1, 
                         cutday='2012-12-21', 
                         equal_length=T, 
                         dicdir=NULL){
  load(paste('weibo_saved_', hisnick, '.RData', sep=''))
  pkgs <- installed.packages()[, 1]
  if(!'rJava' %in% pkgs){
    install.packages('rJava', 
                     repos='http://mirrors.ustc.edu.cn/CRAN/')
  }
  if(!'rmmseg4j' %in% pkgs){
    install.packages('rmmseg4j', 
                     repos='http://mirrors.ustc.edu.cn/CRAN/')
  }
  if(!'wordcloud' %in% pkgs){
    install.packages('wordcloud', 
                     repos='http://mirrors.ustc.edu.cn/CRAN/')
  }
  
  require(rmmseg4j)
  require(wordcloud)
  weibo_data <- weibo_get$weibo_data[order(
    -as.numeric(as.POSIXlt(weibo_get$weibo_data$weibo_time))), ]
  weibo_data_all <- sapply(strsplit(weibo_data$weibo_content, '//'), '[', 1)
  flag <- min(which(as.POSIXlt(weibo_data$weibo_time) <= as.POSIXlt(cutday)))
  weibo_data_1 <- sapply(strsplit(weibo_data$weibo_content[flag:nrow(weibo_data)], '//'), '[', 1)
  weibo_data_2 <- sapply(strsplit(weibo_data$weibo_content[(flag-1):1], '//'), '[', 1)
  if(equal_length){
    weibo_data_1 <- weibo_data_1[1:min(length(weibo_data_2),length(weibo_data_1))]
    weibo_data_2 <- weibo_data_2[1:min(length(weibo_data_2),length(weibo_data_1))]
  }
  
  # 分词
  f_fenci <- function(input=weibo_data){
    weibo_data <- input[input != '' & !is.na(input)]
    f_cut <- function(x){
      unlist(strsplit(mmseg4j(x, dicDir=dicdir), ' '))
    }
    words <- unlist(lapply(weibo_data, f_cut))
    words <- words[!words  %in% c('转发','回复')]
    words[words == '联网'] <- '互联网'
    words[words == '据分析'] <- '数据分析'
    
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
    
    return(words_df3)
  }
  
  words_df3 <- f_fenci(weibo_data_all)
  words_df1 <- f_fenci(weibo_data_1)
  words_df2 <- f_fenci(weibo_data_2)
  words_1 <- as.character(words_df1[order(-words_df1$words_freq2)[1:9], 1])
  words_2 <- as.character(words_df2[order(-words_df2$words_freq2)[1:9], 1])
  
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
  wordcloud(words_df3$words_names, words_df3$words_freq2, min.freq=0, scale=c(scale_a, scale_b), 
            max.words=100, random.order=F, colors=rainbow(100,start=0.5,end=1), rot.per=0)
  plot(0, 0, type='n', xlim=c(0,100), ylim=c(0,100), axes=F)
  text(0, 50, paste(weibo_get$nick, '\n', 
                    '在', cutday, '之前的\n', 
                    length(weibo_data_1), '条微博的关键词是：\n', 
                    paste(c(words_1[1:3], ''), collapse=';'), '\n', 
                    paste(c(words_1[4:6], ''), collapse=';'), '\n', 
                    paste(c(words_1[7:9]), collapse=';'), '\n\n', 
                    '在', cutday, '之后的\n', 
                    length(weibo_data_2), '条微博的关键词是：\n', 
                    paste(c(words_2[1:3], ''), collapse=';'), '\n', 
                    paste(c(words_2[4:6], ''), collapse=';'), '\n', 
                    paste(c(words_2[7:9]), collapse=';'), 
                    sep=''), 
       cex=3, font=2, adj=0, col=rgb(0,0.3,1))
  dev.off()
}

