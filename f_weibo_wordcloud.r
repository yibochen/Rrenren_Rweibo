

# 亮哥指导我可以用个人词频与公共词频做比较，来筛选关键词。所以我又做了生成词云的函数。
f_weibo_wordcloud <- function(weibo_data=weibo_10000_0, hisnick='chenyibo'){
  require(wordcloud)
  
  # 分词
  require(rmmseg4j)
  f_cut <- function(x){
    unlist(strsplit(mmseg4j(x, reload=T), ' '))
  }
  words <- unlist(mapply(f_cut, weibo_data))
  words <- words[words != 'na']
  words <- words[words != '转发']
  names(words) <- NULL
  
  # 统计词频
  words_freq <- sort(table(words), dec=T)
  words_names <- names(words_freq)
  words_length <- nchar(words_names)
  
  # 加载搜狗实验室的词频文件
  SogouLabDic <- read.table('SogouLabDic.dic', fill=T, head=F)
  
  words_df <- data.frame(words_names=words_names, words_freq=words_freq, words_length=words_length)
  # 只做两三个字的词，简单一点。。。
  words_df <- words_df[words_df$words_length %in% c(2,3), ]
  names(SogouLabDic)[1] <- 'words_names'
  SogouLabDic <- SogouLabDic[SogouLabDic[,1] %in% words_df$words_names, ]
  
  words_df2 <- merge (words_df, SogouLabDic, by='words_names', all.x=T)
  # 可以筛选名词和动词。不过似乎没有必要，因为形容词副词什么的也能够体现用词风格嘛
  words_df2 <- words_df2[grep('^[NV],$',words_df2$V3), ]
  
  # 匹配不到的扔掉
  words_df3 <- words_df2[!is.na(words_df2$V2), ]
  words_df3$words_freq2 <- words_df3$words_freq * log(max(words_df3$V2)/words_df3$V2)
  
  words_df3 <- words_df3[order(-words_df3$words_freq2), ][1:50, ]
  # words_df3$words_rank <- ceiling(rank(words_df3$words_freq2))
  # words_df3$words_rank <- ceiling(words_df3$words_rank*50/max(words_df3$words_rank))
  
  # 做词云（这个包貌似对中文支持不是很好）
  png(paste('weibo_wordcloud_', Sys.Date(), '_', hisnick, '.png', sep=''),width=500,height=500)
  par(mar=c(0,0,0,0))
  wordcloud(words_df3$words_names, words_df3$words_freq2, min.freq=0,
            scale=c(6+(max(words_df3$words_freq2)/min(words_df3$words_freq2)-3.8)*0.65,1), 
            max.words=50, random.order=F, colors=terrain.colors(50,1))
  dev.off()
}

