

# 然后是抓取数据的函数。
f_weibo_search <- function(cH=ch0, mykeyword='测试一下'){
# mykeyword就是关键词
require(rjson)
require(RCurl)
memory.limit(4000)

pg=1
mykeyword1 <- charToRaw(iconv(mykeyword,'GBK','UTF-8'))
mykeyword2 <- paste('%25', mykeyword1, sep='', collapse='')
the1url <- paste('s.weibo.com/weibo/', mykeyword2, '&page=', pg, sep='')
the1get <- getURL(the1url, curl=cH, .encoding="UTF-8")
write(the1get, "temp.txt")
the1get <- readLines("temp.txt")

# 微博信息
infoi <- grep('\"pid\":\"pl_weibo_feedlist\"', the1get)
a1 <- gsub('<script>STK && STK.pageletM && STK.pageletM.view\\(','',the1get[infoi])
a1 <- gsub('\\)</script>','',a1);a1 <- fromJSON(a1)$html;write(a1, 'a1.txt')
a1 <- readLines("a1.txt")
number1 <- max(grep('search_page_M', a1))
number2 <- strsplit(a1[number1], '</a>')[[1]]
number3 <- number2[length(number2)-2]
number4 <- strsplit(number3, '>')[[1]]
number5 <- number4[length(number4)]
pages <- as.integer(number5)

weibo_data <- data.frame(mid=NULL, url=NULL, nickname=NULL, content=NULL)

# 循环读取页面
for (pg in 1:pages){

the1url <- paste('s.weibo.com/weibo/', mykeyword2, '&page=', pg, sep='')
the1get <- getURL(the1url, curl=cH, .encoding="UTF-8")
write(the1get, "temp.txt")
the1get <- readLines("temp.txt")

# 微博信息
infoi <- grep('\"pid\":\"pl_weibo_feedlist\"', the1get)
a1 <- gsub('<script>STK && STK.pageletM && STK.pageletM.view\\(','',the1get[infoi])
a1 <- gsub('\\)</script>','',a1);a1 <- fromJSON(a1)$html;write(a1, 'a1.txt')
a1 <- readLines("a1.txt")

infoi1 <- grep('class=\"feed_list\"', a1)
infoi2 <- grep('feed_list_content', a1)
infoi3 <- grep('class=\"date\" node-type=\"feed_list_item_date\"', a1)
a1_mid <- a1[infoi1]
a1_content <- a1[infoi2+1]
a1_url0 <- a1[infoi3]
a1_url <- a1_url0[-grep('target=\"_blank\" class=\"date\"', a1_url0)]
rm(a1_url0)

a2_mid <- gsub('^.*mid=\"([0-9]+)\" .*$', '\\1', a1_mid)
a2_url <- gsub('^.*<a href=\"(.+)\" title=\".*$', '\\1', a1_url)
a2_nickname <- gsub('^.*<a nick-name=\"([^\"]+)\" href=\".*$', '\\1', a1_content)
a2_content0 <- gsub('^.*<em>(.*)</em>.*$', '\\1', a1_content)
a2_content <- gsub('<[^<>]*>', '', a2_content0)
# 要不要去掉转发的内容?
# a2_content <- gsub('//@.*', '', a2_content)

x <- abs(length(a2_mid)-length(a2_url))+
abs(length(a2_mid)-length(a2_nickname))+
abs(length(a2_mid)-length(a2_content))
if(x == 0){
weibo_data <- rbind(weibo_data, 
data.frame(mid=a2_mid, url=a2_url, nickname=a2_nickname, content=a2_content))
}
if(x > 0){
print('warning!')
}
gc()
print(nrow(weibo_data))
}

file.remove("a1.txt")
file.remove("temp.txt")

weibo_data$mid <- as.character(weibo_data$mid)
weibo_data$url <- as.character(weibo_data$url)
weibo_data$nickname <- as.character(weibo_data$nickname)
weibo_data$content <- as.character(weibo_data$content)
return(weibo_data)
}

