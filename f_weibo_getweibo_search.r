
# 首先还是微博登录的函数：
f_weibo_login <- function(name="****", pwd="****"){
memory.limit(4000)
require(RCurl)
require(digest)

# 对ID的预处理
name <- gsub('@', '%40', name)
name <- base64(name)[1]

# 常规的打包，具体没仔细研究
myH <- c("Host"="login.sina.com.cn",
"User-Agent"="Mozilla/5.0 (Windows NT 5.1; rv:2.0.1) Gecko/20100101 Firefox/4.0.1",
"Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
"Accept-Language"="zh-cn,zh;q=0.5",
"Accept-Encoding"="gzip, deflate",
"Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7",
"Keep-Alive"="115",
"Connection"="keep-alive",
"Referer"="http://weibo.com/",
"Content-Type"="application/x-www-form-urlencoded; charset=UTF-8")
d <- debugGatherer()
cH <- getCurlHandle(debugfunction=d$update, verbose=T,
ssl.verifyhost=F, ssl.verifypeer=F, followlocation=T, cookiefile="cc.txt")

# 预登录的页面。这里貌似应该用一些正则匹配的，也没有仔细研究
preurl <- paste("http://login.sina.com.cn/sso/prelogin.php?entry=miniblog&callback=sinaSSOController.preloginCallBack&su=", 
name, "&client=ssologin.js(v1.3.18)", sep='')
prelogin <- readLines(preurl, warn=F)
servertime <- strsplit(prelogin, '\"servertime\":')[[1]][2]
servertime <- strsplit(servertime, ',\"pcid\"')[[1]][1]
pcid <- strsplit(prelogin, '\"pcid\":\"')[[1]][2]
pcid <- strsplit(pcid, '\",\"nonce\"')[[1]][1]
nonce <- strsplit(prelogin, '\"nonce\":\"')[[1]][2]
nonce <- strsplit(nonce, '\"}')[[1]][1]
servertime
pcid
nonce
# 加密的过程
pwd1 <- digest(pwd, algo='sha1', seria=F)
pwd2 <- digest(pwd1, algo='sha1', seria=F)
pwd3 <- digest(paste(pwd2, servertime, nonce, sep=''), algo='sha1', seria=F)
getCurlInfo(cH)[["cookielist"]]
pinfo=c(
"service"="miniblog",
"client"="ssologin.js(v1.3.18)",
"entry"="weibo",
"encoding"="UTF-8",
"gateway"="1",
"savestate"="7",
"from"="",
"useticket"="1",
"su"=name,
"servertime"=servertime,
"nonce"=nonce,
"pwencode"="wsse",
"sp"=pwd3,
"vsnf"="1",
"vsnval"="",
"pcid"=pcid,
"url"="http://weibo.com/ajaxlogin.php?framelogin=1&callback=parent.sinaSSOController.feedBackUrlCallBack",
"returntype"="META",
"ssosimplelogin"="1",
"setdomain"="1"
)
# 登录
ttt <- postForm("http://login.sina.com.cn/sso/login.php?client=ssologin.js(v1.3.18)", 
httpheader=myH, .params=pinfo, curl=cH, style="post", .encoding="UTF-8")
getCurlInfo(cH)[["cookielist"]]

newurl <- strsplit(ttt[1], 'location.replace\\(\'')[[1]][2]
newurl <- strsplit(newurl, '\'\\);')[[1]][1]
newurl
getURL(newurl, curl=cH, .encoding="UTF-8")
getCurlInfo(cH)[["cookielist"]]
return(cH)
}



# 然后是抓取数据的函数。
f_weibo_get <- function(cH=ch0, mykeyword='测试一下'){
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



