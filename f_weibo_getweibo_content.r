

# 首先还是微博登录的函数：
f_weibo_login <- function(name="****", pwd="****"){
  try(memory.limit(4000), silent=T)
  require(RCurl)
  require(digest)
  
  # 对ID的预处理
  name <- URLencode(name, reserved=T)
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
                  httpheader=myH, .params=pinfo, curl=cH, style="post")
  getCurlInfo(cH)[["cookielist"]]
  
  newurl <- strsplit(ttt[1], 'location.replace\\(\'')[[1]][2]
  newurl <- strsplit(newurl, '\'\\);')[[1]][1]
  newurl
  getURL(newurl, curl=cH, .encoding="UTF-8")
  getCurlInfo(cH)[["cookielist"]]
  return(cH)
}


# 然后是抓取数据的函数。目前只写了feeds部分的抓取，其他是类似的，而且会更简单一点，不需要刷新页面。
f_weibo_get <- function(cH=ch0, N=200, hisnick='chenyibo'){
  # 参数N是想要获取的微博条数。参数hisnick是对方的ID
  try(memory.limit(4000), silent=T)
  require(RCurl)
  require(RJSONIO)
  
  # 先看一下有多少页
  pg=1
  the1url <- paste('http://weibo.com/', hisnick, '/profile?page=', pg, sep='')
  the1get <- getURL(the1url, curl=cH, .encoding="UTF-8")
  write(the1get, "temp.txt")
  the1get <- readLines("temp.txt")
  
  idi <- grep('\\[\'oid\'\\]', the1get)
  oid <- strsplit(the1get[idi], '\\[\'oid\'\\] = \'')[[1]][2]
  oid <- strsplit(oid, '\';')[[1]][1]
  idi <- grep('\\[\'uid\'\\]', the1get)
  uid <- strsplit(the1get[idi], '\\[\'uid\'\\] = \'')[[1]][2]
  uid <- strsplit(uid, '\';')[[1]][1]
  
  # 微博信息
  numberi <- max(grep('node-type=\\\\"weibo\\\\">', the1get))
  number <- strsplit(the1get[numberi], 'node-type=\\\\"weibo\\\\">')[[1]][2]
  number <- strsplit(number, '<\\\\/strong>')[[1]][1]
  pages <- ceiling(min(as.numeric(number), N)/45)
  
  weibo_data <- c()
  
  # 循环读取页面
  for (pg in 1:pages){
    
    # 第一屏
    the1url <- paste('http://weibo.com/', hisnick, '/profile?page=', pg, sep='')
    the1get <- getURL(the1url, curl=cH, .encoding="UTF-8")
    write(the1get, "temp.txt")
    the1get <- readLines("temp.txt")
    
    # 看别人的时候是hisFeed，看自己的时候是myFeed(后面的url也略有差异，主要是刷新的时候需要用到uid)
    if(uid == oid){
      myfeedi <- grep('\"pid\":\"pl_content_myFeed\"', the1get)
    }
    if(uid != oid){
      myfeedi <- grep('\"pid\":\"pl_content_hisFeed\"', the1get)
    }
    a1 <- gsub('<script>STK && STK.pageletM && STK.pageletM.view\\(','',the1get[myfeedi])
    a1 <- gsub('\\)</script>','',a1)
    a1 <- fromJSON(a1)[['html']]
    write(a1, 'a1.txt')
    a1 <- readLines("a1.txt")
    
    # 最后一条微博的ID
    lastmidi <- max(grep('mid=\"', a1))
    lastmid <- strsplit(a1[lastmidi], 'mid=\"')[[1]][2]
    lastmid <- strsplit(lastmid, '\"')[[1]][1]
    
    # 于是第二屏
    the2url <- paste('http://weibo.com/aj/mblog/mbloglist?page=', pg, 
                     '&count=15&max_id=', lastmid, '&pre_page=', pg, '&end_id=&pagebar=0&uid=', oid, sep='')
    the2get <- getURL(the2url, curl=cH, .encoding="UTF-8")
    # write(the2get, "temp.txt")
    # the2get <- readLines("temp.txt")
    a2 <- fromJSON(the2get)[['data']]
    write(a2, 'a2.txt')
    a2 <- readLines("a2.txt")
    
    # 最后一条微博的ID
    lastmidi <- max(grep('mid=\"', a2))
    lastmid <- strsplit(a2[lastmidi], 'mid=\"')[[1]][2]
    lastmid <- strsplit(lastmid, '\"')[[1]][1]
    
    # 于是第三屏
    the3url <- paste('http://weibo.com/aj/mblog/mbloglist?page=', pg, 
                     '&count=15&max_id=', lastmid, '&pre_page=', pg, '&end_id=&pagebar=1&uid=', oid, sep='')
    the3get <- getURL(the3url, curl=cH, .encoding="UTF-8")
    # write(the3get, "temp.txt")
    # the3get <- readLines("temp.txt")
    a3 <- fromJSON(the3get)[['data']]
    write(a3, 'a3.txt')
    a3 <- readLines("a3.txt")
    
    # 筛选微博正文内容，连接起来
    a123 <- c(a1, a2, a3)
    index <- grep('node-type=\"feed_list_content\"', a123)
    a11 <- a123[index]
    a111 <- gsub('<[^<>]*>', '', a11)
    a1111 <- gsub('\t', '', a111)
    weibo_data <- c(weibo_data, a1111)
    gc()
    print(length(weibo_data))
  }
  
  file.remove("a1.txt")
  file.remove("a2.txt")
  file.remove("a3.txt")
  file.remove("temp.txt")
  
  # 去掉英文和数字，去掉@对象
  # weibo_data <- gsub('@[^ ]+ ', '', weibo_data)
  # weibo_data <- gsub('[0-9a-zA-Z]+', '', weibo_data)
  weibo_data <- weibo_data[1:min(as.numeric(number), N)]
  weibo_data <- weibo_data[!is.na(weibo_data)]
  return(weibo_data)
}

