

# 首先还是微博登录的函数：
f_weibo_login <- function(name='****', pwd='****'){
  try(memory.limit(4000), silent=T)
  require(RCurl)
  require(digest)
  require(RJSONIO)
  
  # 对ID的预处理
  name1 <- URLencode(name, reserved=T)
  name2 <- base64(name1)[1]
  
  d <- debugGatherer()
  cH <- getCurlHandle(followlocation=T, verbose=T, 
                      debugfunction=d$update, 
                      ssl.verifyhost=F, ssl.verifypeer=F, 
                      cookiejar='./cookies', cookiefile='./cookies')
  
  # 预登录
  preurl <- paste('http://login.sina.com.cn/sso/prelogin.php?entry=miniblog&callback=sinaSSOController.preloginCallBack&su=', 
                  name2, '&client=ssologin.js(v1.3.18)', sep='')
  prelogin <- getURL(preurl, curl=cH)
  preinfo <- fromJSON(gsub('^.*\\((.*)\\).*$','\\1',prelogin))
  servertime <- preinfo$servertime
  pcid <- preinfo$pcid
  nonce <- preinfo$nonce
  # 加密的过程
  pwd1 <- digest(pwd, algo='sha1', seria=F)
  pwd2 <- digest(pwd1, algo='sha1', seria=F)
  pwd3 <- digest(paste(pwd2, servertime, nonce, sep=''), algo='sha1', seria=F)
  pinfo=c(
    'service'='miniblog',
    'client'='ssologin.js(v1.3.18)',
    'entry'='weibo',
    'encoding'='UTF-8',
    'gateway'='1',
    'savestate'='7',
    'from'='',
    'useticket'='1',
    'su'=name2,
    'servertime'=servertime,
    'nonce'=nonce,
    'pwencode'='wsse',
    'sp'=pwd3,
    'vsnf'='1',
    'vsnval'='',
    'pcid'=pcid,
    'url'='http://weibo.com/ajaxlogin.php?framelogin=1&callback=parent.sinaSSOController.feedBackUrlCallBack',
    'returntype'='META',
    'ssosimplelogin'='1',
    'setdomain'='1')
  # 登录
  bkp_ctype <- Sys.getlocale('LC_CTYPE')
  if(bkp_ctype == 'zh_CN.UTF-8'){Sys.setlocale('LC_CTYPE', 'C')}
  ttt <- postForm('http://login.sina.com.cn/sso/login.php?client=ssologin.js(v1.3.18)', 
                  .params=pinfo, curl=cH, style='post')
  newurl <- gsub('^.*location.replace\\(\'(.+)\'\\);.*$', '\\1', ttt[1])
  x <- getURL(newurl, curl=cH, .encoding='UTF-8')
  getCurlInfo(cH)[['cookielist']]
  Sys.setlocale('LC_CTYPE', bkp_ctype)
  return(cH)
}


# 然后是抓取数据的函数。目前只写了feeds部分的抓取，其他是类似的，而且会更简单一点，不需要刷新页面。
f_weibo_get <- function(cH=ch0, N=200, hisnick='chenyibo'){
  # 参数N是想要获取的微博条数。参数hisnick是对方的ID
  try(memory.limit(4000), silent=T)
  require(RCurl)
  require(RJSONIO)
  require(XML)
  
  # 先看一下有多少页
  pg=1
  the1url <- paste('http://weibo.com/', hisnick, '/profile?page=', pg, sep='')
  the1get <- getURL(the1url, curl=cH, .encoding='UTF-8')
  oid <- gsub('^.*\\[\'oid\'\\] = \'([^\']+)\';.*$', '\\1', the1get)
  uid <- gsub('^.*\\[\'uid\'\\] = \'([^\']+)\';.*$', '\\1', the1get)
  number <- gsub('^.*<strong node-type=\\\\"weibo\\\\">([0-9]+)<\\\\/strong>.*$', '\\1', the1get)
  pages <- ceiling(min(as.numeric(number), N)/45)
  
  weibo_data <- c()
  # 循环读取页面
  for (pg in seq_len(pages)){
    # 第一屏
    the1url <- paste('http://weibo.com/', hisnick, '/profile?page=', pg, sep='')
    the1get <- getURL(the1url, curl=cH, .encoding="UTF-8")
    # 看别人的时候是hisFeed，看自己的时候是myFeed(后面的url也略有差异，主要是刷新的时候需要用到uid)
    myfeed <- paste('^.*<script>STK && STK.pageletM && STK.pageletM.view\\\\((\\{', 
                    ifelse(uid == oid, '\"pid\":\"pl_content_myFeed\"', '\"pid\":\"pl_content_hisFeed\"'), 
                    '.+\\})\\\\)</script>.*$', sep='')
    a1 <- gsub('^.*<script>STK && STK.pageletM && STK.pageletM.view\\((\\{\"pid\":\"pl_content_myFeed\".+?\\})\\)</script>.*$', '\\1', the1get)
    a1 <- fromJSON(a1)[['html']]
    # 最后一条微博的ID
    lastmid <- gsub('^.*mid=\"([0-9]+)\".*$', '\\1', a1)
    
    # 于是第二屏
    the2url <- paste('http://weibo.com/aj/mblog/mbloglist?page=', pg, 
                     '&count=15&max_id=', lastmid, '&pre_page=', pg, '&end_id=&pagebar=0&uid=', oid, sep='')
    the2get <- getURL(the2url, curl=cH, .encoding='UTF-8')
    a2 <- fromJSON(the2get)[['data']]
    # 最后一条微博的ID
    lastmid <- gsub('^.*mid=\"([0-9]+)\".*$', '\\1', a2)
    
    # 于是第三屏
    the3url <- paste('http://weibo.com/aj/mblog/mbloglist?page=', pg, 
                     '&count=15&max_id=', lastmid, '&pre_page=', pg, '&end_id=&pagebar=1&uid=', oid, sep='')
    the3get <- getURL(the3url, curl=cH, .encoding='UTF-8')
    a3 <- fromJSON(the3get)[['data']]
    
    # 筛选微博正文内容
    b1 <- htmlParse(a1, encoding='UTF-8')
    b2 <- htmlParse(a2, encoding='UTF-8')
    b3 <- htmlParse(a3, encoding='UTF-8')
    c1 <- getNodeSet(b1, path='//div[@node-type="feed_list_content"]')
    c2 <- getNodeSet(b2, path='//p[@node-type="feed_list_content"]')
    c3 <- getNodeSet(b3, path='//p[@node-type="feed_list_content"]')
    c123 <- c(c1, c2, c3)
    d123 <- sapply(c123, xmlValue)
    weibo_data <- c(weibo_data, d123)
    gc()
    print(length(weibo_data))
  }
  
  weibo_data <- weibo_data[!is.na(weibo_data)]
  return(weibo_data)
}

