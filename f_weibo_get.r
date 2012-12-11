

# 然后是抓取数据的函数。目前只写了feeds部分的抓取，其他是类似的，而且会更简单一点，不需要刷新页面。
f_weibo_get <- function(cH=ch0, N=200, hisnick='chenyibo'){
  # 参数N是想要获取的微博条数。参数hisnick是对方的ID
  # 根据操作系统选择加载包
  sysname <- Sys.info()['sysname']
  if(length(grep('Windows', sysname)) == 1){
    try(memory.limit(4000), silent=T)
    require(RJSONIO)
  } else{
    require(rjson)
  }
  require(RCurl)
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
    the1get <- getURL(the1url, curl=cH, .encoding='UTF-8')
    # 看别人的时候是hisFeed，看自己的时候是myFeed(后面的url也略有差异，主要是刷新的时候需要用到uid)
    myfeed <- paste('^.*<script>STK && STK.pageletM && STK.pageletM.view\\((\\{', 
                    ifelse(uid == oid, '\"pid\":\"pl_content_myFeed\"', '\"pid\":\"pl_content_hisFeed\"'), 
                    '.+?\\})\\)</script>.*$', sep='')
    a1 <- gsub(myfeed, '\\1', the1get)
    a1 <- fromJSON(a1)[['html']]
    # 最后一条微博的ID
    if(length(grep('mid=\"([0-9]+)\"', a1)) > 0){
      lastmid <- gsub('^.*mid=\"([0-9]+)\".*$', '\\1', a1)
    } else{
      lastmid <- ''
    }
    
    # 于是第二屏
    the2url <- paste('http://weibo.com/aj/mblog/mbloglist?page=', pg, 
                     '&count=15&max_id=', lastmid, '&pre_page=', pg, '&end_id=&pagebar=0&uid=', oid, sep='')
    the2get <- getURL(the2url, curl=cH, .encoding='UTF-8')
    a2 <- fromJSON(the2get)[['data']]
    # 最后一条微博的ID
    if(length(grep('mid=\"([0-9]+)\"', a2)) > 0){
      lastmid <- gsub('^.*mid=\"([0-9]+)\".*$', '\\1', a2)
    } else{
      lastmid <- ''
    }
    
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

