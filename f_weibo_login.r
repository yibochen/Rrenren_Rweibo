

# 微博登录的函数
f_weibo_login <- function(name='****', pwd='****'){
  # 根据操作系统选择加载包
  pkgs <- installed.packages()[, 1]
  if(!'digest' %in% pkgs){
    install.packages('digest', 
                     repos='http://mirrors.ustc.edu.cn/CRAN/')
  }
  if(!'RCurl' %in% pkgs){
    install.packages('RCurl')
  }
  if(!'RJSONIO' %in% pkgs){
    install.packages('RJSONIO', 
                     repos='http://mirrors.ustc.edu.cn/CRAN/')
  }
  
  sysname <- Sys.info()['sysname']
  if(length(grep('Windows', sysname)) == 1){
    try(memory.limit(4000), silent=T)
    require(RJSONIO)
  } else{
    require(RJSONIO)
  }
  require(RCurl)
  require(digest)
  
  # ID预处理
  name1 <- URLencode(name, reserved=T)
  name2 <- base64(name1)[1]
  
  d <- debugGatherer()
  cH <- getCurlHandle(followlocation=T, verbose=T, 
                      debugfunction=d$update, 
                      ssl.verifyhost=F, ssl.verifypeer=F, 
                      cookiejar='./cookies', cookiefile='./cookies')
  
  # 预登录


###########RSA加密###################################
#library(base64)
#su <- base64Encode(name)[1]
#url_prelogin <- 'http://login.sina.com.cn/sso/prelogin.php?entry=weibo&callback=sinaSSOController.preloginCallBack&su=&rsakt=mod&client=ssologin.js(v1.4.5)&_=1364875106625'
#url_login <- 'http://login.sina.com.cn/sso/login.php?client=ssologin.js(v1.4.5)'
#resp <- getURL(url_prelogin, curl=cH)
#json_data <- substr(resp,regexpr("\\(",resp)+1,regexpr("\\)",resp)-1)
#data <- fromJSON(json_data)
#servertime <- data$servertime
#nonce <- data$nonce
#pubkey <- data$pubkey
#rsakv <- data$rsakv
# 以下超strtio限了，无法对pubkey进行十进制化
#rsaPublickey <- strtoi(paste('0x',pubkey,sep=''))
# 接下来需要对sp进行rsa加密，无法去实现了。。。


  preurl <- 'http://login.sina.com.cn/sso/prelogin.php?entry=weibo&callback=sinaSSOController.preloginCallBack&su=&rsakt=mod&client=ssologin.js(v1.4.5)'
  prelogin <- getURL(preurl, curl=cH)
  preinfo <- fromJSON(gsub('^.*\\((.*)\\).*$','\\1',prelogin))
  servertime <- preinfo$servertime
#  pcid <- preinfo$pcid
#  nonce <- preinfo$nonce
  # 加密的过程
#  pwd1 <- digest(pwd, algo='sha1', seria=F)
#  pwd2 <- digest(pwd1, algo='sha1', seria=F)
#  pwd3 <- digest(paste(pwd2, servertime, nonce, sep=''), algo='sha1', seria=F)
#  pinfo=c(
#   'service'='miniblog',
#    'client'='ssologin.js(v1.3.18)',
#    'entry'='weibo',
#    'encoding'='UTF-8',
#    'gateway'='1',
#    'savestate'='7',
#    'from'='',
#    'useticket'='1',
#    'su'=name2,
#    'servertime'=servertime,
#    'nonce'=nonce,
#    'pwencode'='wsse',
#    'sp'=pwd3,
#    'vsnf'='1',
#    'vsnval'='',
#    'pcid'=pcid,
#    'url'='http://weibo.com/ajaxlogin.php?framelogin=1&callback=parent.sinaSSOController.feedBackUrlCallBack',
#    'returntype'='META',
#    'ssosimplelogin'='1',
#    'setdomain'='1')
################无加密部分#####################
pinfo=c(
entry = 'sso',
gateway = '1',
from = '',
savestate = '30',
useticket = '0',
pagerefer = 'http://login.sina.com.cn/sso/login.php?client=ssologin.js(v1.4.13)',
vsnf = '1',
su= name2,
service = 'sso',
sp = pwd,
sr = '1280*1024',
encoding = 'UTF-8',
prelt = '0',
callback = 'parent.sinaSSOController.loginCallBack',
returntype = 'IFRAME',
setdomain = '1')
###############################################
  # 登录
  bkp_ctype <- Sys.getlocale('LC_CTYPE')
  if(bkp_ctype == 'zh_CN.UTF-8'){Sys.setlocale('LC_CTYPE', 'C')}
  x <- try(ttt <- postForm('http://login.sina.com.cn/sso/login.php?client=ssologin.js(v1.4.13)', 
                           .params=pinfo, curl=cH, style='POST'), silent=T)
  if(class(x) == 'try-error'){cat('no!!!!!!');return(NULL)}
print(ttt[1])
  newurl <- gsub('location.replace\\(\'(.*?)\"\\)', '\\1', ttt[1])
  x <- try(x <- getURL(newurl, curl=cH, .encoding='UTF-8'), silent=T)
#print(x)
#print(class(x))
  Sys.setlocale('LC_CTYPE', bkp_ctype)
  if(class(x) == 'try-error'){cat('no!!!!!!');return(NULL)}
  getCurlInfo(cH)[['cookielist']]
  return(cH)
}

