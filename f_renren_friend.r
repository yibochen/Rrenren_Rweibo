

f_renren_friend=function(name="****",pwd="******",N=0){


library(RCurl)
memory.limit(4000)

myH=c(
"User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.2.12) Gecko/20101026 Firefox/3.6.12",
"Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
"Accept-Language"="zh-cn,zh;q=0.5",
#"Accept-Encoding"="gzip,deflate",
"Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7",
"Keep-Alive"="115",
"Connection"="keep-alive",
#"Host"="status.renren.com",
"Content-Type"="application/x-www-form-urlencoded; charset=UTF-8",
#"Content-Length"=as.character(nchar(xx)*2+1),
"Pragma"="no-cache",
#"Referer"="http://status.renren.com/ajaxproxy.htm",
"Cache-Control"="no-cache"
)

d=debugGatherer()
cH=getCurlHandle(
debugfunction=d$update,verbose=T,
ssl.verifyhost=F,ssl.verifypeer=F,
followlocation=T,
cookiefile="cc.txt")

pinfo=c(
"email"=name,
"password"=pwd,
"origURL"="http://www.renren.com/Home.do",
"domain"="renren.com"
)

pinfo=iconv(pinfo,from="GB18030",to="UTF-8")

ttt=postForm("http://passport.renren.com/PLogin.do",httpheader=myH,
.params=pinfo,curl=cH,style="post")

getCurlInfo(cH)[["cookielist"]]

h=getURL("http://www.renren.com",curl=cH,.encoding="gbk")
write(h,"temp.txt")
hh=readLines("temp.txt",encoding="UTF-8")
# file.remove("temp.txt")
rm(h)
hh=hh[grep("user : ",hh)]
hh=gsub("[^0-9]","",hh)
uid0=hh

f0=function(uid=uid0){

memory.limit(4000)
library(RCurl)

friend=data.frame(u0=0,id0=0,u1=0,id1=0,loc=0)

thisurl=paste("http://friend.renren.com/GetFriendList.do?curpage=0&id=",uid,sep="")
h=getURL(thisurl,curl=cH,.encoding="gbk")

write(h,"temp.txt")
hh=readLines("temp.txt",encoding="UTF-8")
rm(h)

imax=0

if(length(grep("最后页",hh))>0){
imax=strsplit(hh[grep("最后页",hh)[1]],"curpage=")[[1]][2]
imax=strsplit(imax,"&amp")[[1]][1]}
if(length(grep("最后页",hh))==0 & length(grep("下一页",hh))>0){
nextpage=grep("下一页",hh)[1]
hh2=hh[(nextpage-30):(nextpage-1)]
lastpage=hh2[max(grep("curpage=",hh2))]
imax=strsplit(lastpage,"curpage=")[[1]][2]
imax=strsplit(imax,"&amp")[[1]][1]
}
imax=as.numeric(imax)

u0=strsplit(hh[grep("的好友</title>",hh)],"的好友</title>")[[1]][1]
u0=strsplit(u0,"人人网 - ")[[1]][2]

ii=grep("即时聊天",hh)

if(length(ii)==0){
hh=hh[grep("打个招呼",hh)[1]]
ff=strsplit(hh,"event,'")[[1]][2]
ff=strsplit(ff,"'[)]")[[1]][1]
ff1=strsplit(ff,"','")[[1]][1]
ff2=strsplit(ff,"','")[[1]][2]
friendnew=data.frame(u0=0,id0=0,u1=ff2,id1=ff1,loc=0)
friend=rbind(friend,friendnew)
if((nrow(friend) %% 90)==0){Sys.sleep(10)}
}

if(length(ii)>=1){

hh=hh[ii]

for(iii in 1:length(ii)){
ff=strsplit(hh[iii],":talkto[(]")[[1]][2]
ff=strsplit(ff,"'[)];return false")[[1]][1]
ff1=strsplit(ff,", '")[[1]][1]
ff2=strsplit(ff,", '")[[1]][2]
friendnew=data.frame(u0=0,id0=0,u1=ff2,id1=ff1,loc=0)
friend=rbind(friend,friendnew)
if((nrow(friend) %% 90)==0){Sys.sleep(10)}
}
}

if(imax>=1){
for(i in 1:imax){
thisurl=paste("http://friend.renren.com/GetFriendList.do?curpage=",i,"&id=",uid,sep="")
h=getURL(thisurl,curl=cH,.encoding="gbk")

write(h,"temp.txt")
hh=readLines("temp.txt",encoding="UTF-8")
# file.remove("temp.txt")
rm(h)

ii=grep("即时聊天",hh)

if(length(ii)==0){
hh=hh[grep("打个招呼",hh)[1]]
ff=strsplit(hh,"event,'")[[1]][2]
ff=strsplit(ff,"'[)]")[[1]][1]
ff1=strsplit(ff,"','")[[1]][1]
ff2=strsplit(ff,"','")[[1]][2]
friendnew=data.frame(u0=0,id0=0,u1=ff2,id1=ff1,loc=0)
friend=rbind(friend,friendnew)
if((nrow(friend) %% 90)==0){Sys.sleep(10)}
}


if(length(ii)>=1){

hh=hh[ii]

for(iii in 1:length(ii)){
ff=strsplit(hh[iii],":talkto[(]")[[1]][2]
ff=strsplit(ff,"'[)];return false")[[1]][1]
ff1=strsplit(ff,", '")[[1]][1]
ff2=strsplit(ff,", '")[[1]][2]
friendnew=data.frame(u0=0,id0=0,u1=ff2,id1=ff1,loc=0)
friend=rbind(friend,friendnew)
if((nrow(friend) %% 90)==0){Sys.sleep(10)}
}
}
}
}

friend$u0=u0
friend$id0=uid
friend=friend[-1,]

file.remove("temp.txt")
return(friend)
}

friend0=f0(uid=uid0)
tmp=friend0
ll=nrow(friend0)

if(N==1){

for(num in 1:ll){

userid=friend0$id1[num]
username=friend0$u1[num]
if((userid %in% tmp$id0)==F){
friend1=f0(uid=userid)
tmp=rbind(tmp,friend1)
print(c(num,username,nrow(friend1),date()))
rm(friend1)
gc()
}
}
}

return(tmp)

}









