
# 登录
source('f_weibo_login.r', encoding='UTF-8')
ch0 <- f_weibo_login('myemail', 'mypwd')

# 获取微博数据
source('f_weibo_get.r', encoding='UTF-8')
weibo_get <- f_weibo_get(cH=ch0, N=10000, hisnick='1199996942', is_e=F)
save(weibo_get, file='weibo_saved_1199996942.RData')
weibo_get <- f_weibo_get(cH=ch0, N=10000, hisnick='chenyibo', is_e=F)
save(weibo_get, file='weibo_saved_chenyibo.RData')
weibo_get <- f_weibo_get(cH=ch0, N=10000, hisnick='taobaoued', is_e=T)
save(weibo_get, file='weibo_saved_taobaoued.RData')

source('f_weibo_app1.r', encoding='UTF-8')
f_weibo_app1('1199996942', cutday='2012-04-01')
f_weibo_app1('chenyibo', cutday='2012-04-01')
f_weibo_app1('taobaoued')

