

# 登录
source('f_weibo_login.r')
ch0 <- f_weibo_login('myemail', 'mypwd')

# 获取微博数据（这里只做了我自己的版本，10000是个足够大的数字）
source('f_weibo_get.r')
weibo_get <- f_weibo_get(cH=ch0, N=10000, hisnick='sunbjt')
save(weibo_get, file='weibo_saved_sunbjt.RData')
weibo_get <- f_weibo_get(cH=ch0, N=10000, hisnick='chenyibo')
save(weibo_get, file='weibo_saved_chenyibo.RData')
weibo_get <- f_weibo_get(cH=ch0, N=10000, hisnick='wheatfield')
save(weibo_get, file='weibo_saved_wheatfield.RData')
weibo_get <- f_weibo_get(cH=ch0, N=10000, hisnick='dr4x')
save(weibo_get, file='weibo_saved_dr4x.RData')

source('f_weibo_app1.r')
f_weibo_app1('sunbjt')
f_weibo_app1('chenyibo')
f_weibo_app1('wheatfield')
f_weibo_app1('dr4x')

