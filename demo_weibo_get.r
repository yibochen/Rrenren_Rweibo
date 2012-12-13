

# 登录
source('f_weibo_login.r')
ch0 <- f_weibo_login('myemail', 'mypwd')

# 获取微博数据
source('f_weibo_get.r')
weibo_get <- f_weibo_get(cH=ch0, N=10000, hisnick='chenyibo', is_e=F)
save(weibo_get, file='weibo_saved_chenyibo.RData')
weibo_get <- f_weibo_get(cH=ch0, N=10000, hisnick='2153625382', is_e=F)
save(weibo_get, file='weibo_saved_2153625382.RData')
weibo_get <- f_weibo_get(cH=ch0, N=10000, hisnick='mmc1006', is_e=F)
save(weibo_get, file='weibo_saved_mmc1006.RData')
weibo_get <- f_weibo_get(cH=ch0, N=10000, hisnick='sunbjt', is_e=F)
save(weibo_get, file='weibo_saved_sunbjt.RData')
weibo_get <- f_weibo_get(cH=ch0, N=10000, hisnick='dr4x', is_e=F)
save(weibo_get, file='weibo_saved_dr4x.RData')
weibo_get <- f_weibo_get(cH=ch0, N=10000, hisnick='diaoyeniunan', is_e=T)
save(weibo_get, file='weibo_saved_diaoyeniunan.RData')
weibo_get <- f_weibo_get(cH=ch0, N=10000, hisnick='taobaoued', is_e=T)
save(weibo_get, file='weibo_saved_taobaoued.RData')
weibo_get <- f_weibo_get(cH=ch0, N=10000, hisnick='shujuyingjia', is_e=T)
save(weibo_get, file='weibo_saved_shujuyingjia.RData')
weibo_get <- f_weibo_get(cH=ch0, N=10000, hisnick='ebaycoe', is_e=T)
save(weibo_get, file='weibo_saved_ebaycoe.RData')

source('f_weibo_app1.r')
f_weibo_app1('chenyibo')
f_weibo_app1('2153625382')
f_weibo_app1('mmc1006')
f_weibo_app1('sunbjt')
f_weibo_app1('dr4x')
f_weibo_app1('diaoyeniunan')
f_weibo_app1('taobaoued', 7, 1.5)
f_weibo_app1('shujuyingjia', 7, 2)
f_weibo_app1('ebaycoe')

