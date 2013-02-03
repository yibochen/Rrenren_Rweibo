

# 登录
source('f_weibo_login.r', encoding='UTF-8')
ch0 <- f_weibo_login('myemail', 'mypwd')

# 获取微博转发数据
source('f_weibo_repost.r', encoding='UTF-8')
weibo_repost <- f_repost_path(cH=ch0, root_url='http://weibo.com/1782871497/y2Z1WtI7D')
save(weibo_repost, file='weibo_saved_repost_qiushiid.RData')
weibo_repost <- f_repost_path(cH=ch0, root_url='http://weibo.com/2514669664/zgmfEpwKX')
save(weibo_repost, file='weibo_saved_repost_yihui.RData')
weibo_repost <- f_repost_path(cH=ch0, root_url='http://weibo.com/2043157342/zcdFaeuD1')
save(weibo_repost, file='weibo_saved_repost_xiaonan.RData')
sum(weibo_repost[[2]][, 1] != 0)
sum(weibo_repost[[2]][, 1] == 0)
# 有的显示转发1，但其实有14条

source('f_weibo_app2.r', encoding='UTF-8')
f_weibo_app2('qiushiid')
f_weibo_app2('yihui')
f_weibo_app2('xiaonan')

# 1、http://weibo.com/1726276573/y8v9vob0V (几百)
# 2、http://weibo.com/1782871497/y2Z1WtI7D (几千)
# 3、http://weibo.com/1782871497/eAsjygS9YSg (几万)

