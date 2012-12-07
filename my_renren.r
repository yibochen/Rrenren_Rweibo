


source('f_renren_onlymyfriend.r')

onlyfriend <- f_renren_onlymyfriend(name='myemail', pwd='mypwd')

save(onlyfriend, file='renren_only_friend.RData')







source('f_renren_friend.r')

friend_my <- f_renren_friend(name="myemail",pwd="mypwd",N=0)

friend_all <- f_renren_friend(name="myemail",pwd="mypwd",N=1)

save.image('renren_friend.RData')

