left_cx <- 1024/2
left_cy <- 768/2

width <- 150
height <- 180

left <- left_cx- width/2
right <- left_cx+ width/2
top <- left_cy - height/2
bottom <- left_cy+ height/2


0,0      512, 0     1024,0
0,384    512, 384   1024,384
0,768    512, 768   1024,768


a_left <- 190
a_right <- 835
a_1 <- a_left+(a_right-a_left)/4
a_2 <- a_left+(a_right-a_left)/4*2
a_3 <- a_left+(a_right-a_left)/4*3
a_4 <- a_right

a2_left <- 210
a2_right <- 820
a2_1 <- a2_left+(a2_right-a2_left)/6
a2_2 <- a2_left+(a2_right-a2_left)/6*2
a2_3 <- a2_left+(a2_right-a2_left)/6*3
a2_4 <- a2_left+(a2_right-a2_left)/6*4
a2_5 <- a2_left+(a2_right-a2_left)/6*5
a2_6 <- a2_left+(a2_right-a2_left)/6*6