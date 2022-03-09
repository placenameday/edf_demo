dt <- read_csv("a03.csv", locale=locale(encoding = 'GB18030'))
dtn <- select(dt, 作答ID, 作答时长, 20:98)
dtnn <- select(dtn, -Q2_1, -Q3_1, -Q4_1, -Q5_1, -Q3_8, -Q4_7, -Q5_8)



cnma <- c("ID", "time", "reward", "next", "phone", "sex", "age", "edu", "statu")
anxno <- (1:40)
maasno <- (1:15)
phono <- (1:10)
anxno_n <- paste("Anxe", anxno, sep = "")
maas_n <- paste("Maas", maasno, sep = "")
phono_n <- paste("Phoneadd", phono, sep="")
mycoln <- c(cnma, anxno_n, maas_n, phono_n)
mycoln <- as.data.frame(mycoln)
dtv1 <- `colnames<-`(dtnn, mycoln$mycoln)
a <- "1、2、5、8、10、11、15、16、19、20、21、23、24、26、27、30、33、34、36、39"
a <- strsplit(a, "、")
aa <- as.data.frame(a)
relist <- rename(aa, no = 1)

relist$list <- str_c("Anxe", relist$no)
write_csv(dtv1, "dtv1.csv")
select(dtv1, relist$list)
f <- select(dtv1, relist$list)
f = 5- f
af <- select(dtv1, -relist$list)
dtv1_re <- bind_cols(af,f)
ztjl <- anxno_n[1:20]
tzjl <- anxno_n[21:40]
dtv1_re$ztjl <- rowSums(dtv1_re[,ztjl])
dtv1_re$tzjl <- rowSums(dtv1_re[,tzjl])


dtv1_re$maas <- rowSums(dtv1_re[,maas_n])
dtv1_re$phoneadd <- rowSums(dtv1_re[,phono_n])

my_dt <- select(dtv1_re, 1:9, tzjl, ztjl, maas, phoneadd)

write_csv(my_dt, "a03_alpha.csv")
