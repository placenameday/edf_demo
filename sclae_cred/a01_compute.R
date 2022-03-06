dt <- read_csv("a01.csv", locale=locale(encoding = 'GB18030'))
dtn <- select(dt, 作答ID, 作答时长, 20:119)
dtnn <- select(dtn, -Q2_1, -Q3_1, -Q4_1, -Q5_1)



cnma <- c("ID", "time", "reward", "next", "phone", "sex", "age", "edu", "statu")
anxno <- (1:40)
mindno <- (1:39)
phono <- (1:10)
anxno_n <- paste("Anxe", anxno, sep = "")
mindno_n <- paste("Mindful", mindno, sep = "")
phono_n <- paste("Phoneadd", phono, sep="")
mycoln <- c(cnma, anxno_n, mindno_n, phono_n)
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

ober <- c("1,6,11,15,20,26,31,36")
mdesc <- c("2,7,12,16,22,27,32,37")
actaw <- c("5,8,13,18,23,28,34,38")
nojug <- c("3,10,14,17,25,30,35,39")
norea <- c("4,9,19,21,24,29,33")

my_listname <- function(x, y="") {
  x <- as.data.frame(strsplit(x, ","))
  x <- rename(x, x = 1)
  x <- mutate(x, name = paste(y, x, sep = ""))
  x
}

oberlist <- my_listname(ober, "Mindful")
mdesclist <- my_listname(mdesc, "Mindful")
actawlist <- my_listname(actaw, "Mindful")
nojuglist <- my_listname(nojug, "Mindful")
norealist <- my_listname(norea, "Mindful")

dtv1_re$Mindfulober <- rowSums(dtv1_re[,oberlist$name])
dtv1_re$Mindfulmdesc <- rowSums(dtv1_re[,mdesclist$name])
dtv1_re$Mindfulactaw <- rowSums(dtv1_re[,actawlist$name])
dtv1_re$Mindfulnojug <- rowSums(dtv1_re[,nojuglist$name])
dtv1_re$Mindfulnorea <- rowSums(dtv1_re[,norealist$name])

dtv1_re$phoneadd <- rowSums(dtv1_re[,phono_n])

my_dt <- select(dtv1_re, 1:9, tzjl, ztjl, Mindfulober, Mindfulmdesc, Mindfulactaw, Mindfulnojug, Mindfulnorea, phoneadd)

write_csv(my_dt, "my_dt.csv")