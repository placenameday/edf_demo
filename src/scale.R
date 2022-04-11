# get id of reverse_score
tgjl_re_id <- "21、23、24、26、27、30、33、34、36、39"
tgjl_re_id <- as.numeric(strsplit(tgjl_re_id, "、")[[1]])
tgjl_re_id <- tgjl_re_id-20

# make a function to due NA in scale
scale_na <- function(x,y,z){
  if(!(x %in% c("1","2","3","4","5","6"))){
    if (y %in% c("b1a","b1b")) {
      1.5
    } else if(z %in% c("maas", "sas")){
      3.5
    } else if(z == "tgjl"){
      2.5
    } else if(is.null(x)){
      NA
    } else x
  } else x
}

scale_na_b1 <- function(x,y){
  if(!(x %in% c("1","2","3","4","5","6"))){
    if (y %in% c("b1a","b1b")) {
      1.5
    } else if(is.null(x)){
      NA
    } else x
  } else x
}

scale_rever <- function(x,y,z){
  if(x == "tgjl"){
    if (y %in% tgjl_re_id) {
      5 - z
    } else z
  } else z
}

msg <- read_csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/msg.csv")

msg1 <- msg
msg1$k1 <- unlist(pmap(list(msg$k1,msg$block_name,msg$type), scale_na))
msg1$k2 <- unlist(pmap(list(msg$k2,msg$block_name), scale_na_b1))
msg1$k3 <- unlist(pmap(list(msg$k3,msg$block_name), scale_na_b1))
msg1$k4 <- unlist(pmap(list(msg$k4,msg$block_name), scale_na_b1))

scale_msg <- filter(msg1, block_name %in% c("b1a", "b1b", "b5a", "b5b"))
scale_msg1 <- mutate(scale_msg, k1=as.numeric(k1),k2=as.numeric(k2),k3=as.numeric(k3),k4=as.numeric(k4))

msg2 <- scale_msg1
msg2$k1 <- unlist(pmap(list(scale_msg1$type,scale_msg1$trial_id,scale_msg1$k1), scale_rever))

msg2 <- group_by(msg2, participant_group, participant_phone, type)
scale_sum <- summarise(msg2, scale_sum=sum(k1))
scale_sum_b5 <- filter(scale_sum, type %in% c("tgjl", "maas", "sas"))
scale_sum_b5 <- pivot_wider(scale_sum_b5, names_from = type, values_from = scale_sum)
scale_sum_b5 <- rename(scale_sum_b5, tgjl_e = tgjl, maas_e = maas, sas_e = sas)

cred <- read_csv("/Users/placenameday/R study/edf_demo/data/processed/sclae_cred/processed/all_4.csv") %>%
  select(-(1:4), -statu, -ztjl) %>%
  rename(tgjl = tzjl, sas=phoneadd)

kk <- left_join(scale_sum_b5, cred)

write_excel_csv(kk, "/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/scale_sum.csv")
