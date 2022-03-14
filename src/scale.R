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

scale_na_b1 <- function(x,y,z){
  if(!(x %in% c("1","2","3","4","5","6"))){
    if (y %in% c("b1a","b1b")) {
      1.5
    } else if(is.null(x)){
      NA
    } else x
  } else x
}

msg1 <- msg
msg1$k1 <- unlist(pmap(list(msg$k1,msg$block_name,msg$type), scale_na))
msg1$k2 <- unlist(pmap(list(msg$k2,msg$block_name,msg$type), scale_na_b1))
msg1$k3 <- unlist(pmap(list(msg$k3,msg$block_name,msg$type), scale_na_b1))
msg1$k4 <- unlist(pmap(list(msg$k4,msg$block_name,msg$type), scale_na_b1))

scale_msg <- filter(msg1, block_name %in% c("b1a", "b1b", "b5a", "b5b"))
scale_msg1 <- mutate(scale_msg, k1=as.numeric(k1),k2=as.numeric(k2),k3=as.numeric(k3),k4=as.numeric(k4))

