
panduan <- function(t,stim_list){
  if (!any(which(t > stim_list$start & t <stim_list$end))) {
    0
  } else{
    stim_list[which(t > stim_list$start & t <stim_list$end),3]
  }
}

stamp_raw <- function(msg, raw){
  raw_2 <- list()
  
  for (i in msg$block) {
    msg_l <- filter(msg, block == i)
    msg_ll <- select(msg_l, 12:23)
    stim_list <- data.frame(start = c(unlist(msg_ll[1,c(1,3,5,7,9,11)])), end = c(unlist(msg_ll[1,c(2,4,6,8,10,12)])), stim = c(1,2,3,4,5,1.5))
    row.names(stim_list) <- NULL
    
    raw_l <- filter(raw, block == i)
    raw_l$stim <- unlist(map(raw_l$time,panduan, stim_list=stim_list))
    raw_2 <- bind_rows(raw_2, select(raw_l, time, stim))
  }
  
  raw_3 <- raw %>% full_join(raw_2) %>%
  full_join(select(msg, c(1,2,3,4,5,24,25)))
}

stamp_raw_fu <- function(msg, raw){
  eb <- function(i) {
    msg_l <- filter(msg, block == i)
    msg_ll <- select(msg_l, 12:23)
    stim_list <- data.frame(start = c(unlist(msg_ll[1,c(1,3,5,7,9,11)])), end = c(unlist(msg_ll[1,c(2,4,6,8,10,12)])), stim = c(1,2,3,4,5,1.5))
    row.names(stim_list) <- NULL
    
    raw_l <- filter(raw, block == i)
    raw_l$stim <- unlist(future_map(raw_l$time,panduan, stim_list=stim_list))
    raw_2 <- select(raw_l, time, stim)
  }
  
  raw_2 <- bind_rows(future_map(msg$block, eb))
  
  raw_3 <- raw %>% full_join(raw_2) %>%
    full_join(select(msg, c(1,2,3,4,5,24,25)))
}


panduan2 <- function(t1, t2, stim_list){
  if (!any(which(t1 > stim_list$start & t2 <stim_list$end))) {
    0
  } else{
    stim_list[which(t1 > stim_list$start & t2 <stim_list$end),3]
  }
}

stamp2 <- function(msg, tar){
  tar_2 <- list()
  
  for (i in msg$block) {
    msg_l <- filter(msg, block == i)
    msg_ll <- select(msg_l, 12:23)
    stim_list <- data.frame(start = c(unlist(msg_ll[1,c(1,3,5,7,9,11)])), end = c(unlist(msg_ll[1,c(2,4,6,8,10,12)])), stim = c(1,2,3,4,5,1.5))
    row.names(stim_list) <- NULL
    
    tar_l <- filter(tar, block == i)
    if (nrow(tar_l)!=0) {
      tar_l$stim <- unlist(map2(tar_l$stime,tar_l$etime,panduan2,stim_list=stim_list))
      tar_2 <- bind_rows(tar_2, select(tar_l, stime, etime, stim))
    }
  }
  
  tar_3 <- tar %>% full_join(tar_2) %>%
    full_join(select(msg, c(1,2,3,4,5,24,25)))
}

stamp2_fu <- function(msg, tar){
  tar_2 <- list()
  eb2 <- function(i) {
    msg_l <- filter(msg, block == i)
    msg_ll <- select(msg_l, 12:23)
    stim_list <- data.frame(start = c(unlist(msg_ll[1,c(1,3,5,7,9,11)])), end = c(unlist(msg_ll[1,c(2,4,6,8,10,12)])), stim = c(1,2,3,4,5,1.5))
    row.names(stim_list) <- NULL
    
    tar_l <- filter(tar, block == i)
    if (nrow(tar_l)!=0) {
      tar_l$stim <- unlist(future_map2(tar_l$stime,tar_l$etime,panduan2,stim_list=stim_list))
      tar_2 <- bind_rows(tar_2, select(tar_l, stime, etime, stim))
    }
  }
  
  tar_2 <- bind_rows(future_map(msg$block, eb2))
  
  tar_3 <- tar %>% full_join(tar_2) %>%
    full_join(select(msg, c(1,2,3,4,5,24,25)))
}
