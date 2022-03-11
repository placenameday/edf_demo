
stamp <- function(data){
  fixinfo <- data$fix
  saccinfo <- data$sacc
  blinksinfo <- data$blinks
  msg <- data$msg
  
  judge_time <- function(block, st, et){
    stim <- msg[which(msg$block==block),]
    stim <- select(stim, -c("acc", "rt", "k1", "k2", "k3", "k4"))
    stim[is.na(stim)] <- 0
    if ((dplyr::between(st, stim$stim_1_start, stim$stim_1_end)) & (dplyr::between(et, stim$stim_1_start, stim$stim_1_end))) {
      return(1)
    } else if ((dplyr::between(st, stim$stim_2_start, stim$stim_2_end)) & (dplyr::between(et, stim$stim_2_start, stim$stim_2_end))) {
      return(2)
    } else if ((dplyr::between(st, stim$stim_3_start, stim$stim_3_end)) & (dplyr::between(et, stim$stim_3_start, stim$stim_3_end))) {
      return(3)
    } else if ((dplyr::between(st, stim$stim_4_start, stim$stim_4_end)) & (dplyr::between(et, stim$stim_4_start, stim$stim_4_end))) {
      return(4)
    } else if ((dplyr::between(st, stim$stim_5_start, stim$stim_5_end)) & (dplyr::between(et, stim$stim_5_start, stim$stim_5_end))) {
      return(5)
    } else if ((dplyr::between(st, stim$stim_1_2_start, stim$stim_1_2_end)) & (dplyr::between(et, stim$stim_1_2_start, stim$stim_1_2_end))) {
      return(1.5)
    } else {
      return(0)
    }
  }
  
  fixinfo$stim_info <- unlist(pmap(list(fixinfo$block, fixinfo$stime, fixinfo$etime), judge_time))
  saccinfo$stim_info <- unlist(pmap(list(saccinfo$block, saccinfo$stime, saccinfo$etime), judge_time))
  blinksinfo$stim_info <- unlist(pmap(list(blinksinfo$block, blinksinfo$stime, blinksinfo$etime), judge_time))
  
  data$fix <- full_join(select(data$msg, c(1,2,3,4,5,24,25)), fixinfo)
  data$sacc <- full_join(select(data$msg, c(1,2,3,4,5,24,25)), saccinfo)
  data$blinks <- full_join(select(data$msg, c(1,2,3,4,5,24,25)), blinksinfo)
  data
}


