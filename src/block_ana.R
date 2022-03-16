# read the tidy event raw data
fix_all <- read_csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/fix.csv")
sacc_all <- read_csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/sacc.csv")
blinks_all <- read_csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/blinks.csv")

get_b1 <- function(x,y,z){
  x <- group_by(x, participant_phone, participant_group, block_name, stim) %>%
    filter(block_name %in% y, stim %in% z)
  x
  # fix b1b stim 1.5 to 1
  x$stim <- map2_dbl(x$block_name, x$stim, ~ if_else(.x=="b1b"&.y==1.5, 1, .y))
  x
}

merge_event <- function(block, stim, x=fix_all,y=sacc_all,z=blinks_all){
  fix_sum <- summarise(get_b1(x, block, stim), fix_count = sum(!is.na(dur)), fix_duration = mean(dur, na.rm = T), pupil = mean(aps, na.rm = T))
  
  sacc_summraise <- summarise(get_b1(y, block, stim), sacc_count = sum(!is.na(ampl)), sacc_duration = mean(dur,na.rm = T),  sacc_ampl = mean(ampl,na.rm = T), sacc_pv = mean(pv,na.rm = T))
  
  blinks_summraise <- summarise(get_b1(z, block, stim), blinks_count = sum(!is.na(dur)), blinks_duration = mean(dur, na.rm = T))
  
  eye_event <- reduce(list(fix_sum, sacc_summraise, blinks_summraise), full_join)
}

 
b1_eye_event <- merge_event(c("b1a","b1b"), c(1,1.5)) %>% left_join(read_csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/scale_sum.csv"))
b2_eye_event <- merge_event(c("b2"), c(1)) %>% left_join(read_csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/scale_sum.csv"))
b3_eye_event <- merge_event(c("b3"), c(1)) %>% left_join(read_csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/scale_sum.csv"))
b4_eye_event <- merge_event(c("b4a","b4b"), c(1)) %>% left_join(read_csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/scale_sum.csv"))
b5_eye_event <- merge_event(c("b5a","b5b"), c(1)) %>% left_join(read_csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/scale_sum.csv"))

write_excel_csv(b1_eye_event, "/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/block_analy/b1_eye_event")
write_excel_csv(b2_eye_event, "/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/block_analy/b2_eye_event")
write_excel_csv(b3_eye_event, "/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/block_analy/b3_eye_event")
write_excel_csv(b4_eye_event, "/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/block_analy/b4_eye_event")
write_excel_csv(b5_eye_event, "/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/block_analy/b5_eye_event")
