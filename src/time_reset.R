
# 读取msg文件，保留时间戳信息

time_msg <- function(msgfile){
  tt <- pivot_longer(msgfile, cols=c(12,14,16,18,20,22), names_to = "stim", values_to = "start_time", values_drop_na = FALSE) %>% mutate(stim=str_replace(stim, "_start", "")) %>%
    mutate(stim=str_replace(stim, "stim_", ""))
  
  td <- pivot_longer(msgfile, cols=c(13,15,17,19,21,23), names_to = "stim", values_to = "end_time", values_drop_na = FALSE) %>% mutate(stim=str_replace(stim, "_end", "")) %>%
    mutate(stim=str_replace(stim, "stim_", ""))
  
  a <- full_join(tt,td) %>%
    select(participant_group, participant_phone, block, block_name, trial_id, trial_name, type, stim, start_time, end_time) %>% mutate(stim=str_replace(stim, "1_2", "1.5")) %>%
    mutate(stim=as.numeric(stim))
  a
}

# time msg file
tm <- time_msg(read_csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/msg.csv"))

# raw eye_tracking file
rfile <- fread("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/raw/1001_raw.csv") %>%
  mutate(participant_phone=as.numeric(participant_phone))

k <- tm %>% filter(participant_phone==rfile$participant_phone[[1]]) %>%
  right_join(rfile) %>%
  mutate(time = time - start_time)


