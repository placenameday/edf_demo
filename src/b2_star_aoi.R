
b2_msg_r <- read_csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/msg_r.csv") %>%
  filter(block_name == "b2") %>% select(trial_name, ans) %>%
  group_by(trial_name) %>% unique.data.frame()

