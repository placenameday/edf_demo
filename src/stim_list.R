msg <- read_csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/msg.csv")

stim <- msg %>%
  select(block_name, type, trial_name) %>%
  group_by(block_name, type) %>%
  summarise(stim_name = unique(trial_name))

write_excel_csv(stim, "/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/stim_info.csv")
