aoi_msg <- read_csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/msg.csv") %>%
  select(block_name, trial_name, type) %>%
  filter(block_name %in% c("b2", "b3")) %>%
  group_by(block_name, type) %>% summarise(stim_name = unique(trial_name))

ii <- aoi_msg %>% filter(block_name == "b2") %>%
  mutate(left = str_split_fixed(type, "-",2)[,1], right = str_split_fixed(type, "-",2)[,2])

i2 <- aoi_msg %>% filter(block_name == "b3") %>%
  mutate(one = str_split_fixed(type, "_",4)[,1], two = str_split_fixed(type, "_",4)[,2], three = str_split_fixed(type, "_",4)[,3], four = str_split_fixed(type, "_",4)[,4])

write_excel_csv(ii, "/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/b2_aoi_info.csv")
write_excel_csv(i2, "/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/b3_aoi_info.csv")
