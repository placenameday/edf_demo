# read the tidy event raw data
fix_all <- read_csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/fix.csv")
sacc_all <- read_csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/sacc.csv")
blinks_all <- read_csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/blinks.csv")

# group data and summaries it
fix_all <- group_by(fix_all, participant_phone, participant_group, block_name, type, stim)
sacc_all <- group_by(sacc_all, participant_phone, participant_group, block_name, type, stim)
blinks_all <- group_by(blinks_all, participant_phone, participant_group, block_name, type, stim)

fix_summraise <- summarise(fix_all, fix_count = sum(!is.na(dur)), fix_duration = mean(dur, na.rm = T), pupil = mean(aps, na.rm = T))
sacc_summraise <- summarise(sacc_all, sacc_count = sum(!is.na(ampl)), sacc_duration = mean(dur,na.rm = T),  sacc_ampl = mean(ampl,na.rm = T), sacc_pv = mean(pv,na.rm = T))
blinks_summraise <- summarise(blinks_all, blinks_count = sum(!is.na(dur)), blinks_duration = mean(dur, na.rm = T))
eye_event <-list(fix_summraise, sacc_summraise, blinks_summraise)

eye_event2 <- reduce(eye_event, full_join)

write.csv(eye_event2,"/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/eye_event.csv")