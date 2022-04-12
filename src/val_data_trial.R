# 定义函数 计算有效眼动数据比例 基于每一个trial 每一个刺激
vali_eye_t <- function(rawd){
  dt <- fread(rawd) %>%
    mutate(wuxiao=if_else((xp==0&yp==0)|(is.na(xp)&is.na(yp)), 1,0), youxiao=if_else((xp==0&yp==0)|(is.na(xp)&is.na(yp)), 0,1)) %>%
    group_by(participant_group, participant_phone, block, stim) %>%
    summarise(vali=sum(wuxiao)/(sum(youxiao)+sum(wuxiao)))
}

ve_c <- function(dic){
  file_list <- paste(dic,"/",list.files(dic), sep = "")
  ilist <- map(file_list, vali_eye_t)
  bind_rows(ilist)
}

track_loss <- ve_c("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/raw") %>%
  filter(vali > 0.25)

write_excel_csv(track_loss, "/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/track_loss_trial.csv")
