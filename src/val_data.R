# 读取数据核对表格信息
dt <- read_csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/数据核对表.csv")
check <- read_excel("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/check_list.xlsx") %>% rename(participant_phone=phone) %>% mutate_if(is.character, as.numeric)

# 列出眼动数据缺失被试
queshi <- anti_join(check,dt)

# 列出重复实验被试
chongfu <- dt[which(duplicated(dt$participant_phone)),]

# 定义函数 计算有效眼动数据比例
vali_eye <- function(rawd){
  dt <- fread(rawd) %>%
    mutate(wuxiao=if_else((xp==0&yp==0)|(is.na(xp)&is.na(yp)), 1,0), youxiao=if_else((xp==0&yp==0)|(is.na(xp)&is.na(yp)), 0,1)) %>%
    group_by(participant_group, participant_phone, block_name) %>%
    summarise(vali=sum(youxiao)/(sum(youxiao)+sum(wuxiao)))
}

ve_c <- function(dic){
  file_list <- paste(dic,"/",list.files(dic), sep = "")
  ilist <- map(file_list, vali_eye)
  bind_rows(ilist)
}
