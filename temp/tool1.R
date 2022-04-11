ch4 <- read_xlsx("/Users/placenameday/Downloads/check_list_4.xlsx")
all <- read_csv("/Users/placenameday/R study/edf_demo/data/processed/sclae_cred/processed/all_3.csv")
check <- read_xlsx("/Users/placenameday/R study/edf_demo/para/para_data/check_list.xlsx")

anti_join(ch4, all)

 unique(all$participant_phone)
 
all2 <- all %>% distinct(participant_phone, .keep_all =TRUE)
write_excel_csv(all2, "/Users/placenameday/R study/edf_demo/data/processed/sclae_cred/processed/all_3_unique.csv")

check %>% distinct(phone, .keep_all =TRUE)
