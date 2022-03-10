
# use eyelinker read asc file and inject group and phone info in it.
read <- function(file){
  require(eyelinker)
  require(readxl)
  require(stringr)
  dt <- read_asc(file, parse_all = T)
  check_list <- read_excel("para/para_data/check_list.xlsx")
  participant_id <- str_extract(file, "\\d\\d\\d\\d")
  dt$participant_group <- check_list[which(str_detect(check_list$`participant id`, participant_id)), 4]
  dt$participant_phone <- check_list[which(str_detect(check_list$`participant id`, participant_id)), 3]
  dt
}
