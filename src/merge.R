# make function to merge data into one file
mergedt <- function(datatype){
  dic <- "data/processed/eye_tacking/tidy"
  file_list <- paste(dic,"/",datatype,"/",read_excel("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/list.xlsx")$filename, "_", datatype,".csv", sep = "")
  merge_dt <- file_list %>% map(read_csv, col_type = list(k1 = "c", k2 = "c",k3 = "c",k4 = "c")) %>%
    bind_rows()
}

# make function to merge data into one file and save it to csv file
mergedt_s <- function(datatype){
  dic <- "data/processed/eye_tacking/tidy"
  dic_a <- paste(dic,"/", datatype, sep = "")
  file_list <- paste(dic_a,"/",list.files(dic_a), sep = "")
  merge_dt <- file_list %>% map(read_csv, col_type = list(k1 = "c", k2 = "c",k3 = "c",k4 = "c")) %>%
    bind_rows()
  write_csv(merge_dt, paste(dic,"/", datatype, ".csv", sep = ""))
}

merlist <- c("msg", "raw", "blinks", "sacc", "fix")
merlist2 <- c("raw_clean", "blinks_clean", "sacc_clean", "fix_clean")
merlist3 <- c("sacc_clean_re")

map(merlist3, mergedt_s)