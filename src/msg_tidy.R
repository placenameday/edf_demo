library(eyelinker)
require(dplyr)
require(tidyr)
require(ggplot2)
require(intervals)
require(stringr)
library(purrr)
require(readxl)

# define ascfile
ascfile <- "edf/s01/1004.asc"
# read asffile and extract msg info
A01 <- read_asc(ascfile, parse_all = TRUE)
A01_msg <- A01$msg
A01_msg <- mutate(A01_msg, block = floor(block))

# define function that get block zone
which_block <- function(keyword, msglist){
  temp <- msglist[which(str_detect(msglist$text, keyword)),]
  a <- min(temp$block)
  b <- max(temp$block)
  l <- list(min=a, max=b)
  return(l)
}

# initial a dataframe contains block info
columns= c("block_id","keywords","min","max")
block_id <- c("b1a", "b1b", "b2", "b3", "b4a", "b4b", "b5a", "b5b")
block_key <- c("b1a_pic", "b1b_pic", "b2_face$", "b3_pic", "b4a_text", "b4b_text", "b5a_scale", "scale_start")
block_info = data.frame(matrix(nrow = length(block_id), ncol = length(columns)))
colnames(block_info) = columns
block_info$block_id <- block_id
block_info$keywords <- block_key

# set block info in
t3 <- map(block_info$keywords, which_block, msglist=A01_msg)
block_info$min <- unlist(map(t3,1))
block_info$max <- unlist(map(t3,2))

block_info$type_p <- c("!V TRIAL_VAR b1a_type ", "!V TRIAL_VAR b1b_type ", "!V TRIAL_VAR b2_face_type ", NA,"!V TRIAL_VAR b4a_filename ", "!V TRIAL_VAR b4b_filename ", "!V TRIAL_VAR b5a_type ", "!V TRIAL_VAR b5b_type ")
block_info$file_p <- c("!V TRIAL_VAR b1a_filename ", "!V TRIAL_VAR b1b_filename ", "!V TRIAL_VAR b2_filename ", NA, "!V TRIAL_VAR b4a_filename ", "!V TRIAL_VAR b4b_filename ", "!V TRIAL_VAR b5a_filename ", "!V TRIAL_VAR b5b_filename ")
block_info$key_1_p <- c("V TRIAL_VAR b1a_key_1 ", "V TRIAL_VAR b1b_key_1 ", "!V TRIAL_VAR b2_key ", NA, NA, NA, "!V TRIAL_VAR b5a_key ", "!V TRIAL_VAR b5b_key ")
block_info$key_2_p <- c("V TRIAL_VAR b1a_key_2 ", "V TRIAL_VAR b1b_key_2 ", NA, NA, NA, NA, NA, NA)
block_info$key_3_p <- c("V TRIAL_VAR b1a_key_3 ", "V TRIAL_VAR b1b_key_3 ", NA, NA, NA, NA, NA, NA)
block_info$key_4_p <- c("V TRIAL_VAR b1a_key_4 ", "V TRIAL_VAR b1b_key_4 ", NA, NA, NA, NA, NA, NA)
block_info$acc_p <- c(NA, NA, "!V TRIAL_VAR b2_ACC ", NA, NA, NA, NA, NA)
block_info$RT_p <- c(NA, NA, "!V TRIAL_VAR b2_RT ", NA, "!V TRIAL_VAR b4a_read_rt 0", "!V TRIAL_VAR b4b_read_rt 0", "!V TRIAL_VAR b5a_RT ", "!V TRIAL_VAR b5b_RT ")

# get block name
block_j <- function(a, b){
  if (a == 0) {
    "b0"
  } else  
  b$block_id[which(b$min <= a & a <= b$max)]
}

A01_msg$block_name <- unlist(map(A01_msg$block, block_j, b=block_info))

# get trial id
trial_j <- function(a,b){
  if (a == "b0") {
    1
  } else  
    lm <- b$min[which(str_detect(b$block_id, a))]
}

minl <- unlist(map(A01_msg$block_name, trial_j, b=block_info))
A01_msg <- mutate(A01_msg, trial_id=block- minl + 1)

# define function1 get type info
get_type <- function(x,z,y=A01_msg){
  info_type <- filter(y, block_name==x)
  info_type$type <- str_split_fixed(info_type$text,z,2)[,2]
  info_type <- info_type[!(info_type$type==""), ]
  info_type
}

# define function2 that get info
getinfo <- function(bb,y){
  tsha <- map2(block_info$block_id, y, get_type)[-4]
  dd <- bind_rows(tsha)
  names(dd)[names(dd) =="type"] <-bb
  dd
}

# get b3 type info
b3_type_p <- c("!V TRIAL_VAR b3_n1 ", "!V TRIAL_VAR b3_n2 ", "!V TRIAL_VAR b3_n3 ", "!V TRIAL_VAR b3_n4 ")
ne_list <- read_excel("ne.xlsx")
nefil_list <- read_excel("para_data/nefil.xlsx")
neg_list <- read_excel("para_data/neg.xlsx")
pos_list <- read_excel("para_data/pos.xlsx")
thr_list <- read_excel("para_data/thr.xlsx")
ne_list$emo <- "ne"
nefil_list$emo <- "nefil"
neg_list$emo <- "neg"
pos_list$emo <- "pos"
thr_list$emo <- "thr"
all_emo <- list(ne_list, nefil_list, neg_list, pos_list, thr_list)
all_emo <- reduce(all_emo, bind_rows)

b3_get <- function(x){
  A01_msg_b3 <- filter(A01_msg, block_name=="b3")
  A01_msg_b3$type <- str_split_fixed(A01_msg_b3$text,x,2)[,2]
  qianmian <- str_split_fixed(A01_msg_b3$text, "!V TRIAL_VAR b3_",2)[,2]
  A01_msg_b3$posi <- str_split_fixed(qianmian, " ",2)[,1]
  A01_msg_b3 
}

b3_all <- b3_type_p %>% map(b3_get) %>%
  reduce(full_join) %>%
  filter(posi!="") %>%
  filter(type!="") %>%
  select(-2, -3) %>%
  pivot_wider(names_from = "posi", values_from = "type")

b3_match <- function(x){
  a <- all_emo[which(str_detect(all_emo$filename, x)),2]
  a
}

b3_all$n1_emo <- unlist(map(b3_all$n1, b3_match))
b3_all$n2_emo <- unlist(map(b3_all$n2, b3_match))
b3_all$n3_emo <- unlist(map(b3_all$n3, b3_match))
b3_all$n4_emo <- unlist(map(b3_all$n4, b3_match))
b3_all <- b3_all %>% mutate(type = paste(n1_emo, n2_emo, n3_emo, n4_emo, sep = "_")) %>%
  mutate(trial_name = paste(n1, n2, n3, n4, sep = "_")) %>%
  mutate(trial_name = paste(n1, n2, n3, n4, sep = "_")) %>%
  select(1,2,3,type,trial_name)

# get type info
type_info <- getinfo("type", block_info$type_p)
b4type <- filter(type_info, str_detect(type_info$block_name, ("b4a|b4b")))
b4type$type <- str_split_fixed(b4type$type, ".0", 2)[,1]
othertype <- filter(type_info, !str_detect(type_info$block_name, ("b4a|b4b")))
type_info <- bind_rows(othertype, b4type)
type_info <- full_join(type_info, b3_all)

# get trial info
trial_info <- getinfo("trial_name", block_info$file_p)
trial_info <- full_join(trial_info, b3_all)

# get keypress info
k1_info <- getinfo("k1", block_info$key_1_p)
k2_info <- getinfo("k2", block_info$key_2_p)
k3_info <- getinfo("k3", block_info$key_3_p)
k4_info <- getinfo("k4", block_info$key_4_p)

# get acc info
acc_info <- getinfo("acc", block_info$acc_p)

# get rt info
rt_info <- getinfo("rt", block_info$RT_p)

# merge all msg, b3 information id absence
all_msg <- select(trial_info, block, block_name, trial_id, trial_name) %>% left_join(select(type_info, block, block_name, type)) %>%
  left_join(select(k1_info, block, k1)) %>%
  left_join(select(k2_info, block, k2)) %>%
  left_join(select(k3_info, block, k3)) %>%
  left_join(select(k4_info, block, k4)) %>%
  left_join(select(acc_info, block, acc)) %>%
  left_join(select(rt_info, block, rt))

# initial data frame contain stims's time zone
time_info <- select(block_info, block_id, min, max)
time_info$stim1_start_p <- c("b1a_pic$", "b1b_pic$", "b2_face$", "b3_pic$", "b4a_text$", "b4b_text$", "b5a_scale$", "-(\\d)* (scale_start)$")
time_info$stim2_start_p <- c("b1a_q1$", "b1b_q1$", "b2_star$", NA, NA, NA, NA, NA)
time_info$stim3_start_p <- c("b1a_q2$", "b1b_q2$", NA, NA, NA, NA, NA, NA)
time_info$stim4_start_p <- c("b1a_q3$", "b1b_q3$", NA, NA, NA, NA, NA, NA)
time_info$stim5_start_p <- c("b1a_q4$", "b1b_q4$", NA, NA, NA, NA, NA, NA)
time_info$stim1_end_p <- c(NA, NA, NA, NA, "b4a_keyboard$", "b4b_keyboard$", "b5a_keyboard$", "^0 scale_start$")
time_info$stim2_end_p <- c("b1a_keyboard_1$", "b1b_keyboard_1$", "b2_keyboard$", NA, NA, NA, NA, NA)
time_info$stim3_end_p <- c("b1a_keyboard_2$", "b1b_keyboard_2$", NA, NA, NA, NA, NA, NA)
time_info$stim4_end_p <- c("b1a_keyboard_3$", "b1b_keyboard_3$", NA, NA, NA, NA, NA, NA)
time_info$stim5_end_p <- c("b1a_keyboard_4$", "b1b_keyboard_4$", NA, NA, NA, NA, NA, NA)

# define function1 get type time
get_time <- function(z,y=A01_msg){
  time <- y[which(str_detect(y$text,z)),c(1,2,4,5)]
}

# define function2 that get time
get_time2 <- function(y, z){
  time <- map(y, get_time)
  time <- bind_rows(time)
  names(time)[names(time) =="time"] <- z
  time
}

stim_1_start <- get_time2(time_info$stim1_start_p, "stim_1_start")
stim_2_start <- get_time2(time_info$stim2_start_p, "stim_2_start")
stim_3_start <- get_time2(time_info$stim3_start_p, "stim_3_start")
stim_4_start <- get_time2(time_info$stim4_start_p, "stim_4_start")
stim_5_start <- get_time2(time_info$stim5_start_p, "stim_5_start")

stim_1_end <- get_time2(time_info$stim1_end_p, "stim_1_end")
stim_2_end <- get_time2(time_info$stim2_end_p, "stim_2_end")
stim_3_end <- get_time2(time_info$stim3_end_p, "stim_3_end")
stim_4_end <- get_time2(time_info$stim4_end_p, "stim_4_end")
stim_5_end <- get_time2(time_info$stim5_end_p, "stim_5_end")

time_list <- list(stim_1_start, stim_2_start, stim_3_start, stim_4_start, stim_5_start, stim_1_end, stim_2_end, stim_3_end, stim_4_end, stim_5_end)

all_msg <- time_list %>% reduce(full_join) %>%
  full_join(all_msg) %>%
  arrange(block) %>%
  select(block, block_name, trial_id, trial_name, type, k1, k2, k3, k4, acc, rt, stim_1_start, stim_1_end, stim_2_start, stim_2_end, stim_3_start, stim_3_end, stim_4_start, stim_4_end, stim_5_start, stim_5_end)
