library(eyelinker)
require(dplyr)
require(tidyr)
require(ggplot2)
require(intervals)
require(stringr)
library(purrr)

# define ascfile
ascfile <- "edf/s01/A01.asc"
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
block_info$type_p <- c("!V TRIAL_VAR b1a_type ", "!V TRIAL_VAR b1b_type ", "!V TRIAL_VAR b2_face_type ", "","!V TRIAL_VAR b4a_filename ", "!V TRIAL_VAR b4b_filename ", "!V TRIAL_VAR b5a_type ", "!V TRIAL_VAR b5b_type ")
block_info$file_p <- c("!V TRIAL_VAR b1a_filename ", "!V TRIAL_VAR b1b_filename ", "!V TRIAL_VAR b2_filename ", "", "!V TRIAL_VAR b4a_filename ", "!V TRIAL_VAR b4b_filename ", "!V TRIAL_VAR b5a_filename ", "!V TRIAL_VAR b5b_filename ")


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

# get type info
get_type <- function(x,z,y=A01_msg){
  info_type <- filter(y, block_name==x)
  info_type$type <- str_split_fixed(info_type$text,z,2)[,2]
  info_type <- info_type[!(info_type$type==""), ]
  info_type
}

sha <- map2(block_info$block_id, block_info$type_p, get_type)[-4]
type_info <- bind_rows(sha)

b4type <- filter(type_info, str_detect(type_info$block_name, ("b4a|b4b")))
b4type$type <- str_split_fixed(b4type$type, ".0", 2)[,1]
othertype <- filter(type_info, !str_detect(type_info$block_name, ("b4a|b4b")))
type_info <- bind_rows(othertype, b4type)

# get trial name info
tsha <- map2(block_info$block_id, block_info$file_p, get_type)[-4]
trial_info <- bind_rows(tsha)
trial_info <- rename(trial_info, trial_name = type)

# merge all msg
A01_msg_f <- left_join(select(trial_info, block, block_name, trial_id, trial_name), select(type_info, block, block_name, type))

# 该解决b3 type 抓取的问题了