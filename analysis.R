# load all require src and package
source("require.R")

# get rawinfo from asc file

# set asc file dic
ac <- "data/raw/eye_asc/"
# set output file dic
bc <- "data/processed/eye_tacking/tidy/"

# set solo file list
ren <- c("2013", "2014")
# set list file
li <- read_excel("para/para_data/asc_list.xlsx")
rr <- as.character(li$list)

# set multisession core
plan(multisession, workers = 4)


# start
map(ren, stamp_asc, ascdic=ac, outputdic=bc)
future_map(ren, stamp_asc_fu, ascdic=ac, outputdic=bc)

map(ren, ~ try(stamp_asc_fu(.x)))
map(ren, ~ try(stamp_asc(.x)))

map(rr, ~ try(stamp_asc_fu(.x)))