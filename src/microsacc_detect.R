source("src/vecvel.R")
source("src/microsacc.R")

SAMPLING = 500
MINDUR = 3
VFAC = 5



compute_ms <- function(data){
  # data import
  dt <- as.matrix(data)
  # compute microsacc
  ms <- microsacc(dt,VFAC,MINDUR,SAMPLING)
}

dt <- fread("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/raw_clean/1004_raw.csv") %>%
  filter(stim==1, block_name=="b1a", trial_id==1, !is.na(xp), !is.na(yp)) %>% mutate(xp=xp/32, yp=yp/32) %>%
  select(xp, yp)

dt2 <- fread("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/raw_clean/1022_raw.csv") %>%
  filter(stim%in%c(1,2), block_name=="b2", trial_id==18, !is.na(xp), !is.na(yp)) %>% mutate(xp=xp/32, yp=yp/32) %>%
  select(xp, yp)
k2 <- compute_ms(dt2)
k2[[1]]

k <- compute_ms(dt)



