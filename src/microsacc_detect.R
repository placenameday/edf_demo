source("src/vecvel.R")
source("src/microsacc.R")

SAMPLING = 500
MINDUR = 3
VFAC = 5

library(data.table)

compute_ms <- function(data){
  # data import
  dt <- as.matrix(data)
  # compute microsacc
  ms <- microsacc(dt,VFAC,MINDUR,SAMPLING)
}

dt <- fread("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/raw_clean/1003_raw.zip") %>%
  filter(stim==1, block_name=="b1a", trial_id==1, !is.na(xp), !is.na(yp)) %>% mutate(xp=xp/32, yp=yp/32) %>%
  select(xp, yp)


k <- compute_ms(dt)
f <- as.data.frame(k[[1]]) %>% select(1:3) %>%
  mutate(participant_phone=1, participant_group="a",block_name="b1a", stim=1, trial_id=1) %>%
  rename(mxp=V1, myp=V2, mpv=V3)
f

