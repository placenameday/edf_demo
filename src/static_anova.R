dta <- read_csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/eye_event_overall/inner_dt_stim.csv")

dtb <- dta %>% mutate(stim=if_else(block_name=="b1b"&stim==1.5,1,stim)) %>%
  filter(block_name!="b2") %>% select(-AOI)

anv <- function(metri, stname, bname, dt3){
  mm <- metri
  b1a <- dt3 %>% filter(block_name==bname, stim==stname) %>%
    group_by(maas_g, tgjl_g, participant_phone) %>%
    summarise(fc=mean(!!sym(metri), na.rm=T)) %>%
    mutate(maas = factor(maas_g, levels=c("high", "low")),
           tgjl = factor(tgjl_g, levels=c("high", "low")))

  aov.manu <- try(aov(fc ~ maas + tgjl + maas:tgjl, data=b1a))
  summary(aov.manu)[[1]] %>% mutate(block_name=bname, stim=stname, metric=mm, effect=rownames(.)) %>%
    rename(p = "Pr(>F)")
}

# anv("fc","1", "b1a", dtb)

dnv3 <- function(sti, bname, dtt){
  s <- sti
  ndt <- dtt %>% filter(stim==sti)
  mlist <- metrilist
  bind_rows(future_map(metrilist, anv, stname=s, bname=bname, dt3=ndt))
}

anv2 <- function(blockname, dt2){
  bn <- blockname
  dt2 <- dt2 %>% filter(block_name==bn)
  stimlist <- unique(dt2$stim)
  bind_rows(future_map(stimlist, dnv3, bname=bn, dtt=dt2))
}


mu <- function(dt1){
  blist <- unique(dt1$block_name)
  bind_rows(future_map(blist, anv2, dt2=dt1))
}

kankan <- mu(dtb) %>% filter(p <= 0.05)

write_excel_csv(kankan, "/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/ea/anova/over_all.csv")


metrilist <- colnames(dtb)[7:19]


ggplot(data=b1a, mapping = aes(
  x = maas_g, y = fc)) +
  geom_boxplot()