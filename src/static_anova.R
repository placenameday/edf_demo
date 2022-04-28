dta <- read_csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/tidy/eye_event_overall/inner_dt_stim.csv")


cl <- readxl::read_xlsx("/Users/placenameday/R study/edf_demo/para/para_data/check_list.xlsx") %>%
  filter(eb==4)

b4phone <- cl$phone


dtb <- dta %>% mutate(stim=if_else(block_name=="b1b"&stim==1.5,1,stim), block_name=if_else(block_name=="b4a"&participant_phone %in% b4phone,"nb4b",if_else(block_name=="b4b"&participant_phone %in% b4phone,"nb4a",block_name))) %>% mutate(block_name=if_else(block_name=="nb4b", "b4b", if_else(block_name=="nb4a", "b4a", block_name))) %>%
  select(-AOI)

anv <- function(metri, stname, bname, dt3){
  mm <- metri
  b1a <- dt3 %>% filter(block_name==bname, stim==stname) %>%
    group_by(maas_g, tgjl_g, sas_g, participant_phone) %>%
    summarise(fc=mean(!!sym(metri), na.rm=T), maas=mean(maas), tgjl=mean(tgjl), sas=mean(sas)) %>%
    mutate(maasg = factor(maas_g, levels=c("high", "mid", "low")),
           tgjlg = factor(tgjl_g, levels=c("high", "mid", "low")),
           sasg = factor(sas_g, levels=c("high", "mid", "low")))
  
  ztjy <- round(shapiro.test(b1a$fc)$p.value,4)
  
  bbc <- b1a %>% pivot_longer(c(maasg,tgjlg,sasg), names_to = "sclae", values_to = "group") %>% mutate(zg=paste(sclae,group,sep = "_"))
  
  qxjy <- round(ifelse(ztjy<0.05, leveneTest(bbc$fc,as.factor(bbc$zg))$`Pr(>F)`[1], bartlett.test(bbc$fc,bbc$zg)$p.value),4)

  aov.manu <- try(aov(fc ~ maasg + tgjlg + sasg + maasg:tgjlg:sasg + maasg:tgjlg + tgjlg:sasg + maasg:sasg, data=b1a))
  summary(aov.manu)[[1]] %>% mutate(block_name=bname, stim=stname, metric=mm, effect=rownames(.), ztx=ztjy, qx=qxjy) %>%
    rename(p = "Pr(>F)") %>% mutate(p=round(p,4), Resdf=summary(aov.manu)[[1]]["Residuals",1])
}

# anv("fc","1", "b1a", dtb)

alm <- function(metri, stname, bname, dt3){
  mm <- metri
  dtc <- dt3 %>% filter(block_name==bname, stim==stname) %>%
    group_by(maas_g, tgjl_g, sas_g, participant_phone) %>%
    summarise(fc=mean(!!sym(metri), na.rm=T), maas=mean(maas), tgjl=mean(tgjl), sas=mean(sas))
  
  ztjy <- round(shapiro.test(dtc$fc)$p.value,4)
  
  
  ldtc <- lm(fc~ maas + tgjl + sas, data = dtc)
  
  cc<-summary(ldtc$residuals)
  Q1<-cc[[2]]
  Q2<-cc[[3]]
  Q3<-cc[[5]]
  skewness<-((Q1-Q2)+(Q3-Q2))/(Q3-Q1)

  
  sl <- as.data.frame(summary(ldtc)["coefficients"][[1]]) %>% mutate(block_name=bname, stim=stname, metric=mm, effect=rownames(.), ztx=ztjy, df=summary(ldtc)[["df"]][2], ccfb=skewness) %>%
    rename(p = "Pr(>|t|)") %>% mutate(p=round(p,4)) %>% filter(effect!="(Intercept)")
}


dnv3 <- function(sti, bname, dtt, m){
  s <- sti
  ndt <- dtt %>% filter(stim==sti)
  mlist <- metrilist
  method <- ifelse(m=="lm",alm,anv)
  bind_rows(future_map(metrilist, method, stname=s, bname=bname, dt3=ndt))
}

anv2 <- function(blockname, dt2, m){
  bn <- blockname
  dt2 <- dt2 %>% filter(block_name==bn)
  stimlist <- unique(dt2$stim)
  
  bind_rows(future_map(stimlist, dnv3, bname=bn, dtt=dt2, m=m))
}


mu <- function(dt1,m){
  blist <- unique(dt1$block_name)
  bind_rows(future_map(blist, anv2, dt2=dt1, m=m))
}

lmall <- mu(dtb, "lm") %>% filter(p <= 0.05)
anovamall <- mu(dtb, "a") %>% filter(p <= 0.05)

write_excel_csv(kankan, "/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/ea/anova/over_all.csv")


metrilist <- colnames(dtb)[7:19]


bba <- dtb %>% filter(block_name=="b1a", stim==1) %>%
  group_by(maas_g, tgjl_g, sas_g, participant_phone) %>%
  summarise(fc=mean(fc, na.rm=T)) %>%
  mutate(maas = factor(maas_g, levels=c("high", "mid", "low")),
         tgjl = factor(tgjl_g, levels=c("high", "mid", "low")),
         sas = factor(sas_g, levels=c("high", "mid", "low")))

tt <- bba$fc

bbc <- bba %>% pivot_longer(c(maas, tgjl, sas), names_to = "sclae", values_to = "group") %>% mutate(zg=paste(sclae,group,sep = "_"))
a <- shapiro.test(bba$fc^2)
b <- bartlett.test(bbc$fc,bbc$zg)
c <- leveneTest(bbc$fc,as.factor(bbc$zg))


ggplot(data=b1a, mapping = aes(
  x = maas_g, y = fc)) +
  geom_boxplot()