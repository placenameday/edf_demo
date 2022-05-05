bname<-"b3"
stname <-1
metri <-"fc"
aoilist <- c("b3_ne","b3_pos", "b3_neg", "b3_thr")

dtc2 <- dt %>% filter(block_name==bname, stim==stname, AOI %in% aoilist) %>%
  group_by(maas_g, tgjl_g, sas_g, participant_phone, AOI) %>%
  summarise(fc=mean(!!sym(metri), na.rm=T), maas=mean(maas), tgjl=mean(tgjl), sas=mean(sas)) %>%
  filter(!is.na(fc)) %>%
  ungroup() %>%
  select(participant_phone, AOI, tgjl, maas, sas, fc)

dtc2$AOI = factor(dtc2$AOI)
dtc2$participant_phone = factor(dtc2$participant_phone)


mixed = lmer(fc ~ maas*AOI + (1 | participant_phone), data = dtc2)
summary(mixed)
anova(mixed)
plot_model(mixed,"eff",axis.title=c("length/10","p(trial)"))
tab_model(mixed)
plot_model(mixed, vline.color = "red")
plot_model(mixed, sort.est = TRUE)
plot_model(mixed, show.values = TRUE, value.offset = .3)
plot_model(mixed, type = "pred", terms = c("maas", "AOI"))


fit1 <- lmer(data = dtc2, fc ~ maas*AOI + (1  | participant_phone))
summary(fit1)


anovaModelRM = aov(fc ~ maas*AOI + Error(participant_phone), dtc2)
summary(anovaModelRM)