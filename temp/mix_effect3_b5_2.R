bname <- c("b5a","b5b")
sname <- c(1)

mlist <- c("bc", "bd",  "msc", "mspv")

metri <- "bc"


dt <- read_csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/ea/data/inner_dt_stim_re_2.csv")



dtc2 <- dt %>% filter(block_name%in%bname, stim%in%sname) %>%
  group_by(block_name, participant_phone, participant_group, trial_name) %>%
  summarise(fc=mean(!!sym(metri), na.rm=T), maas=mean(maas), tgjl=mean(tgjl), sas=mean(sas)) %>%
  filter(!is.na(fc)) %>%
  ungroup() %>%
  convert_as_factor(block_name, participant_phone, participant_group, trial_name) %>%
  select(block_name, participant_phone, participant_group, trial_name, tgjl, maas, sas, fc)


# 极端值处理
outdata <- dtc2 %>%
  group_by(trial_name) %>%
  identify_outliers(fc)
dtb3_r <- anti_join(dtc2, outdata)
outdata2 <- dtb3_r %>%
  group_by(trial_name) %>%
  identify_outliers(fc)
dtb3_r2 <- anti_join(dtb3_r, outdata2)
outdata3 <- dtb3_r2 %>%
  group_by(trial_name) %>%
  identify_outliers(fc)
dtb3_r3 <- anti_join(dtb3_r2, outdata3)
outdata4 <- dtb3_r3 %>%
  group_by(trial_name) %>%
  identify_outliers(fc)
dtb3_r4 <- anti_join(dtb3_r3, outdata4)
outdata5 <- dtb3_r4 %>%
  group_by(trial_name) %>%
  identify_outliers(fc)


# 模型拟合
mixed_maas = lmer(fc ~ maas+participant_group+maas*participant_group + (1 | participant_phone)+ (1 | trial_name), data = dtb3_r4)
mixed_sas = lmer(fc ~ sas+participant_group+sas*participant_group + (1 | participant_phone)+ (1 | trial_name), data = dtb3_r4)

# maas 结果
# 基本参数
maas_t <- model_parameters(mixed_maas)
maas_perfor <- model_performance(mixed_maas)
maas_jh <- anova(mixed_maas)
maas_eta <- eta_squared(mixed_maas)
# 简单效应与配对检验
maas_emm <- emmeans(mixed_maas, pairwise ~ maas | participant_group , cov.reduce = function(x) quantile(x, c(0.1, 0.3, 0.5, 0.7,0.9)))
maas_se1 <- joint_tests(mixed_maas , by=c("participant_group"))
maas_se1.5 <- joint_tests(mixed_maas , by=c("maas"),cov.reduce = function(x) quantile(x, c(0.1, 0.3, 0.5, 0.7,0.9)))

# 模型固定因子效应量
maas_p1 <- plot_model(mixed_maas, show.values = TRUE, value.offset = .3, title = metri)
# 模型交互1
maas_p2 <- plot_model(mixed_maas, type = "pred", terms = c("maas","participant_group"), title = metri, axis.title = metri)
# 模型交互2
maas_p3 <- plot_model(mixed_maas, type = "pred", terms = c("participant_group", "maas"), title = metri, axis.title = metri)
# 结果汇总
maas_results <- list(maas_t=maas_t, maas_perfor=maas_perfor, maas_jh=maas_jh, maas_eta=maas_eta,  maas_emm=maas_emm,  maas_se1=maas_se1,  maas_se1.5=maas_se1.5,  p1=maas_p1, p2=maas_p2, p3=maas_p3)

# sas 结果
# 基本参数
sas_t <- model_parameters(mixed_sas)
sas_perfor <- model_performance(mixed_sas)
sas_jh <- anova(mixed_sas)
sas_eta <- eta_squared(mixed_sas)
# 简单效应与配对检验
sas_emm <- emmeans(mixed_sas, pairwise ~ sas | participant_group , cov.reduce = function(x) quantile(x, c(0.1, 0.3, 0.5, 0.7,0.9)))
sas_se1 <- joint_tests(mixed_sas , by=c("participant_group"))
sas_se1.5 <- joint_tests(mixed_sas , by=c("sas"),cov.reduce = function(x) quantile(x, c(0.1, 0.3, 0.5, 0.7,0.9)))

# 模型固定因子效应量
sas_p1 <- plot_model(mixed_sas, show.values = TRUE, value.offset = .3, title = metri)
# 模型交互1
sas_p2 <- plot_model(mixed_sas, type = "pred", terms = c("sas","participant_group"), title = metri, axis.title = metri)
# 模型交互2
sas_p3 <- plot_model(mixed_sas, type = "pred", terms = c("participant_group", "sas"), title = metri, axis.title = metri)
# 结果汇总
sas_results <- list(sas_t=sas_t, sas_perfor=sas_perfor, sas_jh=sas_jh, sas_eta=sas_eta,  sas_emm=sas_emm,  sas_se1=sas_se1,  sas_se1.5=sas_se1.5,  p1=sas_p1, p2=sas_p2, p3=sas_p3)



f <- list(maas_results, sas_results)