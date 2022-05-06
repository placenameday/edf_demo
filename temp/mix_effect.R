bname<-"b3"
stname <-1
metri <-"fc"
aoilist <- c("b3_ne","b3_pos", "b3_neg", "b3_thr")



# 数据准备
dtc2 <- dt %>% filter(block_name==bname, stim==stname, AOI %in% aoilist) %>%
  group_by(maas_g, tgjl_g, sas_g, participant_phone, AOI) %>%
  summarise(fc=mean(!!sym(metri), na.rm=T), maas=mean(maas), tgjl=mean(tgjl), sas=mean(sas)) %>%
  filter(!is.na(fc)) %>%
  ungroup() %>%
  convert_as_factor(participant_phone, AOI) %>%
  select(participant_phone, AOI, tgjl, maas, sas, fc)


# 极端值处理
outdata <- dtc2 %>%
  group_by(AOI) %>%
  identify_outliers(fc)
dtb3_r <- anti_join(dtc2, outdata)
outdata2 <- dtb3_r %>%
  group_by(AOI) %>%
  identify_outliers(fc)
dtb3_r2 <- anti_join(dtb3_r, outdata2)
outdata3 <- dtb3_r2 %>%
  group_by(AOI) %>%
  identify_outliers(fc)
dtb3_r3 <- anti_join(dtb3_r2, outdata3)
outdata4 <- dtb3_r3 %>%
  group_by(AOI) %>%
  identify_outliers(fc)
dtb3_r4 <- anti_join(dtb3_r3, outdata4)
outdata5 <- dtb3_r4 %>%
  group_by(AOI) %>%
  identify_outliers(fc)


# 模型拟合
mixed_maas = lmer(fc ~ maas+AOI+maas*AOI + (1 | participant_phone), data = dtb3_r4)
mixed_sas = lmer(fc ~ sas*AOI + (1 | participant_phone), data = dtb3_r4)
mixed_two = lmer(fc ~ maas+sas+AOI+maas*sas+maas*AOI+sas*AOI + (1 | participant_phone), data = dtb3_r4)

# maas 结果
# 基本参数
maas_t <- model_parameters(mixed_maas)
maas_perfor <- model_performance(mixed_maas)
maas_jh <- anova(mixed_maas)
maas_eta <- eta_squared(mixed_maas)
# 简单效应与配对检验
maas_emm <- emmeans(mixed_maas, pairwise ~ AOI | maas, cov.reduce = function(x) quantile(x, c(0.1, 0.3, 0.5, 0.7,0.9)))
maas_se1 <- joint_tests(mixed_maas , by=c("AOI"))
maas_se2 <- joint_tests(mixed_maas , by=c("maas"), cov.reduce = function(x) quantile(x, c(0.1, 0.3, 0.5, 0.7,0.9)))
maas_pair <- contrast(maas_emm, "consec", simple = "each", combine = TRUE, adjust = "mvt")
# 交互作用可视化
maas_inter_v <- emmip(mixed_maas, maas ~ AOI, mult.name = "variety", cov.reduce = FALSE)
# 模型固定因子效应量
maas_p1 <- plot_model(mixed_maas, show.values = TRUE, value.offset = .3, title = metri)
# 模型交互1
maas_p2 <- plot_model(mixed_maas, type = "pred", terms = c("maas", "AOI"), title = metri, axis.title = metri)
# 模型交互2
maas_p3 <- plot_model(mixed_maas, type = "pred", terms = c("AOI", "maas"), title = metri, axis.title = metri)
# 结果汇总
maas_results <- list(maas_t=maas_t, maas_perfor=maas_perfor, maas_jh=maas_jh, maas_eta=maas_eta,  maas_emm=maas_emm, maas_se1=maas_se1, maas_se2=maas_se2,  maas_pair=maas_pair, maas_inter_v=maas_inter_v, p1=maas_p1, p2=maas_p2, maas_p3=maas_p3)

# sas 结果
sas_t <- model_parameters(mixed_sas)
sas_perfor <- model_performance(mixed_sas)
sas_jh <- anova(mixed_sas)
sas_eta <- eta_squared(mixed_sas)
# 简单效应与配对检验
sas_emm <- emmeans(mixed_sas, pairwise ~ AOI | sas, cov.reduce = function(x) quantile(x, c(0.1, 0.3, 0.5, 0.7,0.9)))
sas_se1 <- joint_tests(mixed_sas , by=c("AOI"))
sas_se2 <- joint_tests(mixed_sas , by=c("sas"), cov.reduce = function(x) quantile(x, c(0.1, 0.3, 0.5, 0.7,0.9)))
sas_pair <- contrast(sas_emm, "consec", simple = "each", combine = TRUE, adjust = "mvt")
# 交互作用可视化
sas_inter_v <- emmip(mixed_sas, sas ~ AOI, mult.name = "variety", cov.reduce = FALSE)
# 模型固定因子效应量
sas_p1 <- plot_model(mixed_sas, show.values = TRUE, value.offset = .3, title = metri)
# 模型交互1
sas_p2 <- plot_model(mixed_sas, type = "pred", terms = c("sas", "AOI"), title = metri, axis.title = metri)
# 模型交互2
sas_p3 <- plot_model(mixed_sas, type = "pred", terms = c("AOI", "sas"), title = metri, axis.title = metri)
# 结果汇总
sas_results <- list(sas_t=sas_t, sas_perfor=sas_perfor, sas_jh=sas_jh, sas_eta=sas_eta,  sas_emm=sas_emm, sas_se1=sas_se1, sas_se2=sas_se2,  sas_pair=sas_pair, sas_inter_v=sas_inter_v, p1=sas_p1, p2=sas_p2, sas_p3=sas_p3)

# two 结果
two_t <- model_parameters(mixed_two)
two_perfor <- model_performance(mixed_two)
two_jh <- anova(mixed_two)
two_eta <- eta_squared(mixed_two)
# 简单效应与配对检验
two_emm <- emmeans(mixed_two, pairwise ~ AOI | maas*sas, cov.reduce = function(x) quantile(x, c(0.1, 0.3, 0.5, 0.7,0.9)))
two_se1 <- joint_tests(mixed_two , by=c("AOI"))
two_se2 <- joint_tests(mixed_two , by=c("maas"), cov.reduce = function(x) quantile(x, c(0.1, 0.3, 0.5, 0.7,0.9)))
two_se3 <- joint_tests(mixed_two , by=c("sas"), cov.reduce = function(x) quantile(x, c(0.1, 0.3, 0.5, 0.7,0.9)))
# 交互作用可视化
two_inter_v <- emmip(mixed_two, maas ~ AOI|sas, mult.name = "variety", cov.reduce = function(x) quantile(x, probs = seq(0, 1, 1/15)))
# 模型固定因子
two_p1 <- plot_model(mixed_two, show.values = TRUE, value.offset = .3, title = metri)
# 模型交互1
two_p2 <- plot_model(mixed_two, type = "pred", terms = c("maas", "sas", "AOI"), title = metri, axis.title = metri)
# 模型交互2
two_p3 <- plot_model(mixed_two, type = "pred", terms = c("sas", "maas", "AOI"), title = metri, axis.title = metri)

two_results <- list(two_t=two_t, two_perfor=two_perfor, two_jh=two_jh, two_eta=two_eta,  two_emm=two_emm, two_se1=two_se1, two_se2=two_se2,  two_se3=two_se3, two_inter_v=two_inter_v, p1=two_p1, p2=two_p2, two_p3=two_p3)

f <- list(maas_results, sas_results, two_results)


bname <- c("b1a", "b1b")
sname <- 1
a <- c("b1_1zhong","b1_1jin", "b1_1yuan")
mlist <- c("fc", "afd", "dff", "ffd", "pd", "dw", "asa", "sc", "sad", "asd", "spv")
metri <- "fc"

# 数据准备
dtc2 <- dt %>% filter(block_name %in% bname, stim==sname, AOI %in% a) %>%
  group_by(participant_phone, block_name, AOI) %>%
  summarise(fc=mean(!!sym(metri), na.rm=T), maas=mean(maas), tgjl=mean(tgjl), sas=mean(sas)) %>%
  filter(!is.na(fc)) %>%
  ungroup() %>%
  convert_as_factor(participant_phone, block_name, AOI) %>%
  select(participant_phone, block_name, AOI, tgjl, maas, sas, fc)


# 极端值处理
outdata <- dtc2 %>%
  group_by(block_name,AOI) %>%
  identify_outliers(fc)
dtb3_r <- anti_join(dtc2, outdata)
outdata2 <- dtb3_r %>%
  group_by(block_name,AOI) %>%
  identify_outliers(fc)
dtb3_r2 <- anti_join(dtb3_r, outdata2)
outdata3 <- dtb3_r2 %>%
  group_by(block_name,AOI) %>%
  identify_outliers(fc)
dtb3_r3 <- anti_join(dtb3_r2, outdata3)
outdata4 <- dtb3_r3 %>%
  group_by(block_name,AOI) %>%
  identify_outliers(fc)
dtb3_r4 <- anti_join(dtb3_r3, outdata4)
outdata5 <- dtb3_r4 %>%
  group_by(block_name,AOI) %>%
  identify_outliers(fc)

# 结果输出
mixed_maas = lmer(fc ~ maas*AOI*block_name + (1| participant_phone), data = dtb3_r4)
mixed_sas = lmer(fc ~ sas*AOI*block_name + (1 | participant_phone), data = dtb3_r4)
mixed_two = lmer(fc ~ maas+sas+AOI+maas*sas+maas*AOI+sas*AOI+AOI*block_name + (1 | participant_phone), data = dtb3_r4)
check_model(mixed_maas)
anova(mixed_maas)
ranova(mixed_maas)
# maas 结果
maas_t <- model_parameters(mixed_maas)
maas_p1 <- plot_model(mixed_maas, show.values = TRUE, value.offset = .3, title = metri)
maas_p2 <- plot_model(mixed_maas, type = "pred", terms = c("maas", "AOI", "block_name"), title = metri, axis.title = metri)
maas_results <- list(table=maas_t, p1=maas_p1, p2=maas_p2)

# sas 结果
sas_t <- model_parameters(mixed_sas)
sas_p1 <- plot_model(mixed_sas, show.values = TRUE, value.offset = .3, title = metri)
sas_p2 <- plot_model(mixed_sas, type = "pred", terms = c("sas", "AOI"), title = metri, axis.title = metri)
sas_results <- list(table=sas_t, p1=sas_p1, p2=sas_p2)

# two 结果
two_t <- model_parameters(mixed_two)
two_p1 <- plot_model(mixed_two, show.values = TRUE, value.offset = .3, title = metri)
two_p2 <- plot_model(mixed_two, type = "pred", terms = c("maas", "sas", "AOI", "block_name"), title = metri, axis.title = metri)
two_p3 <- plot_model(mixed_two, type = "pred", terms = c("sas", "maas", "AOI", "block_name"), title = metri, axis.title = metri)
two_results <- list(table=two_t, p1=two_p1, p2=two_p2, p3=two_p3)

f <- list(maas_results, sas_results, two_results)
