bname <- c("b1a", "b1b")
sname <- 1
a <- c("b1_2_a","b1_2_q", "bg")
sname <- c(2)
mlist <- c("k1","k2","k3","k4","fc", "afd", "dff", "ffd", "pd", "dw", "asa", "sc", "sad", "asd", "spv")
metri <- "key"

dt <- read.csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/ea/data/inner_dt_aoi_re_2.csv")




# 数据准备
dtc2 <- dt %>% filter(block_name %in% bname, stim %in% sname) %>%
  group_by(participant_phone, block_name, type, stim) %>%
  summarise(fc=mean(!!sym(metri), na.rm=T), maas=mean(maas), tgjl=mean(tgjl), sas=mean(sas)) %>%
  filter(!is.na(fc)) %>%
  ungroup() %>%
  convert_as_factor(participant_phone, block_name, type, stim) %>%
  select(participant_phone,  block_name, type, stim, tgjl, maas, sas, fc)


# 极端值处理

dtb3_r4 <- dtc2



# 模型拟合
mixed_maas = lmer(fc ~ maas+block_name+maas*block_name+maas*type+block_name*type+block_name*type*maas + (1 | participant_phone), data = dtb3_r4)
mixed_sas = lmer(fc ~ sas*block_name+sas*type+block_name*type+block_name*type*sas + (1 | participant_phone), data = dtb3_r4)
mixed_two = lmer(fc ~ maas+sas+block_name+maas*sas+maas*block_name+sas*block_name+maas*type+block_name*type+sas*type+block_name*type*maas+block_name*type*sas+block_name*type*sas*maas + (1 | participant_phone), data = dtb3_r4)

# maas 结果
# 基本参数
maas_t <- model_parameters(mixed_maas)
maas_perfor <- model_performance(mixed_maas)
maas_jh <- anova(mixed_maas)
maas_eta <- eta_squared(mixed_maas)
# 简单效应与配对检验
maas_emm <- emmeans(mixed_maas, pairwise ~ type | maas*block_name, cov.reduce = function(x) quantile(x, c(0.1, 0.3, 0.5, 0.7,0.9)))
maas_emm2 <- emmeans(mixed_maas, pairwise ~ type|block_name)
maas_se1 <- joint_tests(mixed_maas , by=c("block_name"))
maas_se1.5 <- joint_tests(mixed_maas , by=c("type"))
maas_se2 <- joint_tests(mixed_maas , by=c("maas"), cov.reduce = function(x) quantile(x, c(0.1, 0.3, 0.5, 0.7,0.9)))

# 交互作用可视化
maas_inter_v <- emmip(mixed_maas, maas ~ block_name|type, mult.name = "variety", cov.reduce = FALSE)
# 模型固定因子效应量
maas_p1 <- plot_model(mixed_maas, show.values = TRUE, value.offset = .3, title = metri)
# 模型交互1
maas_p2 <- plot_model(mixed_maas, type = "pred", terms = c("maas", "block_name", "type"), title = metri, axis.title = metri)
# 模型交互2
maas_p3 <- plot_model(mixed_maas, type = "pred", terms = c("maas","type","block_name"), title = metri, axis.title = metri)
# 结果汇总
maas_results <- list(maas_t=maas_t, maas_perfor=maas_perfor, maas_jh=maas_jh, maas_eta=maas_eta,  maas_emm=maas_emm, maas_emm2=maas_emm2,maas_se1=maas_se1, maas_se2=maas_se2, maas_inter_v=maas_inter_v, p1=maas_p1, p2=maas_p2, maas_p3=maas_p3)

# sas 结果
# 基本参数
sas_t <- model_parameters(mixed_sas)
sas_perfor <- model_performance(mixed_sas)
sas_jh <- anova(mixed_sas)
sas_eta <- eta_squared(mixed_sas)
# 简单效应与配对检验
sas_emm <- emmeans(mixed_sas, pairwise ~ type | sas*block_name, cov.reduce = function(x) quantile(x, c(0.1, 0.3, 0.5, 0.7,0.9)))
sas_emm2 <- emmeans(mixed_sas, pairwise ~ type|block_name)
sas_se1 <- joint_tests(mixed_sas , by=c("block_name"))
sas_se1.5 <- joint_tests(mixed_sas , by=c("type"))
sas_se2 <- joint_tests(mixed_sas , by=c("sas"), cov.reduce = function(x) quantile(x, c(0.1, 0.3, 0.5, 0.7,0.9)))

# 交互作用可视化
sas_inter_v <- emmip(mixed_sas, sas ~ block_name|type, mult.name = "variety", cov.reduce = FALSE)
# 模型固定因子效应量
sas_p1 <- plot_model(mixed_sas, show.values = TRUE, value.offset = .3, title = metri)
# 模型交互1
sas_p2 <- plot_model(mixed_sas, type = "pred", terms = c("sas", "block_name", "type"), title = metri, axis.title = metri)
# 模型交互2
sas_p3 <- plot_model(mixed_sas, type = "pred", terms = c("sas","type","block_name"), title = metri, axis.title = metri)
# 结果汇总
sas_results <- list(sas_t=sas_t, sas_perfor=sas_perfor, sas_jh=sas_jh, sas_eta=sas_eta,  sas_emm=sas_emm, sas_emm2=sas_emm2,sas_se1=sas_se1, sas_se2=sas_se2, sas_inter_v=sas_inter_v, p1=sas_p1, p2=sas_p2, sas_p3=sas_p3)

# two 结果
two_t <- model_parameters(mixed_two)
two_perfor <- model_performance(mixed_two)
two_jh <- anova(mixed_two)
two_eta <- eta_squared(mixed_two)
# 简单效应与配对检验
two_emm <- emmeans(mixed_two, pairwise ~ type|block_name | maas*sas, cov.reduce = function(x) quantile(x, c(0.3, 0.5, 0.7)))
two_se1 <- joint_tests(mixed_two , by=c("block_name"))
two_se2 <- joint_tests(mixed_two , by=c("maas"), cov.reduce = function(x) quantile(x, c(0.1, 0.3, 0.5, 0.7,0.9)))
two_se3 <- joint_tests(mixed_two , by=c("sas"), cov.reduce = function(x) quantile(x, c(0.1, 0.3, 0.5, 0.7,0.9)))
# 交互作用可视化
two_inter_v_m <- emmip(mixed_two, maas ~ block_name|type, mult.name = "variety", cov.reduce = function(x) quantile(x, c(0.1,0.2, 0.3,0.4, 0.5,0.6, 0.7,0.8,0.9)))
two_inter_v_s <- emmip(mixed_two, sas ~ block_name|type, mult.name = "variety", cov.reduce = function(x) quantile(x, c(0.1,0.2, 0.3,0.4, 0.5,0.6, 0.7,0.8,0.9)))
two_inter_v <- emmip(mixed_two, maas ~ block_name|type|sas, mult.name = "variety", cov.reduce = function(x) quantile(x, c(0.1, 0.3, 0.5, 0.7,0.9)))
# 模型固定因子
two_p1 <- plot_model(mixed_two, show.values = TRUE, value.offset = .3, title = metri)
# 模型交互1
two_p2 <- plot_model(mixed_two, type = "pred", terms = c("maas", "sas", "block_name","type"), title = metri, axis.title = metri)
# 模型交互2
two_p3 <- plot_model(mixed_two, type = "pred", terms = c("maas", "sas", "type","block_name"), title = metri, axis.title = metri)

two_results <- list(two_t=two_t, two_perfor=two_perfor, two_jh=two_jh, two_eta=two_eta,  two_emm=two_emm, two_se1=two_se1, two_se2=two_se2,  two_se3=two_se3, two_inter_v_m=two_inter_v_m,two_inter_v_s=two_inter_v_s, two_inter_v=two_inter_v, p1=two_p1, p2=two_p2, two_p3=two_p3)

f <- list(maas_results, sas_results, two_results)