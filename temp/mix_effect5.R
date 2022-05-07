bname <- c("b5a", "b5b")
sname <- c(1)
a <- c("b5_q", "b5_a_1", "b5_a_2", "b5_a_3", "b5_a_4", "b5_a_5", "b5_a_6","bg")
mlist <- c("fc", "afd", "dff", "ffd", "pd", "dw", "asa", "sc", "sad", "asd", "spv")
metri <- "bc"

dt <-read.csv("/Users/placenameday/R study/edf_demo/data/processed/eye_tacking/ea/data/inner_dt_stim_re.csv")


# 数据筛选
dtb3 <- dt %>% filter(block_name%in%bname, stim==sname) %>%
  group_by(participant_phone, participant_group) %>% select(participant_phone, metri, participant_group) %>%
  drop_na() %>%
  summarise(fc=sum(!!sym(metri))) %>%
  convert_as_factor(participant_phone, participant_group) %>% ungroup() %>%
  reorder_levels(participant_group, order = c("a", "b", "c","d"))



outdata <- dtb3 %>%
  group_by(participant_group) %>%
  identify_outliers(fc)

dtb3_r <- anti_join(dtb3, outdata)

outdata2 <- dtb3_r %>%
  group_by(participant_group) %>%
  identify_outliers(fc)

dtb3_r2 <- anti_join(dtb3_r, outdata2)

outdata3 <- dtb3_r2 %>%
  group_by(participant_group) %>%
  identify_outliers(fc)

dtb3_r3 <- anti_join(dtb3_r2, outdata3)

outdata4 <- dtb3_r3 %>%
  group_by(participant_group) %>%
  identify_outliers(fc)

dtb3_r3 %>%
  group_by(participant_group) %>%
  get_summary_stats(fc, type = "mean_sd")


model  <- lm(fc ~ participant_group, data = dtb3_r3)
ggqqplot(residuals(model))
shapiro_test(residuals(model))

dtb3_r3 %>%
  group_by(participant_group) %>%
  shapiro_test(fc)

ggqqplot(dtb3_r3, "fc", facet.by = "participant_group")

ggboxplot(dtb3_r3 , x = "participant_group", y = "fc")

dtb3_r3 %>% levene_test(fc ~ participant_group)

res.aov <- dtb3_r3 %>% anova_test(fc ~ participant_group)
res.aov

pwc <- dtb3_r3 %>% tukey_hsd(fc ~ participant_group)
pwc

pwc <- pwc %>% add_xy_position(x = "participant_group")
ggboxplot(dtb3_r3, x = "participant_group", y = "fc") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc),
    y=metri
  )
