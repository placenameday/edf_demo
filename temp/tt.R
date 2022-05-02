mixa <- function(dt, bname, sname, a, group, metri){
  
  dtb3 <- dt %>% filter(block_name==bname, stim==sname, AOI %in% a) %>%
    mutate(tgroup=!!sym(group)) %>%
    filter(tgroup!="mid") %>%
    group_by(participant_phone, tgroup, AOI) %>%
    summarise(fc=mean(!!sym(metri), rm.na=T)) %>%
    select(participant_phone, tgroup, fc, AOI) %>%
    convert_as_factor(participant_phone, tgroup, AOI) %>% ungroup()
  
  outdata <- dtb3 %>%
    group_by(AOI, tgroup) %>%
    identify_outliers(fc)
  dtb3_r <- anti_join(dtb3, outdata)
  outdata2 <- dtb3_r %>%
    group_by(AOI, tgroup) %>%
    identify_outliers(fc)
  dtb3_r2 <- anti_join(dtb3_r, outdata2)
  outdata3 <- dtb3_r2 %>%
    group_by(AOI, tgroup) %>%
    identify_outliers(fc)
  dtb3_r3 <- anti_join(dtb3_r2, outdata3)
  outdata4 <- dtb3_r3 %>%
    group_by(AOI, tgroup) %>%
    identify_outliers(fc)
  dtb3_r4 <- anti_join(dtb3_r3, outdata4)
  outdata5 <- dtb3_r4 %>%
    group_by(AOI, tgroup) %>%
    identify_outliers(fc)
  
  ms <- dtb3_r4 %>%
    group_by(AOI, tgroup) %>%
    get_summary_stats(fc, type = "mean_sd")
  
  bxp <- ggboxplot(
    dtb3_r4, x = "AOI", y = "fc",
    color = "tgroup", palette = "jco"
  ) + labs(y=metri)
  bxp
  
  ztx <- dtb3_r4 %>%
    group_by(AOI, tgroup) %>%
    shapiro_test(fc)
  
  ztp <- ggqqplot(dtb3_r4, "fc", ggtheme = theme_bw()) +
    facet_grid(AOI ~ tgroup)
  
  qx <- dtb3_r4 %>%
    group_by(AOI) %>%
    levene_test(fc ~ tgroup)
  
  res.aov <- anova_test(
    data = dtb3_r4, dv = fc, wid = participant_phone,
    between = tgroup, within = AOI
  )
  fc <- get_anova_table(res.aov)
  
  # Effect of group at each time point
  one.way <- dtb3_r4 %>%
    group_by(AOI) %>%
    anova_test(dv = fc, wid = participant_phone, between = tgroup) %>%
    get_anova_table() %>%
    adjust_pvalue(method = "bonferroni")
  one.way
  
  # Pairwise comparisons between group levels
  pwc <- dtb3_r4 %>%
    group_by(AOI) %>%
    pairwise_t_test(fc ~ tgroup, p.adjust.method = "bonferroni")
  sh <- pwc
  
  # Effect of time at each level of exercises group
  one.way2 <- dtb3_r4 %>%
    group_by(tgroup) %>%
    anova_test(dv = fc, wid = participant_phone, within = AOI) %>%
    get_anova_table() %>%
    adjust_pvalue(method = "bonferroni")
  one.way2
  
  # Pairwise comparisons between time points at each group levels
  # Paired t-test is used because we have repeated measures by time
  pwc2 <- dtb3_r4 %>%
    group_by(tgroup) %>%
    pairwise_t_test(
      fc ~ AOI, paired = F, 
      p.adjust.method = "bonferroni"
    )
  pwc2
  
  
  # Visualization: boxplots with p-values
  pwc <- pwc %>% add_xy_position(x = "AOI")
  pwc.filtered <- pwc
  f <- bxp + 
    stat_pvalue_manual(pwc.filtered, tip.length = 0, hide.ns = TRUE) +
    labs(
      subtitle = get_test_label(res.aov, detailed = TRUE),
      caption = get_pwc_label(pwc)
    )
  list(ztx=ztx, ztp=ztp, ms=ms, qx=qx, fc=fc, jd1=one.way, jd2=one.way2, sh=sh, sh2=pwc2, f=f)
}

k <- mixa(dt, "b3", 1, c("b3_ne","b3_pos", "b3_neg", "b3_thr"), "tgjl_g", "dw")



dtb3aa <- dt %>% filter(block_name=="b3", stim==1, AOI %in% c("outter_l","outter_r", "outter_t", "outter_b")) %>%
  mutate(tgroup=tgjl_g) %>%
  filter(tgroup=="high") %>%
  group_by(participant_phone, AOI) %>%
  summarise(fc=mean(fc, rm.na=T)) %>%
  select(participant_phone, AOI, fc) %>%
  pivot_wider(names_from = AOI, values_from = fc) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  pivot_longer(2:5, names_to = "AOI", values_to = "fc") %>%
  ungroup() %>%
  arrange(AOI, participant_phone) %>%
  convert_as_factor(participant_phone, AOI)

msaaa <- dtb3aa %>% 
  group_by(AOI) %>%
  get_summary_stats(fc, type = "common")

res.friedaa <- friedman_test(fc ~ AOI | participant_phone, data=dtb3aa)
res.friedaa

effsize <- dtb3aa %>% friedman_effsize(fc ~ AOI |participant_phone)

# pairwise comparisons
pwc <- dtb3 %>%
  wilcox_test(fc ~ AOI, paired = TRUE, p.adjust.method = "bonferroni")
db <- pwc

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "AOI")
f <- ggboxplot(selfesteem, x = "AOI", y = "fc", add = "point") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
list(ms, res.fried, effsize, db, f)
