

## Check with Shapiro-Wilk test for normality, select only the normal distributions
library(dlookr); dlookr::normality(group_by(sumar,Group)) %>% # grouped by "Group"
  mutate(normality = ifelse(p_value > 0.05, "normal", "non-normal"),
        p_value = format(p_value, scientific = FALSE)) %>% 
  filter(normality == "normal") %>% arrange(variable) %>% dplyr::select(variable, Group) 
# Normal variables across all groups are: Duration and % dev. Q25

## gender repartition in all three groups
gender_sumar <- sumar %>% 
  group_by(Group, Gender) %>% tally() %>%
    mutate(text = paste0(n, " ",Gender,"s")) %>% 
  group_by(Group) %>%
    summarise(
      procent = paste0("(",round(100*first(n)/(first(n)+last(n))),"%)"),
      Gender = paste(first(text), procent ,"&" ,last(text)),
      'Nr. of children' = sum(n)) %>% 
  select(-procent)

# # Chi squared for gender inequalities
# library(MASS); chisq.test(sumar$Group, sumar$Gender)
# # p-value = 0.9649, *NS χ2 Pearson's chi-squared test
# detach(package:MASS) 


library(dunn.test)
dunn.test(sumar$Weight, sumar$Group ,method = "holm",alpha = 0.05)
dunn.test(sumar$`% time under baseline`, sumar$Group, method = "holm", alpha = 0.05)
dunn.test(sumar$`Duration (minutes)`, sumar$Group, method = "holm", alpha = 0.05)
dunn.test(sumar$`Age (months)`, sumar$Group ,method = "holm",alpha = 0.05)

# the table with the demographics and duration

sumar %>% select(Group:Weight,`Duration (minutes)`) %>% na.omit() %>% 
  group_by(Group) %>% 
   summarise_if(is.numeric, list(~ qwraps2::median_iqr(.))) %>%
  ungroup() %>% 
  left_join(gender_sumar) %>% select(1,6,2,3,5,4) %>% 
  add_row(
    Group = "p value",
    `Nr. of children` = " ",
    `Age (months)` = "not significant",
    Weight = "not significant",
    Gender = "not significant",
    `Duration (minutes)` = "p < 0.01 | anova"
  ) %>% 
  rename('Weight (kg)' = Weight) %>% 
  purrr::modify_if(~is.numeric(.), ~round(., 1)) %>%
  t() %>% as_tibble(rownames = " ") %>% setNames(as.character(.[1,])) %>%
  slice(-1) %>% rename(" " = Group) %>% 
  kable(align = "c", "html", booktabs = T,
        caption = "Table 1. Demography and duration of anesthesia / sedation") %>% 
  kable_styling(bootstrap_options = c("striped"), 
                full_width = F, position = "c") %>% 
  column_spec(1, bold = T) %>% 
  #row_spec(5, bold = F, color = "white", background = "#D7261E") %>% 
  footnote(general = "All values are presented as: median value (25th percentile, 75th percentile).")
rm(gender_sumar)


# the table with the results and its statistic

library(dunn.test)
dunn.test(sumar$`% dev. median`, sumar$Group ,method = "holm",alpha = 0.05)
timeunderBL <- dunn.test(sumar$`% time under baseline`, sumar$Group ,method = "holm",alpha = 0.05)
rbind(timeunderBL$comparisons,timeunderBL$P.adjusted) %>% as_tibble() %>% t();rm(timeunderBL)


sumar %>% select(c(Group, `% dev. median`,`% time under baseline`)) %>% na.omit %>% 
  rename(`% NIRS deviation from baseline` = `% dev. median`) %>% 
  group_by(Group) %>% 
  summarise_if(is.numeric, list(~ qwraps2::median_iqr(.))) %>%
  ungroup() %>% 
  add_row(
    Group = "p value",
    `% NIRS deviation from baseline` = "not significant",
    `% time under baseline` = "p = 0.01 | Kruskal-Wallis") %>% 
  purrr::modify_if(~is.numeric(.), ~round(., 1)) %>%
  t() %>% as_tibble(rownames = " ") %>% setNames(as.character(.[1,])) %>%
  slice(-1) %>% rename(" " = Group) %>% 
  kable(align = "c", "html", booktabs = T,
        caption = "Table 2. Procentual NIRS deviation and time under baseline") %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), 
                full_width = F, position = "c") %>% 
  column_spec(1, bold = T) %>% 
  #row_spec(4, bold = F, color = "white", background = "#D7261E") %>% 
  footnote(general = "All values are presented as median value (25th percentile, 75th percentile).
  % dev. Q25/median/Q75 is the procentual deviation from baseline 
  (the 25th percentile, median and 75th percentile)") 


## The GRAPH with the difference in time under the baseline

sumar %>% select(Group, `% time under baseline`) %>% 
  rename(time_under_BL = `% time under baseline`) %>% mutate(time_under_BL = time_under_BL/100) %>% 
  ggplot(aes(x = Group, y = time_under_BL)) +
  geom_boxplot(aes(fill=Group), width = 0.7) +
  geom_point(size = 3,shape = 1, position = position_jitter(w = 0.3, h = 0)) + 
  theme_pubr(base_size = 14) +
  scale_fill_manual(values = c("grey90", "grey70", "grey50")) + 
  scale_y_continuous(labels = scales::percent, breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(),
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption = element_text(size = 14, hjust = 0)) +
  ggsignif::geom_signif(test="t.test",
                        comparisons = combn(levels(sumar$Group),2, simplify = F),
                        step_increase = 0.1) +
  labs(title = "Figure 3. Procent of time under NIRS baseline"
  #caption = "
  #The vertical axis represents the percent of NIRS values
  #below baseline per child (the small circles). For example, a patient 
  #plotted at 0% has no NIRS values below its baseline"
  )

