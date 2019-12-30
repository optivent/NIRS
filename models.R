# models

clean.it <- function() {
  basic.packages <- c("package:stats","package:graphics",
                      "package:grDevices","package:utils",
                      "package:datasets","package:methods",
                      "package:base")
  package.list <- dplyr::setdiff( search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)] , basic.packages)
  if (length(package.list)>0)  for(package in package.list) detach(package, character.only=TRUE)
  ll <- dplyr::setdiff( ls(envir = globalenv()), ## objects to exclude from cleaning
                        c("clean.it")) 
  rm(list = ll, envir = globalenv()); gc() # or sessionInfo()
  #devtools::install_github('bbc/bbplot'), requires bioconductor as dependency
  if(!require(pacman))install.packages("pacman")
  pacman::p_load(tidyverse,purrr, magrittr,readxl,lubridate, here, qwraps2,
                 tsibble, chron, knitr, kableExtra, magick, webshot)
}
clean.it()

here("inputs")
data <- readRDS("NIRS_interpolated.rds")
<<<<<<< HEAD



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

# Chi squared for gender inequalities
library(MASS); chisq.test(sumar$Group, sumar$Gender)
# p-value = 0.9649, *NS χ2 Pearson's chi-squared test
detach(package:MASS) 



library(dunn.test)
dunn.test(sumar$Weight, sumar$Group ,method = "holm",alpha = 0.05)
dunn.test(sumar$`Age (months)`, sumar$Group ,method = "holm",alpha = 0.05)

# the table with the demographics and duration

library(qwraps2); options(qwraps2_markup = "markdown")

# dt <- sumar; 
# summary_table(group_by(dt, Group), 
#               dplyr::select(dt, .data$`Age (months)`, .data$Weight, .data$Gender, .data$`Duration (minutes)`) %>%
#               qsummary(.,
#                  numeric_summaries = list("Median (1st Q., 3rd Q.)" = "~ qwraps2::median_iqr(%s)"),
#                  n_perc_args = list(digits = 1, show_symbol = TRUE, show_denom = "always"))); rm(dt)

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
  kable_styling(bootstrap_options = c("striped", "hover"), 
                full_width = F, position = "c") %>% 
  column_spec(1, bold = T) %>% row_spec(4, bold = F, color = "white", background = "#D7261E") %>% 
  footnote(general = "All values are presented as median value (25th percentile, 75th percentile).
  % dev. Q25/median/Q75 is the procentual deviation from baseline per patient per group
  (the 25th percentile, median and 75th percentile)")
rm(gender_sumar)


# the table with the results and its statistic

sumar %>% select(c(Group, `% dev. Q25`:`% dev. Q75`,`% time under baseline`)) %>% na.omit %>% 
  group_by(Group) %>% 
  summarise_if(is.numeric, list(~ qwraps2::median_iqr(.))) %>%
  ungroup() %>% 
  add_row(
    Group = "p value",
    `% dev. Q25` = "not significant",
    `% dev. median` = "not significant",
    `% dev. Q75` = "p < 0.01 | Kruskal-Wallis",
    `% time under baseline` = "p = 0.01 | Kruskal-Wallis") %>% 
  purrr::modify_if(~is.numeric(.), ~round(., 1)) %>%
  t() %>% as_tibble(rownames = " ") %>% setNames(as.character(.[1,])) %>%
  slice(-1) %>% rename(" " = Group) %>% 
  kable(align = "c", "html", booktabs = T,
        caption = "Table 2. Procentual NIRS deviation and time under baseline") %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), 
                full_width = F, position = "c") %>% 
  column_spec(1, bold = T) %>% row_spec(4, bold = F, color = "white", background = "#D7261E") %>% 
  footnote(general = "All values are presented as median value (25th percentile, 75th percentile).
  % dev. Q25/median/Q75 is the procentual deviation from baseline per patient per group
  (the 25th percentile, median and 75th percentile)") 






library("ggpubr")

comparison_list <- list(
  c("Anesthesia with surgery", "Anesthesia without surgery"),
  c("Anesthesia with surgery", "Sedation"),
  c("Anesthesia without surgery", "Sedation"))

# ggpubr::ggboxplot(na.omit(sumar %>% rename (median_proc_dev = `% dev. median`)),
#                   x = "Group", y = "median_proc_dev", 
#                   color = "Group",
#                   ylab = "median procentual deviation",
#                   add = "jitter", shape = "Group",
#                   palette = c("brown", "darkorange", "red")) +
#   stat_compare_means(comparisons = comparison_list) +
#   stat_compare_means(label.y = 57)

ggpubr::ggboxplot(na.omit(sumar %>% rename (time_under_BL = `% time under baseline`)),
                  x = "Group", y = "time_under_BL", 
                  color = "Group",
                  ylab = "% time under baseline", 
                  add = "jitter", shape = "Group",
                  palette = c("brown", "darkorange", "red")) +
  stat_compare_means(comparisons = comparison_list) +
  stat_compare_means(label.y = 130)

=======
manualtable <- readRDS("manualtable.rds")

# summary %>% group_by(Group) %>% tally()
# 
# Group                           n
# 1 Anesthesia with surgery       29
# 2 Anesthesia without surgery    21
# 3 Sedation                      31
#data %>% group_by(Group, Patient) %>% summarise(n = n()) %>% View()

set.seed(111)
boot_NIRS <- rbind(
  filter(data, Group %in% c("Anesthesia with surgery","Anesthesia without surgery")) %>% 
  group_by(Patient) %>% 
    sample_n(size = 3194 * 31/10, replace = TRUE) %>% 
  ungroup(),
  filter(data, Group == "Sedation") %>% 
    group_by(Patient) %>% 
     sample_n(size = 3194 * 51/10, replace = TRUE) %>% 
    ungroup()
)

#boot_NIRS %>% group_by(Group) %>% tally()

boot_NIRS %>%
  filter(between(PROC_min, -30, 0)) %>% mutate(PROC_min = PROC_min/100) %>% 
  mutate(Group = ifelse(Group %in% c("Anesthesia with surgery", "Anesthesia without surgery"), "Anaesthesia", "Sedation")) %>% 
  ggplot(aes(x = PROC_min, Group = Group)) +
  geom_histogram(bins = 30) + 
  scale_x_continuous(labels = percent) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l") + 
  geom_vline(aes(xintercept = -0.2), colour="red", linetype = "longdash")+
  facet_wrap(~Group) +
  theme_bw(base_size = 16) +
  labs(subtitle = "Table 3. Exponential histogram from bootstrapped data", 
       x = "negative procentual deviation from baseline",
       y = "number of observations",
       caption = "each patient is equally represented by bootstrapping")
>>>>>>> 1b4396704a43602230e6f1ae17ca408ef89ee664
 
summary %>% summary
