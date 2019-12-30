## detect events, first run the import_data.R to have the data and IIDkey

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

<<<<<<< HEAD
setwd(here("inputs")); data <- readRDS("NIRS_interpolated.rds")
=======
here("inputs")
data <- readRDS("NIRS_interpolated.rds")
>>>>>>> 1b4396704a43602230e6f1ae17ca408ef89ee664


# this function will find the number of unintrerrupted seconds with alarms
seqle <- function(x,incr=1) {
  if(!is.numeric(x)) x <- as.numeric(x)
  n <- length(x)
  y <- x[-1L] != x[-n] + incr
  i <- c(which(y|is.na(y)),n)
  list(lengths = diff(c(0L,i)),
       values = x[head(c(0L,i)+1L,-1L)])
}

sumar <- data %>% 
  group_by(Patient) %>% 
    summarise(
      Group = first(Group) %>% as.factor(),
<<<<<<< HEAD
      "Age (months)" = first(Months),
      Weight = first(Weight),
      Gender = first(Gender) %>% as.factor(),
      #date = first(as.Date(datetime)),
      #start = strftime(first(datetime), format="%H:%M:%S"),
      #stop = strftime(last(datetime), format="%H:%M:%S"),
      'Duration (minutes)' = as.integer(floor(max(minutes))), 
      #nr_obs = n(),
      #'baseline' = first(Rbaseline + Lbaseline)/2,
=======
      months = first(Months),
      weight = first(Weight),
      gender = first(Gender) %>% as.factor(),
      date = first(as.Date(datetime)),
      start = strftime(first(datetime), format="%H:%M:%S"),
      stop = strftime(last(datetime), format="%H:%M:%S"),
      minutes = as.integer(floor(max(minutes))), 
      nr_obs = n(),
      'baseline' = first(Rbaseline + Lbaseline)/2,
>>>>>>> 1b4396704a43602230e6f1ae17ca408ef89ee664
      #'min. abs. ' = min(NIRS_min, na.rm = TRUE),
      #'%dev. min.' = min(PROC_min, na.rm = TRUE),
      '% dev. Q25' = (quantile(L_proc, 0.25, na.rm = TRUE) + quantile(R_proc, 0.25, na.rm = TRUE))/2,
      '% dev. median' = (median(L_proc, na.rm = TRUE) + median(R_proc, na.rm = TRUE))/2,
      '% dev. Q75' = (quantile(L_proc, 0.75, na.rm = TRUE) + quantile(R_proc, 0.75, na.rm = TRUE))/2,
      'max. consec.(s)' = max(seqle(interp_ID[alarm_min ==1], incr = 1)[[1]]), # here comes the seqle function
<<<<<<< HEAD
      'Alarms (seconds)'= length(which(alarm_min==1)),
      '% time under baseline' = floor(100*length(which(under_BL==1))/n())
=======
      'alarms (seconds)'= length(which(alarm_min==1))
      #'% time < BL' = floor(100*length(which(under_BL==1))/n())
>>>>>>> 1b4396704a43602230e6f1ae17ca408ef89ee664
      #penalization1 = sum(PROC_min[PROC_min < 0] ^ 2, na.rm = TRUE)/length(which(PROC_min < 0)),
      #penalization2 = sum(PROC_min[PROC_min < 0] ^ 2, na.rm = TRUE)/n(),
      #penalization3 = sum(PROC_min[PROC_min < 0] ^ 2, na.rm = TRUE)
      #test = mean(PROC_min[PROC_min<0], na.rm = TRUE),
      #neg_sum = sum(PROC_min[PROC_min < 0], na.rm = TRUE)
      #'neg.surface/% time < BL' = 'negative surface'/'% time < BL'
  ) %>%
  dplyr::ungroup() %>% 
  rename(Pat.ID = Patient) %>% 
  purrr::modify_if(~is.numeric(.), ~round(., 3)) %>% 
<<<<<<< HEAD
  arrange(Group, `Age (months)`) 
# rm(seqle)
=======
  select(-c(date,start,stop,nr_obs)) %>% 
  arrange(Group, months) 

>>>>>>> 1b4396704a43602230e6f1ae17ca408ef89ee664
# swr = function(string, nwrap=25) { # the nwrap defines the width of the text wrapping
#   paste(strwrap(string, width=nwrap), collapse="\n")
# };swr = Vectorize(swr)

<<<<<<< HEAD

## Supplementary Table 
library(kableExtra)
sumar %>%
  mutate('% time under baseline' = paste0(`% time under baseline`, " %")) %>% 
=======
sumar %>%
  #mutate('% time < BL' = paste0(`% time < BL`, " %")) %>% 
>>>>>>> 1b4396704a43602230e6f1ae17ca408ef89ee664
  purrr::modify_if(~is.numeric(.), ~round(., 1)) %>% 
  dplyr::select(-Group) %>% 
  kable(align = "c", "html", booktabs = T,
        caption = "Supplementary descriptive Table of each Patient") %>%
  kable_styling(bootstrap_options = c("striped","condensed",
                                      "scale_down",font_size = 5),
                full_width = F, position = "center") %>% 
  #collapse_rows(columns = 2, valign = "middle") %>% 
  pack_rows("Anaesthesia with surgery", 1, 29, label_row_css = "background-color: #666; color: #fff;") %>% 
  pack_rows("Anaesthesia without surgery", 30, 50, label_row_css = "background-color: #666; color: #fff;") %>% 
  pack_rows("Sedation", 51, 81, label_row_css = "background-color: #666; color: #fff;") %>% 
<<<<<<< HEAD
  row_spec(c(2,31,53), bold = T)  %>% 
  footnote(general = "% dev. Q25/median/Q75 = 1st, 2nd, and 3rd quartile of procentual deviation from baseline.
  max. consec. (s) = the duration in seconds of the longest desaturation under -20%.
  Alarms (seconds) = how many seconds with a NIRS value below -20% from baseline.") 


# Figure 1. NIRS monitoring in patients with events# patients with events (4,24,89) - see summary
data %>% 
  filter(Patient %in% c(4,24,89)) %>% 
  filter(PROC_avg < 25) %>%
  filter(PROC_min > -35) %>% 
  left_join(rename(sumar, Patient = Pat.ID) %>% select(Patient, `Alarms (seconds)`,`% time under baseline`)) %>%  
=======
  row_spec(c(2,31,53), bold = T) %>% 
  footnote(general = "
  Baseline = the average of left and right NIRS\n
  minutes = the duration of anaeshtesia or sedation\n
  % dev. min. = the lowest procentual deviation in any NIRS channel\n
  % dev. Q25/median/Q75 = 1st, 2nd, and 3rd quartile of procentual deviation\n
  max.consec.(s) = maximum duration in seconds of a NIRS drop below -20%\n
  sum alarms (s) = the sum of the seconds with a NIRS drop below -20%\n
  ") 


# summary on the three or two groups 

library(qwraps2)
sumar_groups_A <- sumar %>% 
  mutate(Group = ifelse(Group %in% c("Anesthesia with surgery", "Anesthesia without surgery"), "Anaesthesia", "Sedation")) %>% 
  group_by(Group) %>% 
    dplyr::select(months, weight, minutes, baseline, `%dev. median`) %>% 
    rename('% deviation' = `%dev. median`,
           Months = months,
           Weight = weight,
           'Baseline' = baseline, 
           'Duration' = minutes) %>% 
    na.omit() %>% 
    summarize_if(is.numeric, funs(qwraps2::median_iqr(.,na_rm = TRUE))) %>% 
  ungroup() %>% t() %>% as_tibble(rownames = " ") %>% setNames(as.character(.[1,])) %>% slice(-1)  

sumar_groups_B <- sumar 
  
  
  

four_groups <- sumar %>% group_by(Group) %>% 
  summarise_if(is.numeric, quantile(c(0.25, 0.5, 0.75)), na.rm = TRUE) %>% 
  filter(Group != "Sedation") %>% 
rbind(
  sumar %>%  mutate(Group = as.character(Group)) %>% 
  mutate(Group = ifelse(Group %in% c("Anesthesia with surgery", "Anesthesia without surgery"), "Anaesthesia", Group)) %>% 
  group_by(Group) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)
) %>% 
  purrr::modify_if(~is.numeric(.), ~round(., 1)) %>%
  t()
  
# all four groups in a kable table   
four_groups %>% kable(align = "c", "html", booktabs = T) %>%
  kable_styling(bootstrap_options = c("striped","condensed"),full_width = F, position = "center")

four_groups[-c(4,6,10,12),-c(1,2)] %>% kable(align = "c", "html") %>%
  kable_styling(full_width = F, position = "center") %>% 
  row_spec(1, bold = T, font_size = 14) %>%  
  row_spec(8, bold = F, color = "white", background = "#D7261E") %>% 
  footnote(general = "
  All values are presented as means per patient per group\n
  Baseline is the average of left and right NIRS\n
  % dev. Q25/median/Q75 is the 1st, 2nd, and 3rd quartile of procentual deviation from baseline\n
  alarms (seconds) = total duration with NIRS drop below -20%\n
  ") 


wilcox.test(summary_AN_SE$`alarms (seconds)` ~ summary_AN_SE$Group, exact = FALSE)

library("ggpubr")
ggboxplot(summary_AN_SE %>% rename(alarms = "alarms (seconds)"), 
          x = "Group", y = "alarms", 
          color = "Group", palette = c("#00AFBB", "#E7B800"),
          ylab = "alarms", xlab = "Groups")



# patients with events (4,24,89) - see summary
g <- data %>% 
  filter(Patient %in% c(4,24,89)) %>% 
  filter(PROC_avg < 25) %>%
  filter(PROC_min > -35) %>% 
>>>>>>> 1b4396704a43602230e6f1ae17ca408ef89ee664
  transmute(
    Patient = as.factor(paste(
      Group,
      paste0("Patient# ",Patient),
<<<<<<< HEAD
      paste0(Months," months old ",Gender),
      paste0(`% time under baseline`,"% time under baseline "),
      paste0(`Alarms (seconds)`," seconds under - 20% "),
=======
      paste0(Months," months ",Gender),
>>>>>>> 1b4396704a43602230e6f1ae17ca408ef89ee664
      sep="\n")),
    time  = as.double(minutes),
    'lowest deviation' = PROC_min/100,
    'average deviation' = PROC_avg/100) %>% 
  pivot_longer(-c(Patient,time)) %>% 
  ggplot(aes(x = time,y = value,colour = name, group = name)) 


g + geom_point(size = 0.5, alpha = 0.5, na.rm = TRUE) + 
  scale_y_continuous(breaks = seq(-0.3,0.2, by = 0.1),labels = scales::percent) +
  scale_x_continuous(breaks=seq(0, 240, by = 60)) +
  theme_bw(base_size = 12) +
  geom_hline(aes(yintercept = 0)) + 
  geom_hline(aes(yintercept = -0.2)) +
  theme(
    plot.caption = element_text(size = 14, hjust = 0.5),
    legend.position="bottom",
    legend.box = "horizontal",
    legend.title = element_blank()
    ) +
  guides(color = guide_legend(override.aes = list(size=5))) +
  facet_wrap(.~Patient, scales = "free_x") +
  labs(title = "Figure 1. The three children with events",
       y = "procentual deviation from baseline (0%)",
       x = "time in minutes")
<<<<<<< HEAD



set.seed(111)
sumar %>% count(Group) %>% mutate(m = 100/n)

rbind(
  filter(data, Group == "Anesthesia with surgery") %>% 
    group_by(Patient) %>% sample_n(size = 3333 * 3.45, replace = TRUE),
  filter(data, Group == "Anesthesia without surgery") %>% 
    group_by(Patient) %>% sample_n(size = 3333 * 4.76, replace = TRUE),
  filter(data, Group == "Sedation") %>% 
    group_by(Patient) %>%sample_n(size = 3333 * 3.23, replace = TRUE)
) -> boot_data
  
boot_data %>% group_by(Group) %>% count()

#boot_NIRS %>% group_by(Group) %>% tally()
library(scales)
boot_data %>%
  filter(between(PROC_min, -30, 0)) %>% mutate(PROC_min = PROC_min/100) %>% 
  #mutate(Group = ifelse(Group %in% c("Anesthesia with surgery", "Anesthesia without surgery"), "Anaesthesia", "Sedation")) %>% 
  ggplot(aes(x = PROC_min, Group = Group)) +
  geom_histogram(bins = 30) + 
  scale_x_continuous(labels = percent) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l") + 
  geom_vline(aes(xintercept = -0.2), colour="red", linetype = "longdash")+
  facet_wrap(~Group) +
  theme_bw(base_size = 16) +
  labs(subtitle = "Figure 2. Histogram of the % NIRS desaturations", 
       x = "negative procentual deviation from baseline",
       y = "number of observations",
       caption = "each patient and group is equally represented by bootstrapping")                
=======
                
>>>>>>> 1b4396704a43602230e6f1ae17ca408ef89ee664
