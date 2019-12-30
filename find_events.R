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


setwd(here("inputs")); data <- readRDS("NIRS_interpolated.rds")



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
      "Age (months)" = first(Months),
      Weight = first(Weight),
      Gender = first(Gender) %>% as.factor(),
      #date = first(as.Date(datetime)),
      #start = strftime(first(datetime), format="%H:%M:%S"),
      #stop = strftime(last(datetime), format="%H:%M:%S"),
      'Duration (minutes)' = as.integer(floor(max(minutes))), 
      #nr_obs = n(),
      #'baseline' = first(Rbaseline + Lbaseline)/2,
      #'min. abs. ' = min(NIRS_min, na.rm = TRUE),
      #'%dev. min.' = min(PROC_min, na.rm = TRUE),
      '% dev. Q25' = (quantile(L_proc, 0.25, na.rm = TRUE) + quantile(R_proc, 0.25, na.rm = TRUE))/2,
      '% dev. median' = (median(L_proc, na.rm = TRUE) + median(R_proc, na.rm = TRUE))/2,
      '% dev. Q75' = (quantile(L_proc, 0.75, na.rm = TRUE) + quantile(R_proc, 0.75, na.rm = TRUE))/2,
      'max. consec.(s)' = max(seqle(interp_ID[alarm_min ==1], incr = 1)[[1]]), # here comes the seqle function
      'Alarms (seconds)'= length(which(alarm_min==1)),
      '% time under baseline' = floor(100*length(which(under_BL==1))/n())
      #penalization1 = sum(PROC_min[PROC_min < 0] ^ 2, na.rm = TRUE)/length(which(PROC_min < 0)),
      #penalization2 = sum(PROC_min[PROC_min < 0] ^ 2, na.rm = TRUE)/n(),
      #penalization3 = sum(PROC_min[PROC_min < 0] ^ 2, na.rm = TRUE)
      #test = mean(PROC_min[PROC_min<0], na.rm = TRUE),
      #neg_sum = sum(PROC_min[PROC_min < 0], na.rm = TRUE)
      #'neg.surface/% time < BL' = 'negative surface'/'% time < BL'
  ) %>%
  dplyr::ungroup() %>% 
  purrr::modify_if(~is.numeric(.), ~round(., 3)) %>% 
  arrange(Group, `Age (months)`) 
# rm(seqle)


# swr = function(string, nwrap=25) { # the nwrap defines the width of the text wrapping
#   paste(strwrap(string, width=nwrap), collapse="\n")
# };swr = Vectorize(swr)


## Supplementary Table 

sumar %>%
  mutate('% time under baseline' = paste0(`% time under baseline`, " %")) %>% 
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
  row_spec(c(2,31,53), bold = T)  %>% 
  footnote(general = "% dev. Q25/median/Q75 = 1st, 2nd, and 3rd quartile of procentual deviation from baseline.
  max. consec. (s) = the duration in seconds of the longest desaturation under -20%.
  Alarms (seconds) = how many seconds with a NIRS value below -20% from baseline.") 


# Figure 1. NIRS monitoring in patients with events# patients with events (4,24,89) - see summary

data %>% 
  filter(Patient %in% c(4,24,89)) %>% 
  filter(PROC_avg < 25) %>%
  filter(PROC_min > -35) %>% 
  left_join(sumar) %>% 
  transmute(
    Patient = as.factor(paste(
      Group,
      paste0("Patient# ",Patient),
      paste0(Months," months old ",Gender),
      paste0(`% time under baseline`,"% time under baseline "),
      paste0(`Alarms (seconds)`," seconds under - 20% "),
      sep="\n")),
    time  = as.double(minutes),
    'lowest deviation' = PROC_min/100,
    'average deviation' = PROC_avg/100) %>% 
  pivot_longer(-c(Patient,time)) %>% 
  ggplot(aes(x = time,y = value,colour = name, group = name)) + 
  geom_point(size = 0.5, alpha = 0.5, na.rm = TRUE) + 
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


### THE HISTOGRAM


set.seed(111)
calibr <- sumar %>% count(Group) %>% mutate(m = 100/n) %>% pull(m)

boot_data <- rbind(
  filter(data, Group == "Anesthesia with surgery") %>% 
    group_by(Patient) %>% sample_n(size = 3333 * calibr[[1]], replace = TRUE),
  filter(data, Group == "Anesthesia without surgery") %>% 
    group_by(Patient) %>% sample_n(size = 3333 * calibr[[2]], replace = TRUE),
  filter(data, Group == "Sedation") %>% 
    group_by(Patient) %>%sample_n(size = 3333 * calibr[[3]], replace = TRUE))
rm(calibr)
  
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

