
library(tidyverse); library(tidyr)
library(here)
setwd(here("inputs"))
manualtable <- readRDS("manualtable.rds") %>% 
  filter(
    between(HF, 50, 200), 
    between(SpO2, 50, 101),
    between(FiO2, 19, 101)
  ) %>% select(-c(Comment, time, Gender)) %>% 
  select(-matches("baseline")) %>% 
  filter_at(vars(L_proc, R_proc), any_vars(!is.na(.))) %>% 
  mutate(NIRS_proc_min = pmin(L_proc, R_proc, na.rm = TRUE)) %>% select(-c(L_proc, R_proc)) %>% 
  mutate(NIRS_min  = pmin(NIRS_li, NIRS_re, na.rm = TRUE)) %>% select(-c(NIRS_li, NIRS_re)) %>% 
  group_by(Patient) %>% 
    mutate(diff_NIRSproc = replace_na(NIRS_proc_min - lag(NIRS_proc_min),0),
           HF_proc = 100*(HF - first(HF))/first(HF),
           sampling_prob = (NIRS_proc_min*diff_NIRSproc)^2/100) %>% 
  ungroup() %>% as_tibble()


manualtable %>% group_by(Patient) %>%
    arrange(timediff) %>% slice(c(1,n())) %>% 
  ungroup() ->
first_and_last

set.seed(111)
manualtable %>% group_by(Patient) %>% 
    sample_n(size = 5, replace = FALSE, weight = sampling_prob) %>% 
  ungroup() ->
middle_samples


samples <- rbind(first_and_last, middle_samples) %>% arrange(Patient, timediff) %>% select(-sampling_prob)

rm(first_and_last, middle_samples)

samples %>% DataExplorer::plot_missing()


df_for_Boruta <- samples %>% select(-c(NIRS_min, CO2, diff_NIRSproc, timediff)) %>% select(-matches("BP_")) %>% na.omit()


library(Boruta)
set.seed(111)
plot(Boruta(NIRS_proc_min ~ ., data = df_for_Boruta, doTrace = 2), cex.axis=.7, las=2, xlab="", main="Variable Importance")

library(report)
library(parallel)
library(doParallel)
library(rstanarm)
cores = parallel::detectCores()-1
options(mc.cores = cores)

anae <- samples %>% filter(Group != "Sedation") %>%
  select(Patient, Weight, HF, BP_mid, BP_dia, FiO2, NIRS_proc_min) %>% na.omit()

linear_model <- stan_glmer(NIRS_proc_min ~ FiO2 + Weight + HF + BP_mid + BP_dia + (1|Patient), 
           data = anae ,
           family="gaussian",
           iter = 2000,
           chains = 16)

options(mc.cores = cores)
bayes_HLM <- stan_glmer(NIRS_proc_min ~ FiO2 + Weight + HF_proc + BP_mid + BP_dia + (1|Patient), 
                        data = anae ,
                        family="gaussian",
                        iter = 2000,
                        chains = 14)
bayes_HLM %>% report() %>% to_fulltable()
bayes_HLM %>% report(standardize="smart", effsize="cohen1988") %>% to_fulltext()


df_for_gamm <- manualtable %>% filter(Group != "Sedation") %>% select(Patient, FiO2, HF, NIRS_proc_min, timediff)
library(itsadug); library(mgcv)
mod_gam <- mgcv::gam(NIRS_proc_min ~ s(HF) + s(FiO2) + te(Patient, timediff, bs = 'fs'),
                     correlation = corAR1(form = ~ timediff),
                     data = df_for_gamm %>% filter(HF < 150),
                     method = "ML") 
# method = "GCV.Cp"  te(ID, time, bs="fs", m=1, k = 5),
# s(time) + s(ID, bs = 're'),
summary(mod_gam)
par(mfrow=c(2,2)); gam.check(mod_gam); par(mfrow=c(1,1))

fvisgam(mod_gam, view=c("HF", "FiO2"), rm.ranef=TRUE, main="fvisgam", dec=1)





