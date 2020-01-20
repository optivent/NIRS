
library(tidyverse); library(tidyr); library(feather)
library(here)
setwd(here("inputs"))

manualtable <- read_feather("manualtable.feather") %>% 
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
rm(df_for_Boruta)

# MUVR is in the Arhive 

# next feature importance based on multilevel bayesian models

stdize = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

scaled_table <- manualtable %>% select(-c(NIRS_min:sampling_prob)) %>% mutate_at(vars(Months:CO2), stdize, na.rm = T)

anae_scal <- scaled_table %>% filter(Group != "Sedation") %>% select(-Group)
seda_scal <- scaled_table %>% filter(Group == "Sedation") %>% select(-Group)
rm(scaled_table)

library(parallel)
library(doParallel)
library(rstanarm)
options(mc.cores = parallel::detectCores()-1)

bayes_HLM <- stan_glmer(min.proc.NIRS ~ HF + FIO2 + Weight + Months + (1|ID),
                        data=anae_scal,
                        family="gaussian",
                        iter = 6000,
                        chains = 14)
library(report)
bayes_HLM %>% report() %>% to_fulltable()
bayes_HLM %>% report(standardize="smart", effsize="cohen1988") %>% to_fulltext()

library(itsadug); library(mgcv)
mod_gam <- mgcv::gam(min.proc.NIRS ~ s(HF) + s(FIO2) + s(ID, bs = 're'),
                     correlation = corAR1(form = ~ time),
                     data = anae %>% filter(HF < 160),
                     method = "ML") 
# method = "GCV.Cp"  te(ID, time, bs="fs", m=1, k = 5),
# s(time) + s(ID, bs = 're'),
summary(mod_gam)
par(mfrow=c(2,2)); gam.check(mod_gam); par(mfrow=c(1,1))

fvisgam(mod_gam, view=c("HF", "FIO2"), rm.ranef=TRUE, main="fvisgam", dec=1)
