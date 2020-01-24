
data50 <- readRDS("mastertable.rds")
demography <- read_excel("demography.xlsx")

names(data50) <- c("ID", "time", "SpO2", 
                   "L_NIRS", "L_NIRS_proc", "R_NIRS", "R_NIRS_proc", 
                   "HF", "RRSyst", "RRDia", "RRmid", "FIO2", "CO2",
                   "Minutevol", "Tidalvol", "RespRate", "Pinsp", "PEEP", 
                   "Temp", "Glucose", "Observ", "TYPE")

data50$ID %<>%  gsub("[^0-9.]", "", .) %>% as.numeric() 
demography$ID %<>%  gsub("[^0-9.]", "", .) %>% as.numeric() 
demography$Gender  %<>%  as.factor()

data50 %<>%  dplyr::inner_join(., demography, by = "ID"); rm(demography)
data50$ID %<>% as.numeric() %>% as.factor()

data50 %<>% dplyr::select(-c("Glucose","Observ","Temp","PEEP","Pinsp",
                             "Tidalvol","Minutevol","RespRate","Gender")) %>% 
  dplyr::filter(HF < 200) %>% 
  rowwise() %>% mutate( 
    min.NIRS = min(L_NIRS,R_NIRS),
    min.proc.NIRS = min(L_NIRS_proc, R_NIRS_proc)) %>% 
  select(-c(L_NIRS, R_NIRS, L_NIRS_proc, R_NIRS_proc))



anae <- data50 %>% filter(TYPE == 1) %>% select(-TYPE)
anae$ID  %<>% as.numeric() %>% as.factor()

anaePLS <- anae
stdize = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
anaePLS[,-c(1,13)] <- lapply(anaePLS[,-c(1,13)], stdize, na.rm = T)                      
anaePLS  %<>% select(-CO2) %>% na.omit()

# anae.interp <- data50.interp %>% filter(TYPE == 1) %>% select(-c(TYPE)) 
# anae.interp$ID  %<>% as.numeric() %>% as.factor() 

library(parallel)
library(doParallel)
library(rstanarm)
options(mc.cores =  parallel::detectCores()-1)

bayes_HLM <- stan_glmer(min.proc.NIRS ~ HF + FIO2 + Weight + Months + (1|ID),
                        data=anaePLS,
                        family="gaussian",
                        iter = 6000,
                        chains = 16)
library(report)
bayes_HLM %>% report() %>% to_fulltable()
bayes_HLM %>% report(standardize="smart", effsize="cohen1988") %>% to_fulltext()

library(itsadug); library(mgcv)
mod_gam <- mgcv::gam(min.proc.NIRS ~ s(HF) + s(FIO2) + s(ID, bs = 're'),
                     #correlation = corAR1(form = ~ time),
                     data = anae %>% filter(HF < 160),
                     method = "ML") 
# method = "GCV.Cp"  te(ID, time, bs="fs", m=1, k = 5),
# s(time) + s(ID, bs = 're'),
summary(mod_gam)
par(mfrow=c(2,2)); gam.check(mod_gam); par(mfrow=c(1,1))

fvisgam(mod_gam, view=c("HF", "FIO2"), rm.ranef=TRUE, main="fvisgam", dec=1)
