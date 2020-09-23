clean.it()
library(tidyverse); library(zoo); library(bestNormalize)
library(tidyr); library(feather); library(here)
setwd(here("inputs"))



stdize = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
autonorm <- function(x, r, k) (bestNormalize(x, r, k, warn =F)$x.t)

no_interpolation <- read_feather("manualtable.feather") %>% 
  as_tibble() %>% 
  filter(!is.na(NIRS_proc_min)) %>% dplyr::select(-c(NIRS_min, timediff)) 

anae <- no_interpolation %>% filter(Group != "Sedation") %>% dplyr::select(-Group) %>% 
  mutate_if(is.numeric, stdize, na.rm = TRUE) 
DataExplorer::plot_histogram(anae)
  
seda <- select(anti_join(no_interpolation, anae), -Group) %>% mutate_if(is.numeric, stdize, na.rm = TRUE)  
DataExplorer::plot_histogram(seda)






 
sensibility <- 2 # continous value between 1 and 3  # sensibility for the sampling

manualtable <- read_feather("manualtable.feather") %>% 
  group_by(Patient) %>%
   complete(
    timediff = seq(
      from = min(timediff),
      to   = max(timediff))) %>% 
    mutate_at(vars(Group:Weight),
              list(~zoo::na.locf(.,na.rm = TRUE))) %>% 
    mutate_at(vars(SpO2:NIRS_min),
          list(~zoo::na.approx(., maxgap = 5, na.rm = FALSE))) %>% 
    mutate(
       diff_NIRSproc = replace_na(NIRS_proc_min - dplyr::lag(NIRS_proc_min),0),
       HF_proc = 100*(HF - first(HF))/first(HF),
       sampling_prob = replace_na(NIRS_proc_min^sensibility, 0)) %>% 
    mutate_at(vars(sampling_prob), stdize, na.rm = T) %>% 
  ungroup() %>% as_tibble() %>% 
  filter(!is.na(NIRS_proc_min)) %>% 
  mutate(NIRS_deviation = as.factor(ifelse(NIRS_proc_min < 0, "negative", "positive")))

first_and_last <- manualtable %>%
  group_by(Patient) %>%
    arrange(timediff) %>% slice(c(1,n())) %>% 
  ungroup() 

set.seed(111)
middle_samples <- manualtable %>% 
  group_by(Patient) %>% 
    sample_n(size = 8, replace = FALSE, weight = sampling_prob) %>% 
  ungroup()

samples <- rbind(first_and_last, middle_samples) %>% arrange(Patient, timediff) %>%
   select(-c(timediff, NIRS_min, diff_NIRSproc, sampling_prob))

rm(first_and_last, middle_samples)

#manualtable %>% group_by(Group, NIRS_deviation) %>% count()

sample_anae_scaled <- samples %>% filter(Group != "Sedation") %>% 
  dplyr::select(-NIRS_deviation) %>% 
  mutate_if(is.numeric, autonorm, r = 10, k = 10)

sample_seda <- samples %>% filter(Group == "Sedation")

# library(corrplot)
# # 
# cor_matrix <- cor.mtest(sample_anae, conf.level = .95)
# # 
# corrplot(cor_matrix, p.mat = cor_matrix$p, sig.level = .2,
#          type = "lower",  method = "number",
#          na.label = "●",
#          tl.col = "black",tl.srt = 45,
#          order = "hclust")

# library(Boruta)
# set.seed(111)
# plot(Boruta(NIRS_deviation ~ ., 
#             data = na.omit(sample_anae ), 
#             doTrace = 2), cex.axis=.7, las=2, xlab="", main="Variable Importance")
# 
# plot(Boruta(NIRS_deviation ~ ., 
#             data = sample_seda %>% select(-c(CO2, BP_sys:BP_mid)), doTrace = 2),
#      cex.axis=.7, las=2, xlab="", main="Variable Importance")
############################### MUVR ######################


library(rpart); library(rpart.plot)
# sample_tree <- rpart(NIRS_deviation ~ ., control = list(maxdepth = 4), data = samples)
# plot(sample_tree); text(sample_tree)
# prp(sample_tree)

sample_anae_tree <- rpart(NIRS_deviation ~ ., control = list(maxdepth = 3),
                          data = sample_anae %>% dplyr::select(-HF_proc), 
                          model = TRUE)
plot(sample_anae_tree); text(sample_anae_tree)
prp(sample_anae_tree)

table(predict(sample_anae_tree, type="class"), sample_anae$NIRS_deviation)



sample_seda_tree <- rpart(NIRS_deviation ~ ., control = list(maxdepth = 3), data = sample_seda)
plot(sample_seda_tree); text(sample_seda_tree)
prp(sample_seda_tree)


library(C50)
c5tree <- C5.0(NIRS_deviation ~ ., 
               data = sample_anae %>% dplyr::select(-c(HF_proc,Group)), 
               trials = 50,
               control = C5.0Control(winnow = TRUE))
summary(c5tree)
C5imp(c5tree)
plot(c5tree)



# next feature importance based on multilevel bayesian models

scaled_table <- manualtable %>% select(Patient, Group:NIRS_proc_min) %>% 
  mutate_at(vars(Months:CO2), stdize, na.rm = T)

anae_scal <- scaled_table %>% filter(Group != "Sedation") %>% select(-Group)
seda_scal <- scaled_table %>% filter(Group == "Sedation") %>% select(-Group)
rm(scaled_table)

library(parallel)
library(doParallel)
library(rstanarm)
options(mc.cores = parallel::detectCores()-1)

bayes_HLM <- stan_glmer(NIRS_proc_min ~  BP_dia + 
                          (1|Patient),
                        data=anae_scal, family="gaussian", iter = 5000, chains = 6)

library(report)
bayes_HLM %>% report() %>% to_fulltable()
bayes_HLM %>% report(standardize="smart", effsize="cohen1988") %>% to_fulltext()

library(itsadug); library(mgcv)
mod_gam <- mgcv::gam(NIRS_proc_min ~ s(HF) + s(FiO2) + te(Patient, timediff, bs = 're'),
                     #correlation = corAR1(form = ~ time),
                     data = manualtable %>% filter(Group != "Sedation"),
                     method = "ML") 
# method = "GCV.Cp"  te(ID, time, bs="fs", m=1, k = 5),
# s(time) + s(ID, bs = 're'),
summary(mod_gam)
par(mfrow=c(2,2)); gam.check(mod_gam); par(mfrow=c(1,1))

fvisgam(mod_gam, view=c("HF", "FiO2"), rm.ranef=TRUE, main="fvisgam", dec=1)

#################

sample_anae <- samples %>% filter(Group != "Sedation")

library("cgam") # constrained general additive models
autofit_anae_FIO2_HF <- ShapeSelect(NIRS_proc_min ~ shapes(FiO2) + shapes(HF),     # computer generated
                                    data = sample_anae %>% filter(HF < 150), genetic = TRUE)
