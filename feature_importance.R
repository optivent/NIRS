clean.it()
library(tidyverse); library(zoo); library(bestNormalize)
library(tidyr); library(feather); library(here)
setwd(here("inputs"))

stdize = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))} # min-max scaling
autonorm <- function(x, r, k) (bestNormalize(x, r, k, warn =F)$x.t)

######### IMPORT DATA ########################################################################

no_interpolation <- read_feather("manualtable.feather") %>% as_tibble() %>% 
  filter(!is.na(NIRS_proc_min)) %>% dplyr::select(-c(NIRS_min))
  
DataExplorer::plot_missing(no_interpolation %>% filter(Group != "Sedation"))   # missing anesthesie
DataExplorer::plot_histogram(no_interpolation %>% filter(Group != "Sedation")) # histogram anesthesie
DataExplorer::plot_missing(no_interpolation %>% filter(Group == "Sedation"))   # missing sedation
DataExplorer::plot_histogram(no_interpolation %>% filter(Group == "Sedation")) # histogram sedation
  
######### DRAW REPRESENTATIVE SAMPLES ########################################################

sensibility <- 2 # continous value between 1 and 3  # sensibility for the sampling
library("tidyimpute")
interpolation <- read_feather("manualtable.feather") %>%
  filter(!is.na(NIRS_proc_min)) %>% 
  group_by(Patient) %>%
   complete(
    timediff = seq(
      from = min(timediff),
      to   = max(timediff))
    ) %>% 
    mutate_at(vars(Group:Weight), list(~zoo::na.locf(.,na.rm = TRUE))) %>% 
    mutate_at(vars(SpO2:NIRS_min), list(~zoo::na.approx(., maxgap = 120, na.rm = FALSE))) %>% # i set the maxgap to 120 minutes to reduce the missing values
    impute_at(.na=mean,  .vars=6:12, na.rm = TRUE) %>% 
    mutate(
       diff_NIRSproc = replace_na(NIRS_proc_min - dplyr::lag(NIRS_proc_min),0),
       HF_proc = 100*(HF - first(HF))/first(HF),
       sampling_prob = replace_na(NIRS_proc_min^sensibility, 0)) %>% 
  ungroup() %>% as_tibble() %>%
  arrange(Patient, timediff) %>%
  mutate(NIRS_deviation = as.factor(ifelse(NIRS_proc_min < 0, "negative deviation", "positive deviation"))) 


set.seed(111)
first_and_last <- group_by(interpolation, Patient) %>% slice(c(1,n())) %>% ungroup() 
middle_samples <- group_by(interpolation, Patient) %>% sample_n(size = 8, replace = FALSE, weight = sampling_prob) %>% ungroup()
samples <- rbind(first_and_last, middle_samples) %>% arrange(Patient, timediff) %>%
  select(-c(sampling_prob, NIRS_min)) 
sample_anae <- samples %>% filter(Group != "Sedation")
sample_seda <- samples %>% filter(Group == "Sedation")
  
rm(first_and_last, middle_samples, samples, sensibility)
DataExplorer::plot_missing(sample_anae)
DataExplorer::plot_missing(sample_seda)

############ CORRELATION MATRICES ############################################################
library(corrplot)

sample_anae_cor <- sample_anae %>% select(-c(diff_NIRSproc)) %>% select_if(is.numeric) %>% cor()
diag(sample_anae_cor) <- NA
res1 <- cor.mtest(sample_anae %>% select_if(is.numeric), conf.level = .95)

sample_seda_cor <- sample_seda %>% select(-c(diff_NIRSproc, FiO2)) %>% select_if(is.numeric) %>% cor()
diag(sample_seda_cor ) <- NA
res2 <- cor.mtest(sample_seda %>% select_if(is.numeric), conf.level = .95)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

par(mfrow=c(1,1))

corrplot(sample_anae_cor, 
         type = "lower", order = "hclust", method = "color", 
         p.mat = res1$p, sig.level = 0.05, insig = "blank",
         diag = FALSE, tl.col = "black",tl.srt = 45,addCoef.col = "black",
         title = "Correlation Matrix in Anesthesia", mar=c(0,0,1,0))

corrplot(sample_seda_cor, 
         type = "lower", order = "hclust", method = "color", 
         p.mat = res1$p, sig.level = 0.05, insig = "blank",
         diag = FALSE, tl.col = "black",tl.srt = 45,addCoef.col = "black",
         title = "Correlation Matrix in Sedation", mar=c(0,0,1,0))


rm(sample_anae_cor, sample_seda_cor, res1, res2, col)


#### MUVR multilevel feature selection only the PLS, no multilevel in RF #####################
library(doParallel); nCore=detectCores()-1   # Number of processor threads to use
library(MUVR)

# no_interpolation_anae <- no_interpolation %>% filter(Group != "Sedation") %>% na.omit() %>% select(-timediff)
# no_interpolation_seda <- no_interpolation %>% filter(Group == "Sedation") %>% select(-c(CO2,timediff)) %>% na.omit()

nRep=nCore*10            # Number of MUVR repetitions
nOuter=7                # Number of outer cross-validation segments
varRatio=0.9            # Proportion of variables kept per iteration 
method='PLS'            # Selected core modelling algorithm


## start cluster
cl=makeCluster(nCore); registerDoParallel(cl)

  # regrModel.anae = MUVR(X=no_interpolation_anae  %>% select(Months:CO2) %>% as.matrix(),
  #                Y=no_interpolation_anae  %>% pull(NIRS_proc_min) %>% as.vector(),
  #                ID= no_interpolation_anae $Patient,
  #                nRep=nRep, nOuter=nOuter, varRatio=varRatio, method=method)
  
MUVR_sample_anae <- sample_anae %>% mutate_if(is.numeric, autonorm, r = 20, k = 5) 
regrModel.anae.sample <-MUVR(
    X= MUVR_sample_anae  %>% select(Months:CO2) %>% as.matrix(),
    Y=MUVR_sample_anae   %>% pull(NIRS_proc_min) %>% as.vector(),
    ID= MUVR_sample_anae$Patient,
    nRep=nRep, nOuter=nOuter, varRatio=varRatio, method=method)
  
  # regrModel.seda = MUVR(X=no_interpolation_seda  %>% select(Months:BP_mid) %>% as.matrix(),
  #                       Y=no_interpolation_seda %>% pull(NIRS_proc_min) %>% as.vector(),
  #                       ID= no_interpolation_seda$Patient,
  #                       nRep=nRep, nOuter=nOuter, varRatio=varRatio, method=method)

MUVR_sample_seda <- sample_seda %>% mutate_at(vars(Months:BP_mid, NIRS_proc_min), autonorm, r = 20, k = 5)
regrModel.seda.sample <-MUVR(
    X= MUVR_sample_seda %>% select(Months:BP_mid) %>% as.matrix(),
    Y= MUVR_sample_seda %>% pull(NIRS_proc_min) %>% as.vector(),
    ID= MUVR_sample_seda$Patient,
    nRep=nRep, nOuter=nOuter, varRatio=varRatio, method=method)
  
stopCluster(cl)
## end cluster 


regrModel.anae.sample$fitMetric
regrModel.seda.sample$fitMetric

#plotVAL(regrModel.anae)
#plotMV(regrModel.anae, model='max')
#plotStability(regrModel.anae, model='min')
par(mfrow=c(1,2))
# plotVIP(regrModel.anae, model='min')
# plotVIP(regrModel.anae.sample, model='min')
# plotVIP(regrModel.anae, model='med')
plotVIP(regrModel.anae.sample, model='min')

# plotVIP(regrModel.seda, model='min')
# plotVIP(regrModel.seda.sample, model='min')
# plotVIP(regrModel.seda, model='med')
plotVIP(regrModel.seda.sample, model='med')

par(mfrow=c(1,1)); 
rm(cl, method, nCore, nOuter, nRep, varRatio, MUVR_sample_anae, MUVR_sample_seda)
rm(regrModel.anae.sample, regrModel.seda.sample)

#### TREE MODELS PRIMARILY FOR THE CHILDREN UNDER ANESTHESIA #####################

library(rpart); library(rpart.plot)

sample_anae_tree <- rpart(NIRS_deviation ~ FiO2 + HF + SpO2,
                          control = list(maxdepth = 5),
                          data = sample_anae %>% filter(FiO2 > 22, Weight > 6), 
                          model = TRUE)
#plot(sample_anae_tree); text(sample_anae_tree)
prp(sample_anae_tree); par(mfrow=c(1,1))

sample_seda_tree <- rpart(NIRS_deviation ~ FiO2 + HF + SpO2,
                          control = list(maxdepth = 3),
                          data = sample_seda)
#plot(sample_seda_tree); text(sample_seda_tree)
prp(sample_seda_tree); par(mfrow=c(1,1))


sample <- rbind(sample_anae, sample_seda)
sample_tree <- rpart(NIRS_deviation ~ Group + Weight + SpO2 + HF + BP_sys + BP_dia + BP_mid,
                          control = list(maxdepth = 3),
                          data = sample)
plot(sample_tree); text(sample_tree)
prp(sample_tree)


library(C50)
c5tree <- C5.0(NIRS_deviation ~ Group + Weight + SpO2 + HF + BP_dia, 
               data = sample, 
               trials = 5,
               control = C5.0Control(winnow = TRUE))
summary(c5tree)
C5imp(c5tree)
plot(c5tree)

rm(c5tree, sample_anae_tree, sample_seda_tree)

#### MULTILEVEL BAYESIAN MODEL ###################################################

scaled_no_interpolation <- no_interpolation %>%
  select(Patient, Group:NIRS_proc_min) %>% 
  mutate_at(vars(Months:CO2), stdize, na.rm = T) 
  
library(parallel)
library(doParallel)
library(rstanarm)
options(mc.cores = parallel::detectCores()-2)

bayes_HLM <- stan_glmer(NIRS_proc_min ~ FiO2 + HF + (1|Patient),
                        family="gaussian",
                        data= no_interpolation,
                        iter = nrow(scaled_no_interpolation)*5, 
                        chains = parallel::detectCores()-2)

library(report)
bayes_HLM %>% report() %>% to_fulltable()
bayes_HLM %>% report(standardize="smart", effsize="cohen1988") %>% to_fulltext()


#### NON-Linear Model ############################################################

library("cgam") # constrained general additive models

ans <- ShapeSelect(NIRS_proc_min ~ shapes(FiO2, set = c("incr")) +
                     shapes(HF, set= c("incr")),
                   data = sample_anae %>% filter(HF < 160))




fit1 <- cgam(NIRS_proc_min ~ s.incr.incr(FiO2, HF, numknots = c(20, 20)), # manual generated
             data = sample_anae %>% filter(HF < 150))
plotpersp(fit1,  main = "3D Plot of a Smooth Cgam Fit1"); summary(fit1)

autofit_anae_FIO2_HF <- ShapeSelect(NIRS_proc_min ~ shapes(SpO2) + shapes(HF),     # computer generated 
                        data = sample_anae %>% filter(HF < 140), genetic = TRUE)


autofit_seda_SpO2_HF <- ShapeSelect(NIRS_proc_min ~ shapes(SpO2) + shapes(HF),     # computer generated 
                                    data = sample_seda %>% filter(HF < 140) %>% 
                                      select(SpO2, HF, NIRS_proc_min) %>% na.omit(),
                                    genetic = TRUE)



sample_anae$HF %>% DataExplorer::plot_histogram()
sample_anae$FiO2 %>% DataExplorer::plot_histogram()

#### ITSADUG #####################################################################

library(itsadug); library(mgcv)
# mod_gam <- mgcv::gam(NIRS_proc_min ~ s(HF) + s(FiO2) + te(Patient, timediff, bs = 're'),
#                      #correlation = corAR1(form = ~ time),
#                      data = anae_scal,
#                      method = "ML") 
# # method = "GCV.Cp"  te(ID, time, bs="fs", m=1, k = 5),
# # s(time) + s(ID, bs = 're'),
# summary(mod_gam)
# par(mfrow=c(2,2)); gam.check(mod_gam); par(mfrow=c(1,1))
# 
# fvisgam(mod_gam, view=c("HF", "FiO2"), rm.ranef=TRUE, main="fvisgam", dec=1)


# mod_gam_anae_HF_SPO2 <- mgcv::gam(NIRS_proc_min ~ te(HF, SpO2) +
#                         s(Patient, bs = "re"),
#                         #correlation = corAR1(form = ~ timediff),
#                         data = sample_anae %>% 
#                         filter(HF < 150, FiO2 < 90, HF > 70),
#                       method = "ML") 
# fvisgam(mod_gam_anae_HF_SPO2, view=c("HF", "SpO2"), rm.ranef=TRUE, main="fvisgam", dec=1)
# summary(mod_gam_anae_HF_SPO2)
# 
# mod_gam_seda_HF_SPO2 <- mgcv::gam(NIRS_proc_min ~ s(HF, k = 5) + s(SpO2, k = 5) +
#                                     s(Patient, bs = "re"),
#                                   #correlation = corAR1(form = ~ timediff),
#                                   data = no_ %>% 
#                                     filter(HF < 150),
#                                   method = "ML") 
# fvisgam(mod_gam_seda_HF_SPO2, view=c("HF", "SpO2"), rm.ranef=TRUE, main="fvisgam", dec=1)
# summary(mod_gam_seda_HF_SPO2)

##

# mod_gam_anae1 <- mgcv::gam(NIRS_proc_min ~ s(HF) + s(FiO2) + 
#                         te(Patient, timediff, bs="fs", m=1, k = 5),
#                      correlation = corAR1(form = ~ timediff),
#                      data = no_interpolation %>% 
#                        select(Patient, Group, timediff, HF, FiO2, NIRS_proc_min) %>% 
#                        na.omit() %>% filter(Group != "Sedation", HF < 140, FiO2 < 80),
#                      method = "ML") 
# summary(mod_gam_anae1)
# fvisgam(mod_gam_anae1, view=c("FiO2","HF"),
#         rm.ranef=FALSE, main="anaesthesia", dec=1)


mod_gam_anae2 <- mgcv::gam(NIRS_proc_min ~ te(FiO2,HF, bs="gp") + 
                             t2(Patient, timediff, bs="fs"),
                           correlation = corAR1(form = ~ timediff),
                           data = no_interpolation %>% 
                             select(Patient, Group, timediff, HF, FiO2, NIRS_proc_min) %>% 
                             na.omit() %>% filter(Group != "Sedation", HF < 140, FiO2 < 80),
                           method = "ML") 
summary(mod_gam_anae2)

par(mfrow=c(1,1))

fvisgam(mod_gam_anae2, view=c("FiO2","HF"), rm.ranef=FALSE, main="anaesthesia", dec=0,
        #plot.type = "persp",
        color = "topo",
        labcex = 1,
        hide.label = TRUE
        #nCol = 19
        )


plot(mod_gam_anae2, select=1, labcex = 1,shade=TRUE, rug=FALSE)






##



