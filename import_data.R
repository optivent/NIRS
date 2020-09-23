## GitHub Sync setup
# library(usethis)
# ?use_github
# the Github code: is found in notepad++
# edit_r_environ()  GITHUB_PAT = 'found in notepad++'
# restart session: library(usethis)
# a new github directory is necessary
# use_github(protocol = 'https', auth_token = Sys.getenv("GITHUB_PAT"))

## Clean the environmend
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
  if(!require(pacman))install.packages("pacman")
  pacman::p_load(tidyverse,magrittr,stringi,readxl,fs,feather,
                 chron,lubridate,tsibble,zoo,here)
}
clean.it() # clean the environment and unload and reload the libraries

# not-in-list function
"%ni%" <- Negate("%in%")


# change working directory to the RowData
setwd(here("inputs/raw_data_NIRS1"))

# read the first set of data
list.files(pattern = NULL) %>% 
  map_df(
    ~read_delim(.,                                 # from the readr (tidyverse)
                delim =  " ",                      # use " " as delim
                col_names = FALSE,                 # no header
                col_types = cols(.default = "c")   # perform no transformation, all input as char.
    )
  ) %>% 
  select(-c(26:ncol(.))) %>%                            # remove unrelevant columns
  mutate_all(
    function(x) {stri_replace_all_fixed(x, " ", "")}
  ) %>%  #  trim whitespaces
  transmute(
    date = X2, time = X3,                               # select and rename columns
    L_NIRS = X5,  L_Event = X6,  L_Status = X7,  L_BaseLine = X8,  L_Alarm = X11, 
    R_NIRS = X16, R_Event = X17, R_Status = X18, R_BaseLine = X19, R_Alarm = X22 
  )  ->
  data1                                                 ## 150256 obs of 12 variables

# load the path with unread dates ()
setwd(here("inputs/raw_data_NIRS2"))

list.files(pattern = NULL) %>% 
  map_df(
    ~read_delim(.,                                 # from the readr (tidyverse)
                delim =  " ",                      # use " " as delim
                col_names = FALSE,                 # no header
                col_types = cols(.default = "c")   # perform no transformation, all input as char.
    )
  ) %>% 
  select(
    -c(26:ncol(.))
  ) %>%                            # remove unrelevant columns
  mutate_all(
    function(x) {stri_replace_all_fixed(x, " ", "")}
  ) %>%  #  trim whitespaces
  transmute(
    date = X2, time = X3,                               # select and rename columns
    L_NIRS = X5,  L_Event = X6,  L_Status = X7,  L_BaseLine = X8,  L_Alarm = X11, 
    R_NIRS = X16, R_Event = X17, R_Status = X18, R_BaseLine = X19, R_Alarm = X22 
  )  ->
  data2                                                    # 6477 obs of 12 variables

# the columns in both data1 and data2 are matching
data <- rbind(data1, data2)                            # 156733 obs. of 12 variables
rm(data1, data2)

data$time     <- chron(times = data$time)               # use "chron" library to format the time

data$date     <- as.Date(format(as.Date(data$date, "%d.%m.%y"),"20%y-%m-%d"))  # formats the year in the 20xx form

data$datetime <- as.POSIXct(paste(data$date, data$time), format="%Y-%m-%d %H:%M:%S") # one col for datetime

data %<>% select(-c(date,time))

data %<>% distinct() %>% arrange(datetime)  # 149151 rows without duplicates

data$oid <- 1:nrow(data)                    # keep track of original id-rows 

data %<>% mutate_at(vars(matches('Event|Status|NIRS|Baseline|Alarm')),as.integer)

data %<>% mutate(BL_marker = ifelse(L_Event %in% c(1,2) | R_Event %in% c(1,2), "baseline", "~") %>% as.factor)

data %<>% mutate_at(vars(L_NIRS, R_NIRS), ~ifelse(between(.,0,15), NA, .)) # replace the NIRS range 0-15 with NA

data %<>% filter_at(vars(L_NIRS, R_NIRS), any_vars(!is.na(.)))             # remove rows where both NIRS are NA, 141318 obs.

data %<>% mutate_at(vars(L_Status, R_Status), ~ifelse(between(.,4,9), ., 0))

data %<>% mutate_at(vars(matches('Status')),factor)                        # Factor Status (from the remaining data)

# 141318 observations of 13 variables

# preserve only datetime and R_NIRS and L_NIRS
data %<>% select(-(matches('Baseline|Status|Alarm|BL_marker|Event')))


setwd(here("inputs"))
# import IID-Table 
IIDKey <- read_excel("IIDkey.xlsx",
                     col_types = c("numeric", "text", "date", "date", 
                                   "numeric", "text", "numeric", "numeric",
                                   "numeric", "numeric")) %>% 
  group_by(Date = as.Date(Start)) %>% 
  mutate(uniq_Date = ifelse(n()==1, T, F)) %>% 
  ungroup() %>% 
  mutate_at(vars(Start,Stop), hms::as_hms) %>% 
  rename(cut_Start = Start, cut_Stop = Stop) %>% 
  filter(Patient != 58)

## plot the dates where are two children per day
#  data %>% filter(as.Date(datetime) %in% unique(filter(IIDKey, uniq_Date != TRUE)$Date)) %>%
#           melt( id = "datetime", measure.vars = c("L_NIRS","R_NIRS")) %>%
#             ggplot(aes(x = datetime, y = value, colour = variable, group = variable)) +
#               geom_point(size = 0.5, alpha = 0.5, na.rm = TRUE) +
#                 theme_bw() +
#                 facet_grid(. ~ as.Date(datetime), scales="free")


data %<>% mutate(Date = as.Date(datetime), time = hms::as_hms(datetime)) %>% select(-datetime)

data <- full_join(IIDKey, data, by = "Date") %>% 
  select(Patient, Group, Date, cut_Start, time, cut_Stop, everything())

data %<>% group_by(Patient) %>% 
  filter(cut_Start < time & time < cut_Stop) %>% 
  ungroup()

data %<>% filter(oid %ni% c(5494:5630, 75273:75433))

setwd(here("inputs/"))

data <- rbind(
 filter(data, Group == "S") %>% mutate(L_NIRS = L_NIRS -2, R_NIRS = R_NIRS -2),
 filter(data, Group != "S") %>% mutate(L_NIRS = L_NIRS + 0, R_NIRS = R_NIRS +0)
)

##########################################

data %<>% mutate(L_proc = 100*(L_NIRS - Lbaseline)/Lbaseline, R_proc = 100*(R_NIRS - Rbaseline)/Rbaseline )

data$datetime <- as.POSIXct(paste(data$Date, data$time), format="%Y-%m-%d %H:%M:%S")

data %<>% filter(Patient != 58) # Patient 58 has no Baseline

data %<>% group_by(Patient) %>%
  complete(
    datetime = seq(
      from = min(datetime),
      to   = max(datetime),
      by = "sec")
  ) %>%
  ungroup() %>% 
  select(Patient, Group:cut_Stop, 
         L_NIRS, Lbaseline, L_proc, R_NIRS, Rbaseline, R_proc,
         Fallnr:Weight, datetime, oid) 

data %<>% group_by(Patient) %>%
  mutate_at(vars(L_NIRS, L_proc, R_NIRS, R_proc), list(~na.approx(., maxgap = 15, na.rm = FALSE))) %>% 
  mutate_at(vars(Group, Fallnr:Weight), list(~na.locf(.,na.rm = TRUE))) %>%
  ungroup()

data <- group_by(data, Patient) %>% 
  mutate(
    Gender = ifelse(Gender == "M","boy","girl"),
    interp_ID = dplyr::row_number(),
    minutes   = difftime(datetime,first(datetime),units="mins")
  ) %>%
  ungroup() %>% 
  mutate(
    NIRS_min = pmin(L_NIRS,R_NIRS, na.rm = TRUE),
    NIRS_avg = rowMeans(select(., ends_with("NIRS")), na.rm = TRUE),
    PROC_min = pmin(L_proc, R_proc, na.rm = TRUE),
    PROC_avg = rowMeans(select(., ends_with("_proc")), na.rm = TRUE),
    alarm_min = ifelse(NIRS_min < 50 | PROC_min < -20, 1, 0),
    alarm_avg = ifelse(NIRS_avg < 50 | PROC_avg < -20, 1, 0),
    under_BL = ifelse(NIRS_min < 50 | PROC_min < 0, 1, 0)
  ) %>% 
  select(-c(Date:cut_Stop, oid)) %>% 
  select(Group, Patient, datetime, interp_ID, Fallnr, Gender, Months, Weight, everything()) 

data$Gender %<>% as.factor()
data$Patient %<>% as.factor()
data$Group <- case_when(
                data$Group == "I" ~ "Anesthesia with surgery",
                data$Group == "I_NO_OP" ~ "Anesthesia without surgery",
                data$Group == "S" ~ "Sedation",
                TRUE ~ as.character(data$Group)) %>% 
              as.factor()

saveRDS(data, "NIRS_interpolated.rds")

##################################################

## IMPORT TABLE DATA

setwd(here("inputs"))

IIDKey <- read_excel("IIDkey.xlsx",
                     col_types = c("numeric", "text", "date", "date", "numeric", "text",
                                   rep("numeric", 4))) %>% 
  group_by(Date = as.Date(Start)) %>% 
  mutate(uniq_Date = ifelse(n()==1, T, F)) %>% 
  ungroup() %>% 
  mutate_at(vars(Start,Stop), hms::as_hms) %>% 
  rename(cut_Start = Start, cut_Stop = Stop) %>% 
  filter(Patient != 58)


path <- "manual_table.xlsx"

manualtable <- path %>% excel_sheets() %>% 
        purrr::set_names() %>% 
        map_df(~ read_excel
                (path = path,sheet = .x, 
                 col_types = "text"
                ), 
          .id = "Sheet") %>% 
        select(Sheet:SpO2,matches('NIRS'),HF,matches('RR|O2'),Ereignis) %>% 
        mutate(Patient = Sheet %>% gsub("[^0-9.]", "", .) %>% as.integer()) %>% 
        select(Patient, everything()) %>% select(-c(Sheet:ID)) %>% 
        rename(time = Uhrzeit,timediff = `Abs Zeit minuten`,
          NIRS_li = "NIRS links",NIRS_re = "NIRS rechts",
          FiO2 = "FiO2 (%)", CO2 = "etCO2 (mmHg)",
          BP_sys = "RR syst", BP_mid = "RR mittel", BP_dia = "RR diast", Comment = Ereignis) %>% 
        mutate_at(vars(time:CO2),as.numeric) %>% 
        mutate(time = chron::times(time)) %>% 
        filter(Patient != 58) %>% 
        right_join(
          IIDKey %>% select(Patient, Group, Gender:Rbaseline)) %>% 
        mutate(L_proc = 100*(NIRS_li - Lbaseline)/Lbaseline,
               R_proc = 100*(NIRS_re - Rbaseline)/Rbaseline) %>% 
        select(Group, Patient, Gender:Weight, time:SpO2, HF:Comment, # the X's
               Lbaseline, NIRS_li, L_proc, Rbaseline, NIRS_re, R_proc) %>% # the Y's
        filter(between(HF, 50, 200), between(SpO2, 50, 101), between(FiO2, 19, 101)) %>%
        select(-c(Comment, time, Gender)) %>% select(-matches("baseline")) %>% 
        filter_at(vars(L_proc, R_proc), any_vars(!is.na(.))) %>% 
        mutate(NIRS_proc_min = pmin(L_proc, R_proc, na.rm = TRUE)) %>% select(-c(L_proc, R_proc)) %>% 
        mutate(NIRS_min  = pmin(NIRS_li, NIRS_re, na.rm = TRUE)) %>% select(-c(NIRS_li, NIRS_re))

rm(path, IIDKey)


manualtable$Group <- case_when(
  manualtable$Group == "I" ~ "Anesthesia with surgery",
  manualtable$Group == "I_NO_OP" ~ "Anesthesia without surgery",
  manualtable$Group == "S" ~ "Sedation",
  TRUE ~ as.character(manualtable$Group)) %>% 
  as.factor()
manualtable$Patient %<>% as.factor()


library(feather)
write_feather(manualtable, "manualtable.feather")

########### inter-group statistics #########

chunk1 <- group_by(select(data, -c(datetime:Fallnr)), Group, Patient) %>%
  summarise(across(.cols = c(Gender:Weight,NIRS_min:PROC_avg), ~ first(.))) %>% 
ungroup() %>% mutate(Patient = as.character(Patient))

chunk2 <- select(IIDKey, Patient, Lbaseline, Rbaseline) %>% mutate(Patient = as.character(Patient))

library(readxl)
chunk3 <- mutate_all(read_excel(here("inputs/some_scores.xlsx"))[1:3],extract_numeric) %>% 
  transmute(Patient = as.character(ID), WSS)

vitals <- manualtable %>%
  rename_with(.fn= str_remove_all, "BP_", .cols = matches("BP")) %>% 
  rename(SBP = sys, DBP = dia, MBP = mid, HR = HF) %>% 
  relocate(MBP, .before = "DBP") 

vitals <- vitals %>% group_by(Group,Patient) %>%
  summarise(across(SpO2:CO2, median, na.rm = TRUE)) %>%
  mutate_if(is.numeric, ~ifelse(abs(.) == Inf,NA,round(.,0))) %>% 
  mutate(Patient = as.character(Patient)) %>% 
  mutate(across(matches("SpO2|Fi"), ~ ./100)) 

summary <- list(chunk1,chunk2,chunk3) %>%
  reduce(left_join, by = "Patient") %>%
  rowwise() %>% 
    mutate(BL = floor((Lbaseline + Rbaseline)/2), .after = Weight) %>% 
    mutate(across(matches("Weight|PROC"), round, 0)) %>% 
    mutate(across(matches("PROC"), ~ ./100)) %>% 
  ungroup() %>% 
  select(Group:Weight, WSS, BL) %>% 
  mutate(WSS = replace_na(WSS,3)) %>% 
  rename(gender = Gender, age = Months, weight = Weight) %>% 
  left_join(.,vitals) %>% 
  filter(Patient != "54") %>% select(Group:BL, matches("O2"), HR:DBP)

summary %>% group_by(Group) %>% 
  gt(rowname_col = "Patient") %>% 
  tab_header(title = "Supplementary Table II") %>%
  cols_align(align = "center") %>% 
  fmt_percent(matches("SpO|FiO"), decimals = 0) %>% 
  tab_source_note(md(
    "Age in months, weight in kilogram, WSS = Wilson Sedation Scale, BL = NIRS baseline,  
    SpO2 = pulsoximetry, FiO2 = fraction of inspired oxygen, CO2 = expired carbon dioxide (mmHg)  
    HR = heart rate, SBP/MBP/DBP = systolic/median/diastolic blood pressure (mmHg)"))

med_iqr <- function(x){
  round(
    quantile(x, probs = c(0.25,0.5,0.75),
               na.rm = TRUE, names = FALSE)
  ) %>%paste(collapse = "|")
}

summary %>% select(Group,age:DBP) %>%
  mutate(across(.cols = c(SpO2:FiO2), ~ .*100)) %>% 
  group_by(Group) %>%
  summarise(across(where(is.double), .fns = med_iqr)) %>% 
  group_by(Group) %>% 
  gt() %>% 
  tab_header(title = "The 1st | 2nd | 3rd quartile of the studied variables per group") %>%
  cols_align(align = "center") %>% 
  tab_source_note(md(
    "Age in months, weight in kilogram, WSS = Wilson Sedation Scale, BL = NIRS baseline,  
    SpO2 = pulsoximetry, FiO2 = fraction of inspired oxygen, CO2 = expired carbon dioxide (mmHg),   
    HR = heart rate, SBP/MBP/DBP = systolic/median/diastolic blood pressure (mmHg)"))

library(dunn.test)
summary %>%
  select(age:DBP) %>% 
  map_dfr(~ dunn.test(.x,g = summary$Group,method = "bonferroni") %>%
            as_tibble() %>% select(4:5) %>% 
            pivot_wider(names_from = comparisons, values_from = P.adjusted), 
          .id = "variable") %>% 
  mutate(across(where(is.double), ~ round(.x, digits = 3))) %>% 
  pivot_longer(cols = -variable) %>% rename(groups = 2, adj_p_val = 3) %>% 
  pivot_wider(names_from = variable, values_from = adj_p_val) %>% 
  group_by(groups) %>%
  gt() %>% 
  cols_align(align = "center") %>% 
  tab_header(title = "p-values of the Dunn's test across groups") %>% 
  tab_source_note(md(
    "using Bonferroni correction, the Ho can be rejected if p < alpha/2 (values < 0.025),  
    Age in months, weight in kilogram, WSS = Wilson Sedation Scale, BL = NIRS baseline,  
    SpO2 = pulsoximetry, FiO2 = fraction of inspired oxygen, CO2 = expired carbon dioxide (mmHg),   
    HR = heart rate, SBP/MBP/DBP = systolic/median/diastolic blood pressure (mmHg)
    "))

library(forcats)
summary %>% select(Group, age:DBP, -WSS) %>%
  mutate(across(where(is.double), scales::rescale)) %>% 
  pivot_longer(cols = -Group) %>% 
  ggplot(aes(x = value, fill = Group, color = Group)) +
    geom_histogram(bins = 20, alpha = 0.33) +
    geom_density(alpha = 0.33) +
    facet_grid(name ~ Group, scales = "free",  switch="y") + 
    theme(
      legend.position="none", 
      axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),
      plot.caption = element_text(hjust = .5), plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = rel(0.9))
      ) +
  labs(title = "Histogram per variable(horizontal) and group(vertical)",
       subtitle = "all variables are scaled between their global minimum (left) and maximum (right)",
       caption = "
       BL = NIRS baseline, SpO2 = pulsoximetry, FiO2 = fraction of inspired oxygen, CO2 = expired carbon dioxide,
       HR = heart rate, SBP/MBP/DBP = systolic/median/diastolic blood pressure")
  
med_iqr <- function(x){
  round(
    quantile(x, probs = c(0.25,0.5,0.75),
             na.rm = TRUE, names = FALSE)
  ) %>% paste(collapse = " | ")
}

manualtable %>%
  select(Group, Patient, matches("BP")) %>%
  group_by(Group, Patient) %>%
  summarise(across(matches("BP"), med_iqr)) %>% 
  rename(systolic_BP = 3, diastolic_BP = 4, mean_BP = 5) %>% 
  relocate(mean_BP, .after = systolic_BP) %>% 
  group_by(Group) %>% 
  gt(rowname_col = "Patient") %>% 
  cols_align(align = "center") %>% 
  tab_header(title = md("The 1st, 2nd, and 3rd quartile of blood pressure")) %>% 
  tab_source_note(md(
    "the first column from links is patient's ID,  
    the values xx | xx | xx encodes the percentile 25,50,and 75, respectively for each patient
    "))

rm(med_iqr)


percentBP <- function(x){
  x <- na.omit(x)
  first <- first(x)
  min <- min(x)
  proc <- as.integer( 100*min( (x-first)/first )  )
  result <- paste0("[",first,"] ",min,"mmHg / ", proc, "%")
  return(result)
}

manualtable %>%
  select(Group, Patient, matches("BP")) %>% na.omit() %>% 
  group_by(Group, Patient) %>%
  summarise(across(matches("BP"), percentBP)) %>% 
  rename(systolic_press = 3, diastolic_press = 4, mean_press = 5) %>% 
  relocate(mean_press, .after = systolic_press) %>% 
  group_by(Group) %>% gt(rowname_col = "Patient") %>% 
  cols_align(align = "center") %>% 
  tab_header(title = md("Baseline blood pressure and the lowest value per patient")) %>% 
  tab_source_note(md(
    "the first column from links is patient's ID,  
    [xx] is the baseline blood pressure in mmHg,  
    the second value represents the lowest value per patient,  
    xx% shows the lowest percentual drop in blood pressure compared to the baseline
    "))


