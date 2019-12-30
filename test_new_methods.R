clean.it()

############## QWRAPS LIBRARY ############################################
library(qwraps2); options(qwraps2_markup = "markdown")

data("mtcars")

mtcars2 <- mtcars %>%
  mutate(
  cyl_factor = factor(cyl, levels = c(6,4,8), labels = paste(c(6,4,8), "cylinders")),
  cyl_character = paste(cyl, "cylinders")
  )
rm(mtcars)

#qwraps2::mean_sd(mtcars2$mpg)
#qwraps2::mean_sd(mtcars2$mpg, denote_sd = "paren")
#print(mean_ci(mtcars2$mpg), show_level = TRUE)
n_perc(sumar$Gender == "boy")



mtcars2 %>% group_by(cyl_factor) %>% summary_table(.)
mtcars2 %>% qsummary(.)

dt <- mtcars2
summary_table(group_by(dt, cyl_factor), dplyr::select(dt, .data$mpg, .data$cyl_factor, .data$wt) %>%
                qsummary(.,
                  numeric_summaries = list("Median (1st Q., 3rd Q.)" = "~ qwraps2::median_iqr(%s)"),
                  n_perc_args = list(digits = 1, show_symbol = TRUE, show_denom = "always"))
              )




sumar %>% select(Group:`Duration (minutes)`) %>% dplyr::group_by(Group) %>%
  summary_table(., 
      qsummary(.,
        numeric_summaries = list(
           "Minimum"   = "~ min(%s)",
           "Median (1st Q., 3rd Q. "= " ~ qwraps2::median_iqr(%s)",
           "Maximum"   = "~ max(%s)"),
        n_perc_args = list(
           digits = 1,
           show_symbol = TRUE,
           show_denom = "always")
      )
  )



list(
  .vars = lst(
        c("months","weight","minutes","%dev. Q25","%dev. median","%dev. Q75","max. consec.(s)"),
        "alarms (seconds)",
        "alarms (seconds)",
        "alarms (seconds)"),               # the list of lists 
  .funs = lst(
        qwraps2::median_iqr,
        min,
        mean,
        max)
  ) %>% # the first list is associated with a function 
pmap(~ sumar %>% na.omit() %>% group_by(Group) %>% summarise_at(.x, .y)) %>% reduce(inner_join, by = "Group") %>% # grouped map reduce 
t() %>% as_tibble(rownames = " ") %>% setNames(as.character(.[1,])) %>% slice(-1) %>% # transpose the matrix
purrr::modify_if(~is.numeric(.), ~round(., 1)) %>% # decimal places
rename(" " = Group) %>% 
kable(align = "c", "html", booktabs = T) %>%
kable_styling(bootstrap_options = c("striped","condensed"), full_width = F, position = "center")

######################################################################
library(psych)
describeBy(sumar %>% select(`Age (months)`, Weight, Gender, `Duration (minutes)`),
           group = sumar$Group, mat = TRUE, digits = 2) %>% View()




#######################################################################

library(compareGroups)
data(predimed)

predimed$tmain <- with(predimed, Surv(toevent, event == "Yes"))
attr(predimed$tmain, "label") <- "AMI, stroke, or CV Death"


compareGroups(group ~ ., data = predimed) %>% View()
