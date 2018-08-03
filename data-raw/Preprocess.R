setwd("home_from_SE/survbayes2AZ/")

library(readr)
library(dplyr)
library(tidyprojectAZ)
library(ggplot2)
set_theme(set_bw)

# load Lung Scamous carcinoma dataset
lusc <- read_tsv("test-data/lusc/clinical/clinical.tsv")
colnames(lusc) <- tolower(colnames(lusc))

# Inspect dataframe
glimpse(lusc)

source("R/misc.R") # or devtools::document() when completed

lusc <- lusc %>%
  mutate_all(funs(convert_blank_to_na))

lusc %>%
  VIM::aggr(prop = FALSE, combined = TRUE, numbers = TRUE,
            sortVars = TRUE, sortCombs = TRUE, plot = TRUE, only.miss = FALSE)

# Get survival data

surv_lusc <- lusc %>% 
  dplyr::select(submitter_id, days_to_last_follow_up, vital_status, age_at_diagnosis                  ,  morphology, tissue_or_organ_of_origin, gender, tumor_stage, race, ethnicity ) %>%
  dplyr::rename(
    patient_id = submitter_id,
    time = days_to_last_follow_up
  ) %>%
  dplyr::mutate_at(vars(time, age_at_diagnosis), .funs = as.numeric) %>%
  dplyr::mutate(status = vital_status == "dead")

glimpse(surv_lusc)

# Plot km max likelihood estimate

mle.surv <- survival::survfit(survival::Surv( time , status) ~ tumor_stage,
                              data = surv_lusc )

require(ggfortify)
ggplot2::autoplot(mle.surv, conf.int = F) +
  labs(x = "Time (days)", y = "Survival probability", title= "K-M")
rm(list = c("mle.surv", "lusc"))

# Save data to use in SCP

devtools::use_data(surv_lusc)

# LOAD exposure data

exp_lusc <- readr::read_tsv("test-data/lusc/clinical/exposure.tsv")

glimpse(exp_lusc)

exp_lusc <- blank_to_na(exp_lusc)

exp_lusc %>%
  VIM::aggr(prop = FALSE, combined = TRUE, numbers = TRUE,
            sortVars = TRUE, sortCombs = TRUE, plot = TRUE, only.miss = FALSE)

exp_lusc <- exp_lusc %>%
  dplyr::select(years_smoked, cigarettes_per_day, submitter_id) %>%
  dplyr::rename(patient_id = submitter_id) %>%
  dplyr::mutate_at( vars( years_smoked, cigarettes_per_day), funs(as.numeric) )

surv_lusc <- left_join(surv_lusc, exp_lusc, by = "patient_id")

# Save data to use in SCP

devtools::use_data(surv_lusc, overwrite = T)
rm(exp_lusc)

# load biosamples

aliquot_lusc <- readr::read_tsv("test-data/lusc/biospecimen/aliquot.tsv")
glimpse(aliquot_lusc)
