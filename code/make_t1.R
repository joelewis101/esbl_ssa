## prepare overview table ###

library(tidyverse)

read_csv("/Users/joelewis/Documents/Projects/ESBL_prev_rv/esbl_ssa/data/final_data_extraction.csv") -> df

df$study_name <- paste0(df$first_author, " ", df$year_pub)

df[order(df$year_pub, df$first_author),] -> df

df$study_period <- paste0(df$year_of_samples_start , "-", str_sub(df$year_of_samples_end, start= -2))

df$study_period[df$year_of_samples_start == df$year_of_samples_end & !is.na(df$year_of_samples_start)] <-
  df$year_of_samples_start[df$year_of_samples_start == df$year_of_samples_end & !is.na(df$year_of_samples_start)]
df$study_period[is.na(df$year_of_samples_start)] <- "NA"

df$study_type[df$study_type == "cross_sec_survey"] <- "Cross sec."
df$study_type[df$study_type == "cohort"] <- "Cohort"


select(df, study_name, year_pub, study_period, country, study_type, inclusion, 
       Adults_or_children, median_age, carriage_n_patients ) -> dfout



write.csv(dfout, "/Users/joelewis/Documents/Projects/ESBL_prev_rv/esbl_ssa/tables/draft_t1.csv", row.names = F)

sum(dfout$carriage_n_patients)

# 8619 participants

dfout %>% group_by(Adults_or_children) %>% summarise(n = sum(carriage_n_patients))

df %>% group_by(population)  %>% summarise(n = sum(carriage_n_patients))

df %>% group_by(population, Adults_or_children)  %>% summarise(n = sum(carriage_n_patients))
