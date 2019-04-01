# make t2 #

library(tidyverse)

read_csv("/Users/joelewis/Documents/Projects/ESBL_prev_rv/esbl_ssa/data/final_data_extraction.csv") -> df

df$study_name <- paste0(df$first_author, " ", df$year_pub)

df[order(df$year_pub, df$first_author),] -> df

df %>% select( study_name, year_pub, `Risk factors assessed`, `Which risk factors`, `Sig risk factors`) %>%
  subset(`Risk factors assessed` == "Yes") -> dfout

write.csv(dfout, 
          "/Users/joelewis/Documents/Projects/ESBL_prev_rv/esbl_ssa/tables/draft_t2.csv",
          row.names = F)
