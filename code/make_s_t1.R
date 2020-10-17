# make s_t1 #

library(tidyverse)

read_csv("/Users/joelewis/Documents/Projects/ESBL_prev_rv/esbl_ssa/data/final_data_extraction.csv") -> df

df$study_name <- paste0(df$first_author, " ", df$year_pub)

df[order(df$year_pub, df$first_author),] -> df

df %>% select( study_name, sample_type, screen, method_speciation,
               method_ESBL_confirmation) -> dfout

dfout$method_ESBL_confirmation[dfout$method_ESBL_confirmation == "double_disc"] <- "Double disc"
dfout$method_ESBL_confirmation[dfout$method_ESBL_confirmation == "double_disk"] <- "Double disc"
dfout$method_ESBL_confirmation[dfout$method_ESBL_confirmation == "combination_disc"] <- "Combination disc"
dfout$method_ESBL_confirmation[dfout$method_ESBL_confirmation == "combination disc"] <- "Combination disc"
dfout$method_ESBL_confirmation[dfout$method_ESBL_confirmation == "BD_phoenix"] <- "BD phoenix"
dfout$method_ESBL_confirmation[dfout$method_ESBL_confirmation == "pcr"] <- "PCR"

str_to_s (dfout$sample_type) -> dfout$sample_type
sub("_", " ", dfout$sample_type) -> dfout$sample_type
str_to_title(dfout$screen) -> dfout$screen

dfout <- unique(dfout)

write.csv(dfout, 
          "/Users/joelewis/Documents/Projects/ESBL_prev_rv/esbl_ssa/tables/draft_ts1.csv",
          row.names = F)
