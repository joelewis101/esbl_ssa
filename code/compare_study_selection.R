
library(tidyverse)

joe <- read_csv("/Users/joelewis/Documents/Projects/ESBL_prev_rv/esbl_ssa/joe_include.csv")

joe$which_analysis[joe$which_analysis == "Carriage"] <- "carriage"

(subset(joe, which_analysis == "carriage" & Include == "Y" & species == "human")) -> carriage_joe
write.csv(carriage_joe, "/Users/joelewis/Documents/Projects/ESBL_prev_rv/esbl_ssa/joe_include.csv")

carriage_reb <- read_csv("/Users/joelewis/Documents/Projects/ESBL_prev_rv/esbl_ssa/rebecca_include.csv")

joe_studnams <- select(carriage_joe, year_pub, journal, first_author, title)
(unique(joe_studnams)) -> joe_studnams
joe_studnams$joe_id <- 1:nrow(joe_studnams)

sub(",","", joe_studnams$first_author) -> joe_studnams$first_author

reb_studnams <- select(carriage_reb, year_pub, journal, first_author, title)
subset(reb_studnams, !is.na(title)) -> reb_studnams
(unique(reb_studnams)) -> reb_studnams
reb_studnams$reb_id <- 1:nrow(reb_studnams)


nrow(joe_studnams)
nrow(reb_studnams)
#merge(joe_studnams, reb_studnams, by = c("year_pub", "first_author"), all.x = T, all.y = T) -> merged

full_join(joe_studnams, reb_studnams, by = c("year_pub", "first_author")) -> merged

# any duplicates?

table(merged$joe_id)
table(merged$reb_id)
length(table(merged$joe_id))
length(table(merged$reb_id))



write.csv(merged,"/Users/joelewis/Documents/Projects/ESBL_prev_rv/esbl_ssa/merged.csv" )
