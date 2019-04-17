### compaere risk of bias JL and RL 

j <- read_csv("data/risk_of_biasJL.csv")

names(j)[6:10] <- c("d1_joe", "d2_joe", "d3_joe", "d4_joe", "QA")

j$study_name <- paste0(j$first_author, " ", j$year_pub)

j$d3_joe[j$QA == "N"] <- "No"

j <- select(j, study_name, d1_joe, d2_joe, d3_joe, d4_joe)
  
r <- read_csv("data/Copy of bias RL.csv")


names(r)[2:5] <-  c("d1_rebecca", "d2_rebecca", "d3_rebecca", "d4_rebecca")
sub(",", "", r$Study) -> r$Study
sub("Isendahl  2012", "Isendahl 2012", r$Study) -> r$Study
sub("Adriatahina 2010", "Andriatahina 2010", r$Study) -> r$Study
sub("Chrindze 2018", "Chirindze 2018", r$Study) -> r$Study
sub("Desta 2018", "Desta 2016", r$Study) -> r$Study
sub("Nikema Pessinaba 2018", "Nikiema Pessinaba 2018", j$study_name) -> j$study_name
sub("Ribero 2016", "Ribeiro 2016", r$Study) -> r$Study



df <- merge(j,r, by.x = "study_name", by.y = "Study", all.x = T, all.y = T)

df <- select(df, study_name, d1_joe, d1_rebecca, d2_joe, d2_rebecca, d3_joe, d3_rebecca, d4_joe, d4_rebecca)

write.csv(df, "risk of bias/compare_RoB.csv")
