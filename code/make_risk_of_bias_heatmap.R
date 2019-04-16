 # risk of bias plot #

library(tidyverse)
library(reshape2)

df <- read_csv("data/risk_of_biasJL.csv")
names(df)[6:10] <- c("d1_demographics", "d2_inclusion", "d3_micro", "d4_ESBL", "QA")

df$study_name <- paste0(df$first_author, " ", df$year_pub)

df$d3_micro[df$QA == "N"] <- "No"

melt(dplyr::select(df, study_name, d1_demographics, 
                   d2_inclusion, d3_micro, d4_ESBL), id.vars = "study_name") -> df_melt

df_melt$variable <- as.character(df_melt$variable)

df_melt$variable[df_melt$variable == "d1_demographics"] <- "D1: Participants"
df_melt$variable[df_melt$variable == "d2_inclusion"] <- "D2: Inclusion"
df_melt$variable[df_melt$variable == "d3_micro"] <- "D3: Culture"
df_melt$variable[df_melt$variable == "d4_ESBL"] <- "D4: ESBL Confirmation"

ggplot(df_melt, aes(study_name, variable, fill = value)) + 
  geom_tile(alpha = 0.8, colour = "black") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), 
        axis.line = element_blank(), legend.title = element_blank()) + 
  labs(x = "", y = "", legend = "") + 
  scale_fill_manual(values = c("indianred1",  "#91cf60"), labels = c(" No", " Yes")) -> p1

ggsave("figures/risk_of_bias_heatmap.png",p1, device = "png", width = 22, height = 12, units = "cm")
