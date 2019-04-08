### try lme4 normal-binomial models ###

library(tidyverse)
library(meta)
library(lme4)
library(ggsci)


read_csv("/Users/joelewis/Documents/Projects/ESBL_prev_rv/esbl_ssa/data/final_data_extraction.csv") -> df

df$study_name <- paste0(df$first_author, " ", df$year_pub)

df<- subset(df, Adults_or_children == "adults" |  Adults_or_children == "children" | 
                      Adults_or_children == "neonates" )

df <- subset(df, population == "community" |population == "IP" |
                      population == "OP" | population == "on_hospital_admission")


#df.adults$population <- factor(df.adults$population,levels = c("IP" , "on_hospital_admission", "OP"  , "community",  "Community_and_OP","hospital_workers","IP guardians" ,"institution" ))



names(df)[names(df) =="carriage_n_patients"] <- "n"
names(df)[names(df) =="carriage_n_patients_ESBL_total"] <- "ESBL"

glmer( formula = cbind(  ESBL, n - ESBL ) ~ Adults_or_children + population + (1 | study_name), data = df , family = binomial  ) -> m

newstudy <- data.frame(study_name = rep("newstudy",12),Adults_or_children = rep(c("neonates", "children", "adults" ), 4), 
                       population = c(rep("community",3 ),rep("OP",3 ), rep("on_hospital_admission",3 ), rep("IP",3 ) ))


sfun2 <- function(x) {
  predict(x,newdata=newstudy,re.form= ~0,
          allow.new.levels=TRUE)
}

sumBoot <- function(merBoot) {
  return(
    data.frame(fit = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.5, na.rm=TRUE))),
               lwr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE))),
               upr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
    )
  )
}



b2 <- bootMer(m, FUN = sfun2, nsim = 1000)

b2sum <- sumBoot(b2)

newstudy$fit <- plogis(b2sum$fit)
newstudy$lwr <- plogis(b2sum$lwr)
newstudy$upr <- plogis(b2sum$upr)

newstudy$Adults_or_children <- str_to_title(newstudy$Adults_or_children)

newstudy$population <- as.character(newstudy$population)
newstudy$population[newstudy$population == "community"] <- "Community"
newstudy$population[newstudy$population == "IP"] <- "Inpatient"
newstudy$population[newstudy$population == "on_hospital_admission"] <- "On hospital admission"
newstudy$population[newstudy$population == "OP"] <- "Outpatient"

newstudy$population <- factor(newstudy$population, levels = c("Inpatient", "On hospital admission",
                                                                 "Outpatient", "Community"))

ggplot(newstudy, aes(x= Adults_or_children, y = fit, colour = Adults_or_children)) + geom_point() + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0) + facet_wrap(~ population, ncol= 1) + 
  coord_flip() + theme_classic()  + theme(panel.grid.major.y =  element_blank(),
                                         strip.background = element_blank(), 
                                         strip.text.x = element_text(size = 10, face = "bold"),
                                         axis.line.y = element_blank())

