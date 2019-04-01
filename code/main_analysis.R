# main analysis script for ESBL review

library(tidyverse)
library(meta)
library(lme4)
library(ggsci)


read_csv("/Users/joelewis/Documents/Projects/ESBL_prev_rv/esbl_ssa/data/final_data_extraction.csv") -> df

df$study_name <- paste0(df$first_author, " ", df$year_pub)

df$prev <- df$carriage_n_patients_ESBL_total / df$carriage_n_patients 

df$lci <- apply(select(df,carriage_n_patients ,carriage_n_patients_ESBL_total ), 
      1, function(x) binom.test(x[2],x[1])$conf.int[1])

df$uci <- apply(select(df,carriage_n_patients ,carriage_n_patients_ESBL_total ), 
                1, function(x) binom.test(x[2],x[1])$conf.int[2])

dftots <- df %>% group_by(study_name) %>% summarise(n = sum(carriage_n_patients), 
                                                    x= sum(carriage_n_patients_ESBL_total),
                                                    country = unique(country))

dftots <- ungroup(dftots)

dftots$lci <- apply(select(dftots,n ,x ), 
                1, function(x) binom.test(x[2],x[1])$conf.int[1])

dftots$uci <- apply(select(dftots,n ,x ), 
                1, function(x) binom.test(x[2],x[1])$conf.int[2])

dftots$prev <- dftots$x / dftots$n

dftots$study_name <- factor(dftots$study_name, levels = dftots$study_name[order(dftots$prev)])

ggplot(dftots, aes(study_name, prev)) + geom_point(shape = "square") + 
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0) + 
  coord_flip() + theme_bw() + ylab("ESBL colonisation prevalence") + xlab("") -> p

ggsave("/Users/joelewis/Documents/Projects/ESBL_prev_rv/esbl_ssa/figures/overal_prev_plot.pdf", p, 
       width = 14, height = 12, units = "cm")


df$study_name <- factor(df$study_name, levels = unique(df$study_name[order(df$prev)]))

ggplot(subset(df, Adults_or_children != "both"), aes(study_name, prev)) + geom_point() + geom_errorbar(aes(ymin = lci, ymax = uci)) + 
  coord_flip() + facet_wrap(~ Adults_or_children)

df.adults <- subset(df, Adults_or_children == "adults" |  Adults_or_children == "children" | 
                      Adults_or_children == "neonates" )




df.adults$population <- factor(df.adults$population,levels = c("IP" , "on_hospital_admission", "OP"  , "community",  "Community_and_OP","hospital_workers","IP guardians" ,"institution" ))

df.adults$study_name <- factor(df.adults$study_name,levels = unique(df.adults$study_name[order(df.adults$year_pub) ]))

df.adults <- subset(df.adults, population == "community" |population == "IP" |
                      population == "OP" | population == "on_hospital_admission")

df.a.sum <- df.adults %>% group_by(study_name) %>% mutate(n = sum(carriage_n_patients), x= sum(carriage_n_patients_ESBL_total))  

metaprop(x, n, data = df.a.sum, 
         , byvar = population, studlab = study_name) -> ma

summary <- data.frame(study_name = "Summary estimate", Adults_or_children = "Summary", population = ma$bylevs, prev = plogis(ma$TE.random.w),
                      lci = plogis(ma$lower.random.w), uci = plogis(ma$upper.random.w))

ggplot(df.adults,aes(study_name, prev, colour = Adults_or_children, shape = Adults_or_children)) + geom_point() +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.5) + 
  coord_flip() + ylim(c(0,1)) + facet_wrap(~ population, scales = 'free', ncol = 1) + theme_bw()

dfout <- rbind(select(df.adults, study_name, Adults_or_children, population, prev, lci, uci),
               summary)

dfout$study_name <- factor(dfout$study_name,levels = c( "Summary estimate" , as.character(unique(df.adults$study_name[order(df.adults$year_pub)])) ))

cols = c(pal_lancet()(3), "black")

names(cols) <- c("adults", "children", "neonates", "Summary")

dfout$population <- as.character(dfout$population)
dfout$population[dfout$population == "IP"] <- "Inpatient"
dfout$population[dfout$population == "OP"] <- "Outpatient"
dfout$population[dfout$population == "on_hospital_admission"] <- "On hospital admission"
dfout$population[dfout$population == "community"] <- "Community"
      
dfout$population <- factor(dfout$population, levels = c("Inpatient", "On hospital admission", "Outpatient","Community" ))           
                 
                 
ggplot(dfout,aes(study_name, prev, colour = Adults_or_children, shape = Adults_or_children)) + geom_point(alpha = 0.8) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.5, alpha = 0.8) + 
 # geom_hline(aes(yintercept = prev), data = subset(dfout, study_name == "Summary estimate"), linetype = "dashed") +
  coord_flip() + ylim(c(0,1)) + facet_wrap(~ population, scales = 'free', ncol = 1) + theme_bw() +
  scale_colour_manual(values = cols) + 
  theme(panel.grid.major.y =  element_blank(),
        strip.background = element_blank(), strip.text.x = element_text(size = 10, face = "bold"))

str_to_title(dfout$Adults_or_children, locale = "en") -> dfout$Adults_or_children

names(cols) <- c("Adults", "Children", "Neonates", "Summary")
dfout$population <- factor(dfout$population, levels = rev(c("Inpatient", "On hospital admission", "Outpatient","Community" )))           


p <- ggplot(subset(dfout, study_name != "Summary estimate"), aes(study_name, prev, colour = Adults_or_children)) + geom_point(alpha = 0.8, shape = "square") +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0, alpha = 0.8) + 
  # geom_hline(aes(yintercept = prev), data = subset(dfout, study_name == "Summary estimate"), linetype = "dashed") +
  coord_flip() + ylim(c(0,1)) + facet_grid(Adults_or_children ~ population, scales = 'free') + theme_bw() +
  scale_colour_manual(values = cols) + xlab("") + 
  theme(
        strip.background = element_blank(), strip.text = element_text(size = 10, face = "bold"), 
        legend.position = "none") + scale_y_continuous(name = "ESBL colonisation prevalence",
                                                       breaks = c(0,0.2, 0.4, 0.6,0.8, 1), limits = c(0,1))

ggsave("/Users/joelewis/Documents/Projects/ESBL_prev_rv/esbl_ssa/figures/grid_prev_plot.pdf", p, 
       width = 22, height = 18, units = "cm")

df.a.sum <- df.adults %>% group_by(study_name, population, year_pub) %>% 
  summarise(n = sum(carriage_n_patients), x= sum(carriage_n_patients_ESBL_total)) 

metaprop(x, n, data = df.a.sum, 
         , byvar = population, studlab = study_name) -> ma

df.a.sum$population <- as.character(df.a.sum$population)
df.a.sum$population[df.a.sum$population == "IP"] <- "Inpatient"
df.a.sum$population[df.a.sum$population == "OP"] <- "Outpatient"
df.a.sum$population[df.a.sum$population == "on_hospital_admission"] <- "On hospital admission"
df.a.sum$population[df.a.sum$population == "community"] <- "Community"

df.a.sum$population <- factor(df.a.sum$population, levels = (c("Inpatient", "On hospital admission", "Outpatient","Community" )))           

df.a.sum$study_name <- factor(df.a.sum$study_name,levels = (unique(df.adults$study_name[order(df.adults$year_pub) ])))


metaprop(x, n, data = df.a.sum, 
         , byvar = population, studlab = study_name) -> ma

forest(ma, col.square="black", hetstat=TRUE, comb.fixed = F, sortvar = rev(year_pub), comb.random = T, overall = F, leftcols = c("studlab", "n"), rightcols = c("effect", "ci"), 
       leftlabs = c("Study", "n", "ESBL"), rightlabs = c("ESBL (%)", "95% CI"), xlab = "", 
       digits = 2,   xlim = c(0,1), fontsize = 8, 
       width = "4cm", height = "3cm",spacing = 0.8,
       weight.study = "fixed", bylab = "") 

# save as 6 x10 in



