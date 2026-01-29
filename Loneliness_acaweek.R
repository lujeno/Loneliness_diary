#-------Importing data######
library(foreign)

lonely_mentor <-read.spss("Loneliness_mentor.sav", use.value.labels = FALSE, to.data.frame = TRUE)
View(lonely_mentor)

#data preparation
options(scipen = 999)

#Create ID######
lonely_mentor$id = c(1:nrow(lonely_mentor))
View(lonely_mentor)

#-------Recode variable####
library(car)

table(lonely_mentor$Gender)
lonely_mentor$Gender <- Recode(lonely_mentor$Gender, "1= 0; 2= 1", as.numeric=T) #0=females, 1=males
View(lonely_mentor)
table(lonely_mentor$Gender)

#-------Descriptive analyses####
library(psych)
library(summarytools)

#Summaries
desc_variables_mentor <-  subset(lonely_mentor, select = c(aut_k1,kom_k1,related_k1,ensom_k1,
                                               aut_k2,kom_k2,related_k2,ensom_k2,
                                               aut_k3,kom_k3,related_k3,ensom_k3,
                                               aut_k4,kom_k4,related_k4,ensom_k4,
                                               aut_k5,kom_k5,related_k5,ensom_k5))

describe(desc_variables_mentor)

describe(lonely_mentor$Age)
freq(lonely_mentor$Age)
lonely_mentor$Age2 <- Recode(lonely_mentor$Age, "18:19='Under20'; 20:21='20-21'; 22:23='22-23';24:36='24ogover'", 
                          as.factor=T)

dfSummary(lonely_mentor$Age2)

desc_kontroll_mentor <-  subset(lonely_mentor, select = c(kontroll_forelesning1,kontroll_forelesning2,kontroll_forelesning3,kontroll_forelesning4,kontroll_forelesning5,
                                                           kontroll_seminar1,kontroll_seminar2,kontroll_seminar3,kontroll_seminar4,kontroll_seminar5,
                                                           kontroll_lestalene1,kontroll_lestalene2,kontroll_lestalene3,kontroll_lestalene4,kontroll_lestalene5,
                                                           kontroll_lestvenner1,kontroll_lestvenner2,kontroll_lestvenner3,kontroll_lestvenner4,kontroll_lestvenner5))
freq(desc_kontroll_mentor)

#-------Missing analyses####
library(summarytools)

Missingsummary_mentor<- dfSummary(desc_variables, round.digits = 2)

view(Missingsummary_mentor)

#-------Creating individual long format datasets######
#Creating individual long format datasets
library(tidyr)

lonely_mentor_long <- lonely_mentor %>%
  pivot_longer(
    cols = starts_with("kontroll_") | starts_with("aut_") | starts_with("kom_") | starts_with("related_") | starts_with("ensom_"),
    names_to = c(".value", "day"),
    names_pattern = "(.*)(\\d+)$"
  )

View(lonely_mentor_long)

library(dplyr)

lonely_mentor_long <- lonely_mentor_long %>%
  rename(
    aut = aut_k,
    kom = kom_k,
    related = related_k,
    ensom = ensom_k
  )

View(lonely_mentor_long)

#-------Correlational analysis#####
main_variables_corr_long <- subset(lonely_mentor_long,select = c(aut, kom,related, ensom))

Missingsummary_mentor2<- dfSummary(main_variables_corr_long, round.digits = 2)

view(Missingsummary_mentor2)

cor.plot(main_variables_corr_long, main = "Correlation of main study variables", stars = TRUE, upper=FALSE)

#--Between and within group correlations (id)
library(rmcorr)

between_corr_main_mentor <- subset(lonely_mentor_long,select = c(aut, kom,related, ensom))

cor.plot(between_corr_main_mentor, stars = T, upper = F)

repeated_corr_mentor<- rmcorr_mat(participant=id, variables = c("aut","kom","related",
                                                              "ensom"),dataset = lonely_mentor_long, CI.level = 0.95)
repeated_corr_mentor$summary
repeated_corr_mentor$matrix
repeated_corr_mentor$models

#-Create APA table
library(apaTables)

apa.cor.table(between_corr_main_mentor, filename = "Table2.doc", table.number = 2,
              show.conf.interval = TRUE, landscape = TRUE)

#-------Linear mixed effects model for Loneliness#####
library(lme4)
library(nlme)
library(report)

library(lmerTest) 
library(lmtest)
library(sandwich)

library(performance)
library(modelsummary)
library(pscl)
library(r2mlm) 

#ICC check Loneliness
null_model_ensom <- lmer(ensom ~ 1 + (1|id), data = lonely_mentor_long, REML = FALSE)
             
performance::icc(null_model_ensom)

#Visualizing changes over time Loneliness
library(ggplot2)

#-Recoding before visualizing
library(car)

#Day
table(lonely_mentor_long$day)
lonely_mentor_long$day <- factor(lonely_mentor_long$day, labels = c("Day1", "Day2","Day3","Day4","Day5")) #1=Day1, 2=Day2, 3=Day3, 4=Day4, 5=Day5
table(lonely_mentor_long$day)

lonely_mentor_long %>% 
  ggplot(aes(day, ensom, group=1))  + 
  stat_summary(fun.data = mean_cl_boot, geom="ribbon", alpha=.3) + 
  stat_summary(fun.y = mean, geom="line") + 
  labs(x="Time interval", y = "Loneliness", 
       title="Loneliness across a study week", subtitle = "Mean and Boostrapped 95% Confidence Intervals")
ggsave(filename="mentor_loneliness.jpg", width=36, height=22, units="cm", dpi=200)

lonely_mentor_long$day <- as.numeric(lonely_mentor_long$day) 
table(lonely_mentor_long$day) 

#-Adding predictors random intercept
lonely_mentor_model1 <- lme(ensom ~ 1 + aut + kom + related + kontroll_forelesning + kontroll_seminar +
                              kontroll_lestalene + kontroll_lestvenner + day,
                         data = lonely_mentor_long,
                         method="ML",
                         na.action = "na.omit",
                         random = ~ 1|id)

summary(lonely_mentor_model1)

lonely_mentor_model1_report <- report(lonely_mentor_model1)
as.data.frame(lonely_mentor_model1_report)
lonely_mentor_model1_report
VarCorr(lonely_mentor_model1)

#-Adding random intercepts day
lonely_mentor_model3 <- lme(ensom ~ 1 + aut + kom + related + kontroll_forelesning + kontroll_seminar +
                              kontroll_lestalene + kontroll_lestvenner + day,
                         data = lonely_mentor_long,
                         method = "ML",
                         na.action = "na.omit",
                         random = list(id = ~ 1, day = ~ 1))

summary(lonely_mentor_model3)

lonely_mentor_model3_report <- report(lonely_mentor_model3)
as.data.frame(lonely_mentor_model3_report)
lonely_mentor_model3_report
VarCorr(lonely_mentor_model3)

#-Comparing models 
anova(lonely_mentor_model1, lonely_mentor_model3)  #model 1 better than 3

#visualizing
library(tidyverse)

lonely_mentor_long$kontroll_forelesning <- factor(lonely_mentor_long$kontroll_forelesning, labels = c("not_lecture", "partipated_lecture")) #0=no, 1=yes
lonely_mentor_long$kontroll_lestalene <- factor(lonely_mentor_long$kontroll_lestalene, labels = c("not_studying_alone", "studying_alone")) #0=no, 1=yes

#-Study alone
a <- lonely_mentor_long %>% 
  ggplot(aes(day, ensom, group=kontroll_lestalene, fill=kontroll_lestalene))  + 
  stat_summary(fun.data = mean_se, geom="ribbon", alpha=.1) +
  stat_summary(fun.y = mean, geom="line") + 
  labs(x="Time interval", y = "Loneliness", 
       color="Studying alone", fill="Studying alone", linetype="Studying alone")
ggsave(filename="loneliness_mentor_studyalone", width=36, height=22, units="cm", dpi=200)

#-Lectures
b<- lonely_mentor_long %>% 
  ggplot(aes(day, ensom, group=kontroll_forelesning, fill=kontroll_forelesning))  + 
  stat_summary(fun.data = mean_se, geom="ribbon", alpha=.1) +
  stat_summary(fun.y = mean, geom="line") + 
  labs(x="Time interval", y = "Loneliness", 
       color="Participated in lecture", fill="Participated in lecture", linetype="Participated in lecture")
ggsave(filename="loneliness_mentor_lecture", width=36, height=22, units="cm", dpi=200)

#ggpatch visualization of all ggplot figures 
library(patchwork)
a / b 
