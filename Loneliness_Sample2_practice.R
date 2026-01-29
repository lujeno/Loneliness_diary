#-------Importing data######
library(readxl)

lonely_prac<- read_excel("Loneliness_Praksisdataset.xlsx")
View(lonely_prac)

#data preparation
options(scipen = 999)

#Create ID######
lonely_prac$id = c(1:nrow(lonely_prac))
View(lonely_prac)

#Recode variable####
library(car)

table(lonely_prac$Gender)
lonely_prac$Gender <- Recode(lonely_prac$Gender, "1= 0; 2= 1; 3=2; 4=3", as.numeric=T) #0=males, 1=Females, 2=Other, 3=Dont wish
View(lonely_prac)
table(lonely_prac$Gender)


table(lonely_prac$Gender)
lonely_prac$Gender2 <- factor(lonely_prac$Gender, labels = c("Males", "Females")) #0=Males, 1=Females
table(lonely_prac$Gender2)

#-------Descriptive analyses####
library(psych)
library(summarytools)

#Summaries

desc_variables_lonely <-  subset(lonely_prac, select = c(Prac_lonely_day1,Prac_lonely_day2,Prac_lonely_day3,
                                                  Prac_lonely_day4,Prac_lonely_day5))

describe(desc_variables_lonely)

dfSummary(desc_variables_lonely)

desc_variables_BPN <-  subset(lonely_prac, select = c(Aut_day1,Aut_day2,Aut_day3,Aut_day4,Aut_day5,
                                                      Comp_day1,Comp_day2,Comp_day3,Comp_day4,Comp_day5,
                                                      Rel_peer_day1,Rel_peer_day2,Rel_peer_day3,Rel_peer_day4,Rel_peer_day5,
                                                      Rel_mentor_day1,Rel_mentor_day2,Rel_mentor_day3,Rel_mentor_day4,Rel_mentor_day5))
describe(desc_variables_BPN)

dfSummary(desc_variables_BPN)

desc_variables_practice <-  subset(lonely_prac, select = c(Practice_obs_day1,Practice_obs_day2,Practice_obs_day3,Practice_obs_day4,Practice_obs_day5,
                                                           Practice_partic_day1,Practice_partic_day2,Practice_partic_day3,Practice_partic_day4,Practice_partic_day5,
                                                           Practice_alone_day1,Practice_alone_day2,Practice_alone_day3,Practice_alone_day4,Practice_alone_day5,
                                                           Practice_planmeeting_day1,Practice_planmeeting_day2,Practice_planmeeting_day3,Practice_planmeeting_day4,Practice_planmeeting_day5,
                                                           Practice_noprac_day1,Practice_noprac_day2,Practice_noprac_day3,Practice_noprac_day4,Practice_noprac_day5))
freq(desc_variables_practice)

describe(lonely_prac$Age)

dfSummary(lonely_prac$Age)

#-------Missing analyses####
library(summarytools)

Missingsummary_prac<- dfSummary(desc_variables_lonely, round.digits = 2)
Missingsummary_prac2<- dfSummary(desc_variables_BPN, round.digits = 2)
Missingsummary_prac3 <- dfSummary(summary_id, round.digits = 2)

view(Missingsummary_prac)
view(Missingsummary_prac2)
view(Missingsummary_prac3)

#-------Creating individual long format datasets######
#Creating individual long format datasets
library(tidyr)

lonely_prac_long <- lonely_prac %>%
  pivot_longer(
    cols = starts_with("Practice_") | starts_with("Aut_") | starts_with("Comp_") | starts_with("Rel_") | starts_with("Prac_"),
    names_to = c(".value", "day"),
    names_pattern = "(.*)_day(\\d+)",
    values_drop_na = TRUE
  )

View(lonely_prac_long)

#--Between and within group correlations (id)
library(rmcorr)

between_corr_main_practice <- subset(lonely_prac_long,select = c(Aut, Comp,Rel_peer, Rel_mentor, Prac_lonely))

cor.plot(between_corr_main_practice, stars = T, upper = F)

repeated_corr_practice<- rmcorr_mat(participant=id, variables = c("Aut","Comp","Rel_peer", "Rel_mentor",
                                                                "Prac_lonely"),dataset = lonely_prac_long, CI.level = 0.95)
repeated_corr_practice$summary
repeated_corr_practice$matrix
repeated_corr_practice$models

#-Create APA table
library(apaTables)

apa.cor.table(between_corr_main_practice, filename = "Table4.doc", table.number = 4,
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
null_model_loneliness <- lmer(Prac_lonely ~ 1 + (1|id), data = lonely_prac_long, REML = FALSE) # note that REML = FALSE
performance::icc(null_model_loneliness)

#Visualizing changes over time Loneliness
library(ggplot2)

#-Recoding before visualizing
library(car)

#Day
table(lonely_prac_long$day)
lonely_prac_long$day <- factor(lonely_prac_long$day, labels = c("Day1", "Day2","Day3","Day4","Day5")) #1=Day1, 2=Day2, 3=Day3, 4=Day4, 5=Day5
table(lonely_prac_long$day)


lonely_prac_long %>% 
  ggplot(aes(day, Prac_lonely, group=1))  + 
  stat_summary(fun.data = mean_cl_boot, geom="ribbon", alpha=.3) + 
  stat_summary(fun.y = mean, geom="line") + 
  labs(x="Time interval", y = "Loneliness", 
       title="Loneliness across a practice week", subtitle = "Mean and Boostrapped 95% Confidence Intervals")
ggsave(filename="practice_loneliness.jpg", width=36, height=22, units="cm", dpi=200)


lonely_prac_long$day <- as.numeric(lonely_prac_long$day) 
table(lonely_prac_long$day) 

#-Adding predictors random intercept
loneliness_model1 <- lme(Prac_lonely ~ 1 + Aut + Comp + Rel_peer + Rel_mentor + Practice_obs + Practice_partic+ Practice_alone + Practice_planmeeting + day ,
                     data = lonely_prac_long,
                     method="ML",
                     na.action = "na.omit",
                     random = ~ 1|id)

summary(loneliness_model1)

loneliness_model1_report <- report(loneliness_model1)
as.data.frame(loneliness_model1_report)
loneliness_model1_report
VarCorr(loneliness_model1)

#-Adding interaction random intercept 
loneliness_model2 <- lme(Prac_lonely ~ 1 + Aut + Comp + Rel_peer + Rel_mentor +  Practice_obs + Practice_partic+ Practice_alone + Practice_planmeeting + day +
                           Practice_obs:day,
                     data = lonely_prac_long,
                     method="ML",
                     na.action = "na.omit",
                     random = ~ 1|id)
summary(loneliness_model2)

anova(loneliness_model1, loneliness_model2) #model2 is slighly better

loneliness_model2_report <- report(loneliness_model2)
as.data.frame(loneliness_model2_report)
loneliness_model2_report
VarCorr(loneliness_model2)


#-Adding day as random intercept
loneliness_model3 <- lme(Prac_lonely ~ 1 + Aut + Comp + Rel_peer + Rel_mentor +  Practice_obs + Practice_partic+ Practice_alone + Practice_planmeeting + day +
                           Practice_obs:day,
                         data = lonely_prac_long,
                         method = "ML",
                         na.action = "na.omit",
                         random = list(id = ~ 1, day = ~ 1))

summary(loneliness_model3)

#-Comparing models #model 1 better than 3
anova(loneliness_model2, loneliness_model3) #model2 is  better

#-visualizing
lonely_prac_long$Practice_obs <- factor(lonely_prac_long$Practice_obs, labels = c("No observation", "Only observation")) #0=no, 1=yes

lonely_prac_long %>% 
  ggplot(aes(day, Prac_lonely, group=Practice_obs, fill=Practice_obs))  + 
  stat_summary(fun.data = mean_se, geom="ribbon", alpha=.1) +
  stat_summary(fun.y = mean, geom="line") + 
  labs(x="Time interval", y = "Loneliness", 
       color="Only observation in practice", fill="Only observation in practice", linetype="Only observation in practice")
ggsave(filename="loneliness_practice_observation_prac", width=36, height=22, units="cm", dpi=200)

#########################################################################
#Visualizing
lonely_prac_long_nomiss <- na.omit(lonely_prac_long)
loneliness_model1 <- lme(
  Prac_lonely ~ 1 + Aut + Comp + Rel_peer + Rel_mentor + Practice_alone + Practice_planmeeting + Practice_noprac + day,
  data = lonely_prac_long_nomiss,
  method = "ML",
  random = ~ 1 | id
)
lonely_prac_long_nomiss$pred <- predict(loneliness_model1)

ggplot(lonely_prac_long_nomiss, aes(x = pred, y = Prac_lonely)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(x = "Predicted loneliness", y = "Actual loneliness",
       title = "Predicted vs. Actual loneliness")
#########################################################################



