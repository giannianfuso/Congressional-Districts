
library(MASS)
library(stargazer)
library(lmtest)
library(glm2)
library(broom)
library(tidyverse)
select <- dplyr::select

#IMR by Race/Ethnicity
#test Poisson vs Negative binomial
#rename dataset to make easier to use
race_IMR<-IMR_byCD_byRace_2_details%>%
  mutate(im=as.integer(InfantMortality))
hist(race_IMR$InfantMortality)
summary(race_IMR$InfantMortality)
str(race_IMR)
levels(race_IMR$RACEHISP)
summary(mod_p<-glm(InfantMortality~RACEHISP+offset(log(LiveBirths/1000)), family="poisson", data=race_IMR))
summary(mod_nb<-glm.nb(InfantMortality~RACEHISP+offset(log(LiveBirths/1000)), data=race_IMR))
lrtest(mod_p, mod_nb)
AIC(mod_p)
AIC(mod_nb)
BIC(mod_p)
BIC(mod_nb)
str(race_IMR)

#overall it's better nb, but we don't have overdispersion if we run stratified by CD.
#testing code for one CD
test_imr<-race_IMR %>%
  filter(CD=="CD 02")
summary(mod_test_im<-glm(im~RACEHISP+offset(log(LiveBirths/1000)), family="poisson", data=test_imr))
stargazer(mod_test_im, apply.coef = exp, apply.ci=exp, type='text')
confint(mod_test_im)
table(race_IMR$RACEHISP)
CI_estim_rate<-race_IMR%>%
  group_by(CONGRESS2, CD)%>%
  group_modify(~{
    mod_test_im<-glm(InfantMortality~RACEHISP+offset(log(LiveBirths/1000)),family="poisson", data=.x)%>%tidy
  })
rel_disp_race<-CI_estim_rate%>%
  mutate(lci=exp(estimate-1.96*std.error),
         uci=exp(estimate+1.96*std.error),
         exp_est=exp(estimate))%>%
  select(CONGRESS2, CD, term, exp_est, lci, uci)%>%
  arrange(CONGRESS2, CD)%>%
  mutate(across(where(is.numeric), round, 2),
         race_ethn=case_when(term=="RACEHISPHispanic" ~"H",
                             term=="RACEHISPNon-Hispanic Black"~"NHB",
                             term=="RACEHISPNon-Hispanic Asian American Pacific Islander"~"NHAAPI",
                             term=="RACEHISPNon-Hispanic Other"~"other"))%>%
  filter(!term%in%c("(Intercept)", "RACEHISPNon-Hispanic Other"))%>%
  select(-term)%>%
  #combine the estimate and CI's into one row
  unite("output", exp_est:lci, sep=" (")%>%
  unite("output", output:uci, sep=" , ")%>%
  mutate(output = paste0(output, ")"))%>%
  pivot_wider(names_from=race_ethn, values_from=c(output))
#write to csv
write.csv(rel_disp_race, "Final Results/rel_disp_race.csv")

#DOD MR by Education
#test Poisson vs Negative binomial
#rename dataset to make easier to use
education_DOD<-DOD_byEducSexCD_2_details
hist(education_DOD$DOD)
summary(education_DOD$DOD)
str(education_DOD)
levels(education_DOD$EDUC)
education_DOD$EDUC <- relevel(education_DOD$EDUC, "Bachelor/Master/Doctorate/Professional Degree")
summary(mod_p<-glm(DOD~EDUC+offset(log(Population/10000)), family="poisson", data=education_DOD))
summary(mod_nb<-glm.nb(DOD~EDUC+offset(log(Population/10000)), data=education_DOD))
lrtest(mod_p, mod_nb)
AIC(mod_p)
AIC(mod_nb)
BIC(mod_p)
BIC(mod_nb)
str(education_DOD)

#overall it's better nb, but we don't have overdispersion if we run stratified by CD.
#testing code for one CD
test_dod<-education_DOD %>%
  filter(CD=="CD 02")
summary(mod_test_dod<-glm(as.integer(DOD)~EDUC+offset(log(Population/10000)), family="poisson", data=test_dod))
stargazer(mod_test_dod, apply.coef = exp, apply.ci=exp, type='text')
confint(mod_test_dod)
table(education_DOD$EDUC)
CI_estim_rate_dod<-education_DOD%>%
  group_by(CONGRESS2, CD, AGE_CAT_EDUC, SEX)%>%
  group_modify(~{
    mod_test_dod<-glm(as.integer(DOD)~EDUC + 
                        offset(log(Population/10000)), family="poisson", data=.x)%>%tidy
  })
rel_disp_educ<-CI_estim_rate_dod%>%
  mutate(lci=exp(estimate-1.96*std.error),
         uci=exp(estimate+1.96*std.error),
         exp_est=exp(estimate))%>%
  select(CONGRESS2, CD, AGE_CAT_EDUC, SEX, term, exp_est, lci, uci)%>%
  arrange(AGE_CAT_EDUC, SEX, CONGRESS2, CD)%>%
  mutate(across(where(is.numeric), round, 2),
         educ=case_when(term=="EDUCLess than High School" ~"LHS",
                             term=="EDUCHigh School"~"HS",
                             term=="EDUCSome College/Associate Degree"~"SCAD"))%>%
  filter(!term%in%c("(Intercept)"))%>%
  select(-term)%>%
  #combine the estimate and CI's into one row
  unite("output", exp_est:lci, sep=" (")%>%
  unite("output", output:uci, sep=" , ")%>%
  mutate(output = paste0(output, ")"))%>%
  pivot_wider(names_from=educ, values_from=c(output)) %>%
  pivot_wider(names_from = CONGRESS2, values_from = c(LHS, HS, SCAD))%>%
  relocate(2,3,1,4,6,8,5,7,9)
#write to csv
write.csv(rel_disp_educ, "Final Results/rel_disp_educ.csv")
