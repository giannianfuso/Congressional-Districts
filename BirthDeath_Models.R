
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

CI_estim_rate_direct <- IMR_byCD_byRace_relDisparity_2 %>%
  rename(term = RACEHISP) %>%
  mutate(
    term = case_when( 
      term == "Non-Hispanic White" ~ "(Intercept)",
      term == "Hispanic" ~ "RACEHISPHispanic",
      term == "Non-Hispanic Black" ~ "RACEHISPNon-Hispanic Black",
      term == "Non-Hispanic Asian American Pacific Islander" ~ "RACEHISPNon-Hispanic Asian American Pacific Islander",
      term == "Non-Hispanic Other" ~ "RACEHISPNon-Hispanic Other"
    )
  ) %>% 
  select(-paired) %>%
  inner_join(CI_estim_rate)

rel_disp_race<-CI_estim_rate%>%
  mutate(lci=format(round(exp(estimate-1.96*std.error),2),nsmall=2),
         uci=format(round(exp(estimate+1.96*std.error),2),nsmall=2),
         exp_est=format(round(exp(estimate),2),nsmall=2))%>%
  select(CONGRESS2, CD, term, exp_est, lci, uci)%>%
  arrange(CONGRESS2, CD)%>%
  mutate(race_ethn=case_when(term=="RACEHISPHispanic" ~"H",
                             term=="RACEHISPNon-Hispanic Black"~"NHB",
                             term=="RACEHISPNon-Hispanic Asian American Pacific Islander"~"NHAAPI",
                             term=="RACEHISPNon-Hispanic Other"~"other"))%>%
  filter(!term%in%c("(Intercept)", "RACEHISPNon-Hispanic Other"))%>%
  select(-term)%>%
  #combine the estimate and CI's into one row
  unite("output", exp_est:lci, sep=" (")%>%
  unite("output", output:uci, sep=" , ")%>%
  mutate(output = paste0(output, ")"))%>%
  pivot_wider(names_from=race_ethn, values_from=c(output)) %>%
  pivot_wider(names_from = CONGRESS2, values_from = c(H, NHB, NHAAPI)) %>%
  relocate(1,2,4,6,3,5,7)
#write to csv
write.csv(rel_disp_race, "Final Results/rel_disp_race.csv", row.names = F)

rel_disp_race_direct<-CI_estim_rate_direct%>%
  mutate(lci=format(round(exp(estimate-1.96*std.error),2),nsmall = 2),
         uci=format(round(exp(estimate+1.96*std.error),2),nsmall = 2),
         IMR=format(round(IMR,2),nsmall = 2))%>%
  select(CONGRESS2, CD, term, IMR, lci, uci)%>%
  arrange(CONGRESS2, CD)%>%
  mutate(race_ethn=case_when(term=="RACEHISPHispanic" ~"H",
                             term=="RACEHISPNon-Hispanic Black"~"NHB",
                             term=="RACEHISPNon-Hispanic Asian American Pacific Islander"~"NHAAPI",
                             term=="RACEHISPNon-Hispanic Other"~"other"))%>%
  filter(!term%in%c("(Intercept)", "RACEHISPNon-Hispanic Other"))%>%
  select(-term)%>%
  #combine the estimate and CI's into one row
  unite("output", IMR:lci, sep=" (")%>%
  unite("output", output:uci, sep=", ")%>%
  mutate(output = paste0(output, ")"))%>%
  pivot_wider(names_from=race_ethn, values_from=c(output)) %>%
  pivot_wider(names_from = CONGRESS2, values_from = c(H, NHB, NHAAPI)) %>%
  relocate(1,2,4,6,3,5,7)
#write to csv
write.csv(rel_disp_race_direct, "Final Results/rel_disp_race_direct.csv", row.names = F)


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

CI_estim_rate_dod_direct <- DOD_byEducSexCD_relDisparity_2 %>%
  rename(term = EDUC) %>%
  mutate(
    term = case_when( 
      term == "Bachelor/Master/Doctorate/Professional Degree" ~ "(Intercept)",
      term == "Less than High School" ~ "EDUCLess than High School",
      term == "High School" ~ "EDUCHigh School",
      term == "Some College/Associate Degree" ~ "EDUCSome College/Associate Degree"
    )
  ) %>% 
  select(-c(paired, MR, MR_college)) %>%
  inner_join(CI_estim_rate_dod)

rel_disp_educ<-CI_estim_rate_dod%>%
  mutate(lci=format(round(exp(estimate-1.96*std.error),2),nsmall = 2),
         uci=format(round(exp(estimate+1.96*std.error),2),nsmall = 2),
         exp_est=format(round(exp(estimate),2),nsmall = 2)) %>%
  select(CONGRESS2, CD, AGE_CAT_EDUC, SEX, term, exp_est, lci, uci)%>%
  arrange(AGE_CAT_EDUC, SEX, CONGRESS2, CD)%>%
  mutate(educ=case_when(term=="EDUCLess than High School" ~"LHS",
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
write.csv(rel_disp_educ, "Final Results/rel_disp_educ.csv", row.names = F)

rel_disp_educ_direct<-CI_estim_rate_dod_direct%>%
  mutate(lci=format(round(exp(estimate-1.96*std.error),2),nsmall = 2),
         uci=format(round(exp(estimate+1.96*std.error),2),nsmall = 2),
         MR_relDisparity=format(round(MR_relDisparity,2),nsmall = 2))%>%
  select(CONGRESS2, CD, AGE_CAT_EDUC, SEX, term, MR_relDisparity, lci, uci)%>%
  arrange(AGE_CAT_EDUC, SEX, CONGRESS2, CD)%>%
  mutate(educ=case_when(term=="EDUCLess than High School" ~"LHS",
                        term=="EDUCHigh School"~"HS",
                        term=="EDUCSome College/Associate Degree"~"SCAD"))%>%
  filter(!term%in%c("(Intercept)"))%>%
  select(-term)%>%
  #combine the estimate and CI's into one row
  unite("output", MR_relDisparity:lci, sep=" (")%>%
  unite("output", output:uci, sep=", ")%>%
  mutate(output = paste0(output, ")"))%>%
  pivot_wider(names_from=educ, values_from=c(output)) %>%
  pivot_wider(names_from = CONGRESS2, values_from = c(LHS, HS, SCAD))%>%
  relocate(2,3,1,4,6,8,5,7,9)

#write to csv
write.csv(rel_disp_educ_direct, "Final Results/rel_disp_educ_direct.csv", row.names = F)
