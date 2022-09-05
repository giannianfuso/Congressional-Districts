
library(MASS)
library(stargazer)
library(lmtest)
library(glm2)
library(broom)
library(tidyverse)
library(addreg)
library(mgcv)
library(fastglm)
library(base)
library(broom)
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
# test_imr<-race_IMR %>%
#   filter(CD=="CD 02")
# summary(mod_test_im<-glm(im~RACEHISP+offset(log(LiveBirths/1000)), family="poisson", data=test_imr))
# stargazer(mod_test_im, apply.coef = exp, apply.ci=exp, type='text')
# confint(mod_test_im)
# table(race_IMR$RACEHISP)

CI_estim_imr<-race_IMR%>%
  group_by(CONGRESS2, CD)%>%
  group_modify(~{
    mod_test_im<-glm(InfantMortality~RACEHISP+offset(log(LiveBirths/1000)),family="poisson", data=.x)%>%tidy
  })

rel_disp_imr_noCI<-CI_estim_imr%>%
  mutate(exp_est=format(round(exp(estimate),2),nsmall=2))%>%
  select(CONGRESS2, CD, term, exp_est)%>%
  arrange(CONGRESS2, CD)%>%
  mutate(race_ethn=case_when(term=="RACEHISPHispanic" ~"H",
                             term=="RACEHISPNon-Hispanic Black"~"NHB",
                             term=="RACEHISPNon-Hispanic Asian American Pacific Islander"~"NHAAPI",
                             term=="RACEHISPNon-Hispanic Other"~"other"))%>%
  filter(!term%in%c("(Intercept)", "RACEHISPNon-Hispanic Other"))%>%
  select(-term)%>%
  pivot_wider(names_from=race_ethn, values_from=c(exp_est)) %>%
  pivot_wider(names_from = CONGRESS2, values_from = c(H, NHB, NHAAPI)) %>%
  relocate(1,2,4,6,3,5,7)

#write to csv
write.csv(rel_disp_imr_noCI, "Final Results/rel_disp_imr_noCI.csv", row.names = F)

rel_disp_imr<-CI_estim_imr%>%
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
  unite("output", output:uci, sep=", ")%>%
  mutate(output = paste0(output, ")"))%>%
  pivot_wider(names_from=race_ethn, values_from=c(output)) %>%
  pivot_wider(names_from = CONGRESS2, values_from = c(H, NHB, NHAAPI)) %>%
  relocate(1,2,4,6,3,5,7)

#write to csv
write.csv(rel_disp_imr, "Final Results/rel_disp_imr.csv", row.names = F)

CI_estim_rate_imr_add_fast<-race_IMR%>%
  group_by(CONGRESS2, CD)%>%
  group_modify(~{
    pop<-.x$LiveBirths/1000
    x<-model.matrix(im~RACEHISP:pop+pop, data = .x)
    y<-.x$im
    additive_im<-fastglm(x=x, y=y, family="poisson"(link="identity"))[["coefficients"]]%>%tidy%>%rename(coef=x)%>%
      bind_cols({
        fastglm(x=x, y=y, family="poisson"(link="identity"))[["se"]]%>%tidy%>%rename(se=x)
      })
  }) %>%
  filter(names!="(Intercept)", names!="pop", names!="RACEHISPNon-Hispanic Other:pop", names != "RACEHISPNon-Hispanic American Indian, Alaska Native:pop",
         names!="RACEHISPUnknown:pop")

#Clean for charts
CI_estim_rate_imr_add_fast_charts <- CI_estim_rate_imr_add_fast %>%
  mutate(RACEHISP=substring(names,9,nchar(names)-4), paired = as.integer(CD))%>%
  rename(IMR=coef)%>%
  select(CD, CONGRESS2, RACEHISP, IMR, paired)

#Save as R Object
save(CI_estim_rate_imr_add_fast_charts, file = "./ShinyApp/CI_estim_rate_imr_add_fast_charts.Rdata")

abs_disp_imr_noCI<-CI_estim_rate_imr_add_fast%>%
  mutate(est=format(round(coef,2),nsmall=2))%>%
  select(CONGRESS2, CD, names, est)%>%
  arrange(CONGRESS2, CD)%>%
  mutate(race_ethn=case_when(names=="RACEHISPHispanic:pop" ~"H",
                             names=="RACEHISPNon-Hispanic Black:pop"~"NHB",
                             names=="RACEHISPNon-Hispanic Asian American Pacific Islander:pop"~"NHAAPI"))%>%
  filter(!names%in%c("(Intercept)", "pop", "RACEHISPNon-Hispanic Other:pop"))%>%
  select(-names)%>%
  pivot_wider(names_from=race_ethn, values_from=c(est)) %>%
  pivot_wider(names_from = CONGRESS2, values_from = c(H, NHB, NHAAPI)) %>%
  relocate(1,2,4,6,3,5,7)

#write to csv
write.csv(abs_disp_imr_noCI, "Final Results/abs_disp_imr_noCI.csv", row.names = F)

abs_disp_imr<-CI_estim_rate_imr_add_fast%>%
  mutate(lci=format(round(coef-1.96*se,2),nsmall=2),
         uci=format(round(coef+1.96*se,2),nsmall=2),
         est=format(round(coef,2),nsmall=2))%>%
  select(CONGRESS2, CD, names, est, lci, uci)%>%
  arrange(CONGRESS2, CD)%>%
  mutate(race_ethn=case_when(names=="RACEHISPHispanic:pop" ~"H",
                             names=="RACEHISPNon-Hispanic Black:pop"~"NHB",
                             names=="RACEHISPNon-Hispanic Asian American Pacific Islander:pop"~"NHAAPI"))%>%
  filter(!names%in%c("(Intercept)", "pop", "RACEHISPNon-Hispanic Other:pop"))%>%
  select(-names)%>%
  #combine the estimate and CI's into one row
  unite("output", est:lci, sep=" (")%>%
  unite("output", output:uci, sep=", ")%>%
  mutate(output = paste0(output, ")"))%>%
  pivot_wider(names_from=race_ethn, values_from=c(output)) %>%
  pivot_wider(names_from = CONGRESS2, values_from = c(H, NHB, NHAAPI)) %>%
  relocate(1,2,4,6,3,5,7)

#write to csv
write.csv(abs_disp_imr, "Final Results/abs_disp_imr.csv", row.names = F)


################################################################################

#IMR by Race with cd116 boundaries

#rename dataset to make easier to use
race_IMR_cd116<-IMR_byCD_byRace_2_cd116_details%>%
  mutate(im=as.integer(InfantMortality))

CI_estim_rate_imr_add_fast_cd116<-race_IMR_cd116%>%
  group_by(CONGRESS2, CD)%>%
  group_modify(~{
    pop<-.x$LiveBirths/1000
    x<-model.matrix(im~RACEHISP:pop+pop, data = .x)
    y<-.x$im
    additive_im<-fastglm(x=x, y=y, family="poisson"(link="identity"))[["coefficients"]]%>%tidy%>%rename(coef=x)%>%
      bind_cols({
        fastglm(x=x, y=y, family="poisson"(link="identity"))[["se"]]%>%tidy%>%rename(se=x)
      })
  }) %>%
  filter(names!="(Intercept)", names!="pop", names!="RACEHISPNon-Hispanic Other:pop", names != "RACEHISPNon-Hispanic American Indian, Alaska Native:pop",
         names!="RACEHISPUnknown:pop") %>%
  mutate(RACEHISP=substring(names,9,nchar(names)-4), paired = as.integer(CD))%>%
  rename(IMR=coef)%>%
  select(CD, CONGRESS2, RACEHISP, IMR, se, paired)

#Save as R Object
save(CI_estim_rate_imr_add_fast_cd116, file = "./ShinyApp/CI_estim_rate_imr_add_fast_cd116.Rdata")

abs_disp_imr_cd116<-CI_estim_rate_imr_add_fast_cd116%>%
  mutate(lci=format(round(IMR-1.96*se,2),nsmall=2),
         uci=format(round(IMR+1.96*se,2),nsmall=2),
         est=format(round(IMR,2),nsmall=2))%>%
  select(CONGRESS2, CD,RACEHISP,est, lci, uci)%>%
  arrange(CONGRESS2, CD)%>%
  mutate(race_ethn=case_when(RACEHISP=="Hispanic" ~"H",
                             RACEHISP=="Non-Hispanic Black"~"NHB",
                             RACEHISP=="Non-Hispanic Asian American Pacific Islander"~"NHAAPI"))%>%
  #combine the estimate and CI's into one row
  unite("output", est:lci, sep=" (")%>%
  unite("output", output:uci, sep=", ")%>%
  mutate(output = paste0(output, ")"))%>%
  pivot_wider(names_from=race_ethn, values_from=c(output)) %>%
  pivot_wider(names_from = CONGRESS2, values_from = c(H, NHB, NHAAPI)) %>%
  relocate(1,2,4,6,3,5,7)

################################################################################


#DOD MR by Education
#test Poisson vs Negative binomial
#rename dataset to make easier to use
education_DOD<-DOD_byEducSexCD_2_details %>%
  mutate(DOD = as.integer(DOD),
         EDUC = factor(EDUC, levels = c("Bachelor/Master/Doctorate/Professional Degree", "Less than High School", 
                          "High School", "Some College/Associate Degree", "Unknown")))

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
summary(mod_test_dod<-glm(DOD~EDUC:Population+Population, family="poisson"(link="identity"), data=test_dod))
stargazer(mod_test_dod, apply.coef = exp, apply.ci=exp, type='text')
confint(mod_test_dod)
table(education_DOD$EDUC)

CI_estim_rate_dod<-education_DOD%>%
  group_by(CONGRESS2, CD, AGE_CAT_EDUC, SEX)%>%
  group_modify(~{
    mod_test_dod<-glm(DOD~EDUC + 
                        offset(log(Population/10000)), family="poisson", data=.x)%>%tidy
  })

rel_disp_educ_noCI<-CI_estim_rate_dod%>%
  mutate(exp_est=format(round(exp(estimate),2),nsmall = 2)) %>%
  select(CONGRESS2, CD, AGE_CAT_EDUC, SEX, term, exp_est)%>%
  arrange(AGE_CAT_EDUC, SEX, CONGRESS2, CD)%>%
  mutate(educ=case_when(term=="EDUCLess than High School" ~"LHS",
                        term=="EDUCHigh School"~"HS",
                        term=="EDUCSome College/Associate Degree"~"SCAD"))%>%
  filter(!term%in%c("(Intercept)"))%>%
  select(-term)%>%
  pivot_wider(names_from=educ, values_from=c(exp_est)) %>%
  pivot_wider(names_from = CONGRESS2, values_from = c(LHS, HS, SCAD))

#write to csv
write.csv(rel_disp_educ_noCI, "Final Results/rel_disp_educ_noCI.csv", row.names = F)

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
  unite("output", output:uci, sep=", ")%>%
  mutate(output = paste0(output, ")"))%>%
  pivot_wider(names_from=educ, values_from=c(output)) %>%
  pivot_wider(names_from = CONGRESS2, values_from = c(LHS, HS, SCAD))%>%
  relocate(2,3,1,4,6,8,5,7,9)

#write to csv
write.csv(rel_disp_educ, "Final Results/rel_disp_educ.csv", row.names = F)

CI_estim_rate_dod_add_fast<-education_DOD%>%
  group_by(CONGRESS2, CD, AGE_CAT_EDUC, SEX)%>%
  group_modify(~{
    pop<-.x$Population/10000
    x<-model.matrix(DOD~EDUC:pop+pop, data = .x)
    y<-.x$DOD
    add_dodmr_df<-fastglm(x=x, y=y, family="poisson"(link="identity"))[["coefficients"]]%>%tidy%>%rename(coef=x)%>%
      bind_cols({
        fastglm(x=x, y=y, family="poisson"(link="identity"))[["se"]]%>%tidy%>%rename(se=x)
      })
  })%>%
  filter(names!="(Intercept)", names!="pop", names!="EDUCUnknown:pop")

#Clean for charts
CI_estim_rate_dod_add_fast_charts <- CI_estim_rate_dod_add_fast %>%
  mutate(EDUC=substring(names,5,nchar(names)-4), paired = as.integer(CD))%>%
  rename(MR=coef)%>%
  select(CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC, MR, paired)

#Save for charts
save(CI_estim_rate_dod_add_fast_charts, file = "./ShinyApp/CI_estim_rate_dod_add_fast_charts.Rdata")

CI_estim_rate_dod_add_fast_AIC<-education_DOD%>%
  group_by(CONGRESS2, CD, AGE_CAT_EDUC, SEX)%>%
  group_modify(~{
    pop<-.x$Population/10000
    x<-model.matrix(DOD~EDUC:pop+pop, data = .x)
    y<-.x$DOD
    add_dodmr_df<-fastglm(x=x, y=y, family="poisson"(link="identity"))[["aic"]]%>%tidy%>%rename(AIC=x)
  })

abs_disp_dod_noCI<-CI_estim_rate_dod_add_fast%>%
  mutate(est=format(round(coef,2),nsmall = 2)) %>%
  select(CONGRESS2, CD, AGE_CAT_EDUC, SEX, names, est)%>%
  arrange(AGE_CAT_EDUC, SEX, CONGRESS2, CD)%>%
  mutate(educ=case_when(names=="EDUCLess than High School:pop" ~"LHS",
                        names=="EDUCHigh School:pop"~"HS",
                        names=="EDUCSome College/Associate Degree:pop"~"SCAD"))%>%
  select(-names)%>%
  pivot_wider(names_from=educ, values_from=c(est)) %>%
  pivot_wider(names_from = CONGRESS2, values_from = c(LHS, HS, SCAD))%>%
  relocate(2,3,1,4,6,8,5,7,9)

#write to csv
write.csv(abs_disp_dod_noCI, "Final Results/abs_disp_dod_noCI.csv", row.names = F)

abs_disp_dod_educ<-CI_estim_rate_dod_add_fast%>%
  mutate(lci=format(round(coef-1.96*se,2),nsmall = 2),
         uci=format(round(coef+1.96*se,2),nsmall = 2),
         est=format(round(coef,2),nsmall = 2)) %>%
  select(CONGRESS2, CD, AGE_CAT_EDUC, SEX, names, est, lci, uci)%>%
  arrange(AGE_CAT_EDUC, SEX, CONGRESS2, CD)%>%
  mutate(educ=case_when(names=="EDUCLess than High School:pop" ~"LHS",
                        names=="EDUCHigh School:pop"~"HS",
                        names=="EDUCSome College/Associate Degree:pop"~"SCAD"))%>%
  select(-names)%>%
  #combine the estimate and CI's into one row
  unite("output", est:lci, sep=" (")%>%
  unite("output", output:uci, sep=", ")%>%
  mutate(output = paste0(output, ")"))%>%
  pivot_wider(names_from=educ, values_from=c(output)) %>%
  pivot_wider(names_from = CONGRESS2, values_from = c(LHS, HS, SCAD))%>%
  relocate(2,3,1,4,6,8,5,7,9)

#write to csv
write.csv(abs_disp_dod_educ, "Final Results/abs_disp_dod_educ.csv", row.names = F)
################################################################################

#DOD by education - cd116
education_DOD_cd116<-DOD_byEducSexCD_2_cd116_orig %>%
  mutate(DOD = as.integer(DOD),
         EDUC = factor(EDUC, levels = c("Bachelor/Master/Doctorate/Professional Degree", "Less than High School", 
                                        "High School", "Some College/Associate Degree", "Unknown")))

#Regression
CI_estim_rate_dod_add_fast_cd116<-education_DOD_cd116%>%
  group_by(CONGRESS2, CD, AGE_CAT_EDUC, SEX)%>%
  group_modify(~{
    pop<-.x$Population/10000
    x<-model.matrix(DOD~EDUC:pop+pop, data = .x)
    y<-.x$DOD
    add_dodmr_df<-fastglm(x=x, y=y, family="poisson"(link="identity"))[["coefficients"]]%>%tidy%>%rename(coef=x)%>%
      bind_cols({
        fastglm(x=x, y=y, family="poisson"(link="identity"))[["se"]]%>%tidy%>%rename(se=x)
      })
  }) %>%
  filter(names!="(Intercept)", names!="pop", names!="EDUCUnknown:pop")%>%
  mutate(EDUC=substring(names,5,nchar(names)-4), paired=as.integer(CD))%>%
  rename(MR=coef)%>%
  select(CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC, MR, paired)

#Save for charts
save(CI_estim_rate_dod_add_fast_cd116, file = "./ShinyApp/CI_estim_rate_dod_add_fast_cd116.Rdata")

################################################################################

#DOD by Race

#Rename
race_DOD<-DOD_byRaceSexCD_2_details %>%
  mutate(DOD = as.integer(DOD))

#Regression by Race
CI_estim_rate_dod_race_add<-race_DOD%>%
  group_by(CONGRESS2, CD, AGE_CAT_EDUC, SEX)%>%
  group_modify(~{
    pop<-.x$Population/10000
    x<-model.matrix(DOD~RACE:pop+pop, data = .x)
    y<-.x$DOD
    add_dodmr_df<-fastglm(x=x, y=y, family="poisson"(link="identity"))[["coefficients"]]%>%tidy%>%rename(coef=x)
  })%>%
  filter(names!="(Intercept)", names!="pop", names!="RACEOther:pop", names != "RACEAmerican Indian, Alaska Native:pop") %>%
  mutate(RACE=substring(names,5,nchar(names)-4), paired = as.integer(CD))%>%
  rename(MR=coef)%>%
  select(CD, CONGRESS2, RACE, AGE_CAT_EDUC, SEX, MR, paired)

#Save as R Object
save(CI_estim_rate_dod_race_add, file = "./ShinyApp/CI_estim_rate_dod_race_add.Rdata")

################################################################################

#DOD by Race - cd116

#Rename
race_DOD_cd116<-DOD_byRaceSexCD_2_cd116_details %>%
  mutate(DOD = as.integer(DOD))

#Regression by Race - cd116
CI_estim_rate_dod_race_add_cd116<-race_DOD_cd116%>%
  group_by(CONGRESS2, CD, AGE_CAT_EDUC, SEX)%>%
  group_modify(~{
    pop<-.x$Population/10000
    x<-model.matrix(DOD~RACE:pop+pop, data = .x)
    y<-.x$DOD
    add_dodmr_df<-fastglm(x=x, y=y, family="poisson"(link="identity"))[["coefficients"]]%>%tidy%>%rename(coef=x)
  })%>%
  filter(names!="(Intercept)", names!="pop", names!="RACEOther:pop", names != "RACEAmerican Indian, Alaska Native:pop") %>%
  mutate(RACE=substring(names,5,nchar(names)-4), paired = as.integer(CD))%>%
  rename(MR=coef)%>%
  select(CD, CONGRESS2, RACE, AGE_CAT_EDUC, SEX, MR, paired)

#Save as R Object
save(CI_estim_rate_dod_race_add_cd116, file = "./ShinyApp/CI_estim_rate_dod_race_add_cd116.Rdata")
