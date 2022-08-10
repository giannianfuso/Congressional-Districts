library(MASS)
library(stargazer)

#IMR by Race/Ethnicity
IMR_byCD_byRace_111_112_pois <- glm(data = filter(IMR_byCD_byRace_2_details, as.integer(CONGRESS2) == 1), 
                                    family = "poisson",
                                    formula = as.integer(InfantMortality) ~ relevel(RACEHISP, ref = 'Non-Hispanic White')
                                    +as.integer(CD)+offset(log(LiveBirths)))
IMR_byCD_byRace_111_112_pois
summary(IMR_byCD_byRace_111_112_pois)
predict(IMR_byCD_byRace_111_112_pois,)

IMR_byCD_byRace_111_112_pois_nb <- glm.nb(data = filter(IMR_byCD_byRace_2_details, as.integer(CONGRESS2) == 1),
                                          formula = as.integer(InfantMortality) ~ relevel(RACEHISP, ref = 'Non-Hispanic White')
                                          + CD + offset(log(LiveBirths)))
summary(IMR_byCD_byRace_111_112_pois_nb)
stargazer(IMR_byCD_byRace_111_112_pois_nb, type='text')
predict(IMR_byCD_byRace_111_112_pois_nb, newdata = filter(IMR_byCD_byRace_2_details, as.integer(CONGRESS2) == 1),interval = 'confidence')


DOD_byEducSexCD_111_1112_pois <- glm(data = filter(DOD_byEducSexCD_deaths_2, as.integer(CONGRESS2) == 1), family = "poisson",
                                     formula = DOD ~ CD + SEX + EDUC + AGE_CAT_EDUC)
summary(DOD_byEducSexCD_111_1112_pois)


summary(Amod_4_psl18d<-glm.nb(count_vax_18_65 ~ psl.law:pop_18_65 + vax_period:pop_18_65 + pop_18_65 , data=vax_period_psl, link="identity"))
stargazer(Amod_4_psl18d, apply.coef=multiply.by.100, type='text')

rci_Amod_4_psl18d<-multiply.by.100(coefci(Amod_4_psl18d, vcov=vcovHC(Amod_4_psl18d,type="HC1",cluster=~State_Fips)))
rci_Amod_4_psl18d

################################################################################

#Bayes
Y_IMR_111_112 <- IMR_byCD_byRace_2_details %>%
  filter(as.integer(CONGRESS2) == 1) %>%
  arrange(CD, RACEHISP) %>%
  ungroup() %>%
  dplyr::select(InfantMortality) %>%
  as.matrix()

N_IMR_111_112 <- IMR_byCD_byRace_2_details %>%
  filter(as.integer(CONGRESS2) == 1) %>%
  arrange(CD, RACEHISP) %>%
  ungroup() %>%
  dplyr::select(LiveBirths) %>%
  as.matrix()
