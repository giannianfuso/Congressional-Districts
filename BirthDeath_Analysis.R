library(tidyverse)
library(foreign)
library(tidycensus)
library(data.table)
library(stringi)
library(stringr) 
library(sf)
library(ggmap)
library(tigris)
library(leaflet)
library(scico)
library(ragg)
library("writexl")
library(highcharter)
library(htmlwidgets)
library(ggalluvial)
library(sp)
library(plotly)
library(dineq)
library(ggh4x)


################################################################################

results_folder <- "C:\\Users\\ga437\\OneDrive - Drexel University\\Congressional_Districts_BRFSS\\Anfuso_Results\\"

################################################################################
#Color themes:

color_AAPI <- "#50bde9"
color_AmericanIndian <- "#009e74"
color_Black <- "#d55e00"
color_Hispanic <- "#cc79a7"
color_White <- "#f0e442"

color_LessHS <- "#cc79a7"
color_HS <- "#009e73"
color_SomeCollege <- "#0072b2"
color_Bachelor <- "#f0e442"

################################################################################

#Infant Mortality by CD
infMort_byCD_byYear <- group_by(death_cd, CONGRESS, CD, year) %>%
  summarise(InfantMortality = sum(INFMORT_CD, na.rm = TRUE))

infMort_byCD <- group_by(death_cd, CONGRESS, CD) %>%
  summarise(InfantMortality = sum(INFMORT_CD, na.rm = TRUE))

#Combine congresses
infMort_byCD_2 <- group_by(death_cd, CONGRESS2, CD) %>%
  summarise(InfantMortality = sum(INFMORT_CD, na.rm = TRUE))

#Live Births by CD
liveBirths_byYear <- group_by(birth_cd, CONGRESS, CD, year) %>%
  summarise(LiveBirths = sum(ALLOCATION))

liveBirths_byCD <- group_by(birth_cd, CONGRESS, CD) %>%
  summarise(LiveBirths = sum(ALLOCATION))

#Combine congresses
liveBirths_byCD_2 <- group_by(birth_cd, CONGRESS2, CD) %>%
  summarise(LiveBirths = sum(ALLOCATION))

#IMR by CD
IMR_byCD <- inner_join(infMort_byCD, liveBirths_byCD) %>%
  mutate(IMR = InfantMortality/LiveBirths * 1000)

#IMR by CD - Combined Congresses
IMR_byCD_2 <- inner_join(infMort_byCD_2, liveBirths_byCD_2) %>%
  mutate(IMR = InfantMortality/LiveBirths * 1000)

################################################################################

#Infant Mortality by CD & Race
infMort_byCD_byRace <- group_by(death_cd, CONGRESS, CD, RACEHISP) %>%
  summarise(InfantMortality = sum(INFMORT_CD, na.rm = TRUE)) %>%
  filter(as.integer(RACEHISP)!=4) #Remove american indian alaska native

#Combine Congresses
infMort_byCD_byRace_2 <- group_by(death_cd, CONGRESS2, CD, RACEHISP) %>%
  summarise(InfantMortality = sum(INFMORT_CD, na.rm = TRUE)) %>%
  filter(as.integer(RACEHISP)!=4) #Remove american indian alaska native

#Combine Congresses - cd116 boundaries
infMort_byCD_byRace_2_cd116 <- group_by(death_cd116, CONGRESS2, CD, RACEHISP) %>%
  summarise(InfantMortality = sum(INFMORT_CD, na.rm = TRUE)) %>%
  filter(as.integer(RACEHISP)!=4) #Remove american indian alaska native

#Live Births by CD & Race
liveBirths_byCD_byRace <- group_by(birth_cd, CONGRESS, CD, RACEHISP) %>%
  summarise(LiveBirths = sum(ALLOCATION)) %>%
  filter(as.integer(RACEHISP)!=4) #Remove american indian alaska native

#Live Births by CD & Race - Combine Congresses
liveBirths_byCD_byRace_2 <- group_by(birth_cd, CONGRESS2, CD, RACEHISP) %>%
  summarise(LiveBirths = sum(ALLOCATION)) %>%
  filter(as.integer(RACEHISP)!=4) #Remove american indian alaska native

#Live Births by CD & Race - Combine Congresses - cd116 boundaries
liveBirths_byCD_byRace_2_cd116 <- group_by(birth_cd116, CONGRESS2, CD, RACEHISP) %>%
  summarise(LiveBirths = sum(ALLOCATION)) %>%
  filter(as.integer(RACEHISP)!=4) #Remove american indian alaska native

#IMR by CD & Race
IMR_byCD_byRace <- inner_join(infMort_byCD_byRace, liveBirths_byCD_byRace) %>%
  mutate(IMR = InfantMortality/LiveBirths * 1000)

#Combine Congresses
IMR_byCD_byRace_2 <- inner_join(infMort_byCD_byRace_2, liveBirths_byCD_byRace_2) %>%
  mutate(IMR = InfantMortality/LiveBirths * 1000, paired = as.integer(CD)) %>%
  select(-c(InfantMortality, LiveBirths))

#Save as R Object
save(IMR_byCD_byRace_2, file = "./ShinyApp/IMR_byCD_byRace_2.Rdata")

#All details for regression
IMR_byCD_byRace_2_details <- inner_join(infMort_byCD_byRace_2, liveBirths_byCD_byRace_2) %>%
  filter(as.integer(RACEHISP) != 7) %>%
  mutate(IMR = InfantMortality/LiveBirths * 1000, paired = as.integer(CD), 
         RACEHISP = relevel(RACEHISP, ref = 'Non-Hispanic White'))

#Combine Congresses - cd116 boundaries
IMR_byCD_byRace_2_cd116_orig <- inner_join(infMort_byCD_byRace_2_cd116, liveBirths_byCD_byRace_2_cd116) %>%
  mutate(IMR = InfantMortality/LiveBirths * 1000, paired = as.integer(CD))

IMR_byCD_byRace_2_cd116 <- inner_join(infMort_byCD_byRace_2_cd116, liveBirths_byCD_byRace_2_cd116) %>%
  mutate(IMR = InfantMortality/LiveBirths * 1000, paired = as.integer(CD)) %>%
  select(-c(InfantMortality, LiveBirths))

#Save as R Object
save(IMR_byCD_byRace_2_cd116, file = "./ShinyApp/IMR_byCD_byRace_2_cd116.Rdata")

################################################################################

#Infant Mortality by CD - Non-Hispanic White
infMort_byCD_byRace_nhw <- group_by(filter(death_cd, as.integer(RACEHISP) ==2), CONGRESS, CD, RACEHISP) %>%
  summarise(InfantMortality = sum(INFMORT_CD, na.rm = TRUE))

#Combined Congresses
infMort_byCD_byRace_nhw_2 <- group_by(filter(death_cd, as.integer(RACEHISP) ==2), CONGRESS2, CD, RACEHISP) %>%
  summarise(InfantMortality = sum(INFMORT_CD, na.rm = TRUE))

#Combined Congresses - cd116 boundaries
infMort_byCD_byRace_nhw_2_cd116 <- group_by(filter(death_cd116, as.integer(RACEHISP) ==2), CONGRESS2, CD, RACEHISP) %>%
  summarise(InfantMortality = sum(INFMORT_CD, na.rm = TRUE))

#Live Births by CD & Race - Non-Hispanic White
liveBirths_byCD_byRace_nhw <- group_by(filter(birth_cd, as.integer(RACEHISP)==2), CONGRESS, CD, RACEHISP) %>%
  summarise(LiveBirths = sum(ALLOCATION))

#Combined Congresses
liveBirths_byCD_byRace_nhw_2 <- group_by(filter(birth_cd, as.integer(RACEHISP)==2), CONGRESS2, CD, RACEHISP) %>%
  summarise(LiveBirths = sum(ALLOCATION))

#Combined Congresses - cd116 boundaries
liveBirths_byCD_byRace_nhw_2_cd116 <- group_by(filter(birth_cd116, as.integer(RACEHISP)==2), CONGRESS2, CD, RACEHISP) %>%
  summarise(LiveBirths = sum(ALLOCATION))

#IMR by CD & Race - Non-Hispanic White
IMR_byCD_byRace_nhw <- inner_join(infMort_byCD_byRace_nhw, liveBirths_byCD_byRace_nhw) %>%
  mutate(IMR_nhw = InfantMortality/LiveBirths * 1000)

#Combined congresses
IMR_byCD_byRace_nhw_2 <- inner_join(infMort_byCD_byRace_nhw_2, liveBirths_byCD_byRace_nhw_2) %>%
  mutate(IMR_nhw = InfantMortality/LiveBirths * 1000)

#Combined congresses - cd116
IMR_byCD_byRace_nhw_2_cd116 <- inner_join(infMort_byCD_byRace_nhw_2_cd116, liveBirths_byCD_byRace_nhw_2_cd116) %>%
  mutate(IMR_nhw = InfantMortality/LiveBirths * 1000)

#Absolute disparity by CD and Race
IMR_byCD_byRace_absDisparity <- full_join(IMR_byCD_byRace, 
                                          IMR_byCD_byRace_nhw, 
                                          by = c("CONGRESS", "CD")) %>%
  select(-c("RACEHISP.y")) %>%
  rename(RACEHISP = RACEHISP.x) %>%
  mutate(IMR_absDisparity = (IMR-IMR_nhw))

#Combined Congresses
IMR_byCD_byRace_absDisparity_2 <- full_join(IMR_byCD_byRace_2, 
                                          IMR_byCD_byRace_nhw_2, 
                                          by = c("CONGRESS2", "CD")) %>%
  select(-c("RACEHISP.y")) %>%
  rename(RACEHISP = RACEHISP.x) %>%
  filter(as.integer(RACEHISP) != 7) %>%
  mutate(IMR_absDisparity = (IMR-IMR_nhw), paired = as.integer(CD)) %>%
  select(c(CONGRESS2, CD, RACEHISP, IMR_absDisparity, paired)) %>%
  rename(IMR = IMR_absDisparity)

#Save as R Object
save(IMR_byCD_byRace_absDisparity_2, file = "./ShinyApp/IMR_byCD_byRace_absDisparity_2.Rdata")

#Relative disparities
IMR_byCD_byRace_relDisparity_2 <- full_join(IMR_byCD_byRace_2, 
                                            IMR_byCD_byRace_nhw_2, 
                                            by = c("CONGRESS2", "CD")) %>%
  select(-c("RACEHISP.y")) %>%
  rename(RACEHISP = RACEHISP.x) %>%
  filter(as.integer(RACEHISP) != 7) %>%
  mutate(IMR_relDisparity = (IMR/IMR_nhw), paired = as.integer(CD)) %>%
  select(c(CONGRESS2, CD, RACEHISP, IMR_relDisparity, paired)) %>%
  rename(IMR = IMR_relDisparity)

#Create tables for paper
IMR_byCD_byRace_absDisparity_2_table <- IMR_byCD_byRace_absDisparity_2 %>%
  filter(as.integer(CONGRESS2) == 1, as.integer(RACEHISP) <= 6) %>%
  mutate(RACEHISP_CONGRESS = paste0(RACEHISP, " - ", CONGRESS2) %>%
           str_replace_all("Hispanic -", "H -") %>%
           str_replace_all("Non-Hispanic White", "NHW") %>%
           str_replace_all("Non-Hispanic Black", "NHB") %>%
           str_replace_all("Non-Hispanic Asian American Pacific Islander", "NHAAPI") %>%
           str_replace_all("Non-Hispanic Other", "NHO")) %>%
  pivot_wider(names_from = RACEHISP_CONGRESS, values_from = IMR, values_fill = 0) %>%
  ungroup() %>%
  select(-c(CONGRESS2, RACEHISP, paired)) %>%
  group_by(CD) %>%
  summarise_all(sum) %>%
  bind_cols({
    IMR_byCD_byRace_absDisparity_2 %>%
      filter(as.integer(CONGRESS2) == 2, as.integer(RACEHISP) <= 6) %>%
      mutate(RACEHISP_CONGRESS = paste0(RACEHISP, " - ", CONGRESS2) %>%
               str_replace_all("Hispanic -", "H -") %>%
               str_replace_all("Non-Hispanic White", "NHW") %>%
               str_replace_all("Non-Hispanic Black", "NHB") %>%
               str_replace_all("Non-Hispanic Asian American Pacific Islander", "NHAAPI") %>%
               str_replace_all("Non-Hispanic Other", "NHO")) %>%
      pivot_wider(names_from = RACEHISP_CONGRESS, values_from = IMR, values_fill = 0) %>%
      ungroup() %>%
      select(-c(CONGRESS2, RACEHISP, paired)) %>%
      add_row(CD = "CD 19") %>%
      group_by(CD) %>%
      summarise_all(sum)
  }) %>%
  select(-c(CD...7)) %>%
  rename(CD = CD...1) %>%
  relocate(1,3,2,4,5,6,8,7,9,10)

#Write to csv
write.csv(IMR_byCD_byRace_absDisparity_2_table, "./Final Results/IMR_byCD_byRace_absDisparity_2_table.csv", row.names = FALSE)

IMR_byCD_byRace_relDisparity_2_table <- IMR_byCD_byRace_relDisparity_2 %>%
  filter(as.integer(CONGRESS2) == 1, as.integer(RACEHISP) <= 6) %>%
  mutate(RACEHISP_CONGRESS = paste0(RACEHISP, " - ", CONGRESS2) %>%
           str_replace_all("Hispanic -", "H -") %>%
           str_replace_all("Non-Hispanic White", "NHW") %>%
           str_replace_all("Non-Hispanic Black", "NHB") %>%
           str_replace_all("Non-Hispanic Asian American Pacific Islander", "NHAAPI") %>%
           str_replace_all("Non-Hispanic Other", "NHO")) %>%
  pivot_wider(names_from = RACEHISP_CONGRESS, values_from = IMR, values_fill = 0) %>%
  ungroup() %>%
  select(-c(CONGRESS2, RACEHISP, paired)) %>%
  group_by(CD) %>%
  summarise_all(sum) %>%
  bind_cols({
    IMR_byCD_byRace_relDisparity_2 %>%
      filter(as.integer(CONGRESS2) == 2, as.integer(RACEHISP) <= 6) %>%
      mutate(RACEHISP_CONGRESS = paste0(RACEHISP, " - ", CONGRESS2) %>%
               str_replace_all("Hispanic -", "H -") %>%
               str_replace_all("Non-Hispanic White", "NHW") %>%
               str_replace_all("Non-Hispanic Black", "NHB") %>%
               str_replace_all("Non-Hispanic Asian American Pacific Islander", "NHAAPI") %>%
               str_replace_all("Non-Hispanic Other", "NHO")) %>%
      pivot_wider(names_from = RACEHISP_CONGRESS, values_from = IMR, values_fill = 0) %>%
      ungroup() %>%
      select(-c(CONGRESS2, RACEHISP, paired)) %>%
      add_row(CD = "CD 19") %>%
      group_by(CD) %>%
      summarise_all(sum)
  }) %>%
  select(-c(CD...7)) %>%
  rename(CD = CD...1) %>%
  relocate(1,3,2,4,5,6,8,7,9,10)

#Write to csv
write.csv(IMR_byCD_byRace_relDisparity_2_table, "./Final Results/IMR_byCD_byRace_relDisparity_2_table.csv", row.names = FALSE)

#Abs disparities - cd116 boundaries
IMR_byCD_byRace_absDisparity_2_cd116 <- full_join(IMR_byCD_byRace_2_cd116, 
                                            IMR_byCD_byRace_nhw_2_cd116, 
                                            by = c("CONGRESS2", "CD")) %>%
  select(-c("RACEHISP.y")) %>%
  rename(RACEHISP = RACEHISP.x) %>%
  mutate(IMR_absDisparity = (IMR-IMR_nhw)) %>%
  select(c(CONGRESS2, CD, RACEHISP, IMR_absDisparity, paired)) %>%
  rename(IMR = IMR_absDisparity)

#Save as R Object
save(IMR_byCD_byRace_absDisparity_2_cd116, file = "./ShinyApp/IMR_byCD_byRace_absDisparity_2_cd116.Rdata")

#MLD Calculations 
total_deaths_IMR<-IMR_byCD_byRace_2_cd116_orig%>%
  ungroup()%>%
  filter(CONGRESS2=="113 - 114 (2013 - 2015)")%>%
  mutate(CONGRESS="116 (2019-2020)")%>%
  select(CD, RACEHISP, InfantMortality, LiveBirths, IMR, CONGRESS)%>%
  rbind(IMR_byCD_byRace)%>%
  #limit to just congress 115 & 116
  filter(CONGRESS%in%c("114 (2015 - 2016)", "116 (2019-2020)"))

#IMR CALCULATION
######################################################################################################
#calculate MLD for each CD separately (IMR)
# Following Haper 2005, calculate the population average rate as a weighted average of the subgroup-specific age-adjusted rates,
# with the overall proportion of each subgroup in the population as weights
test<-total_deaths_IMR%>%
  filter(CD=="CD 01")%>%
  filter(CONGRESS=="114 (2015 - 2016)")
# pj= the proportion of births in the race-ethnic group/total births in CD
#rj=race-ethnic group IMR/total IMR (in CD)
#test for 1 cd
test1<-test%>%
  mutate(total_births=sum(LiveBirths),
         pj=LiveBirths/total_births,
         total_deaths=sum(InfantMortality),
         IMR_total=total_deaths/total_births*1000,
         rj=IMR/IMR_total,
         MLD=pj*(-log(rj)),
         CD_MLD=sum(MLD))
mld_race1<-total_deaths_IMR%>%
  #filter out imr of zero
  filter(IMR != 0)%>%
  group_by(CONGRESS, CD) %>%
  group_modify(~{
    total_births<-sum(.x$LiveBirths)
    pj<-.x$LiveBirths/total_births
    total_deaths=sum(.x$InfantMortality)
    IMR_total<-total_deaths/total_births*1000
    rj<-.x$IMR/IMR_total
    MLD<-pj*(-log(rj))
    data.frame(
      CD_MLD=sum(MLD)
    ) %>% as_tibble
  })
write.csv(mld_race1, "Final Results\\mld_race.csv")
#take mean MLD across CD's
mean_mld<-mld_race1%>%
  group_by(CONGRESS)%>%
  summarize(mean_mld=mean(CD_MLD))
#repeat MLD, now using CD's as the subgroup, and do separately for each race/ethnicty (so no final summary step)
mld_race_spec<-total_deaths_IMR%>%
  #filter out imr of zero
  filter(IMR != 0)%>%
  group_by(CONGRESS, RACEHISP)%>%
  group_modify(~{
    total_births<-sum(.x$LiveBirths)
    pj<-.x$LiveBirths/total_births
    total_deaths=sum(.x$InfantMortality)
    IMR_total<-total_deaths/total_births*1000
    rj<-.x$IMR/IMR_total
    MLD<-pj*(-log(rj))
    data.frame(
      CD_MLD=sum(MLD)
    ) %>% as_tibble
  })
write.csv(mld_race_spec, "Final Results/mld_race_spec.csv", row.names = FALSE)
################################################################################
DOD_byYear <- group_by(death_cd, year) %>%
  summarise(DOD = sum(DESPAIR, na.rm = TRUE))

population_byCD <- group_by(population_byRaceSexAgeCD, year, CD) %>%
  summarise(Population = sum(Population))

DOD_byCD <- group_by(death_cd, year, CD) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE)) %>%
  mutate(year = as.numeric(year)) %>%
  left_join(population_byCD) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population)) %>%
  inner_join(CONGRESS)

#Combined congresses
DOD_byCD_2 <- group_by(death_cd, year, CD) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE)) %>%
  mutate(year = as.numeric(year)) %>%
  left_join(population_byCD) %>%
  inner_join(CONGRESS) %>%
  group_by(CD, CONGRESS2) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population, na.rm = TRUE)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

################################################################################

DOD_byRaceSexCD_deaths <- group_by(death_cd, CD, CONGRESS, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE)) %>%
  filter(as.integer(RACE)!=3) #Remove american indian alaksa native

#Combined congresses
DOD_byRaceSexCD_deaths_2 <- group_by(death_cd, CD, CONGRESS2, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE)) %>%
  filter(as.integer(RACE)!=3) #Remove american indian alaksa native

#DOD - cd116
DOD_byRaceSexCD_deaths_2_cd116 <- group_by(death_cd116, CD, CONGRESS2, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE)) %>%
  filter(as.integer(RACE)!=3) #Remove american indian alaksa native

#DOD - White/Non-White
DOD_byRace2SexCD_deaths_2 <- group_by(death_cd, CD, CONGRESS2, SEX, RACE2, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#DOD - White/Non-White - cd116
DOD_byRace2SexCD_deaths_2_cd116 <- group_by(death_cd116, CD, CONGRESS2, SEX, RACE2, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#Mortality Rates
DOD_byRaceSexCD <- left_join(DOD_byRaceSexCD_deaths, population_byRaceSexAgeCD_byCongress) %>%
  group_by(CD, CONGRESS, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population), as.integer(RACE)!=3)

#Mortality Rates - Combined congresses 
DOD_byRaceSexCD_2 <- left_join(DOD_byRaceSexCD_deaths_2, population_byRaceSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population), as.integer(RACE)!=3) %>%
  select(-c(DOD, Population))
write.csv(DOD_byRaceSexCD_2, file = "DOD_byRaceSexCD_2.csv")

#Sensitivity analysis -- original method of pulling population
DOD_byRaceSexCD_2_orig <- left_join(DOD_byRaceSexCD_deaths_2, population_byRaceSexAgeCD_byCongress2_orig) %>%
  group_by(CD, CONGRESS2, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population), as.integer(RACE)!=3) %>%
  select(-c(DOD, Population))
write.csv(DOD_byRaceSexCD_2_orig, file = "DOD_byRaceSexCD_2_orig.csv")

#Save as R Object
save(DOD_byRaceSexCD_2, file = "./ShinyApp/DOD_byRaceSexCD_2.Rdata")

#Mortality Rates - cd116
DOD_byRaceSexCD_2_cd116 <- left_join(DOD_byRaceSexCD_deaths_2_cd116, population_byRaceSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population), as.integer(RACE)!=3) %>%
  select(-c(DOD, Population))

#Save as R Object
save(DOD_byRaceSexCD_2_cd116, file = "./ShinyApp/DOD_byRaceSexCD_2_cd116.Rdata")

#Mortality Rates - Combined congresses - Wider
# DOD_byRaceSexCD_2_wider <- DOD_byRaceSexCD_2 %>%
#   mutate(SEX_AGE = paste(SEX, AGE_CAT_EDUC)) %>%
#   pivot_wider(names_from = SEX_AGE, values_from = MR, values_fill = 0) %>%
#   rename(m_25_34 = "Male 25 to 34 years", m_35_44 = "Male 35 to 44 years", m_45_64 = "Male 45 to 64 years",
#          f_25_34 = "Female 25 to 34 years", f_35_44 = "Female 35 to 44 years", f_45_64 = "Female 45 to 64 years") %>%
#   group_by(CD, CONGRESS2, RACE) %>%
#   summarise("Male 25 to 34 years" = sum(m_25_34), "Male 35 to 44 years" = sum(m_35_44), "Male 45 to 64 years" = sum(m_45_64),
#             "Female 25 to 34 years" = sum(f_25_34), "Female 35 to 44 years" = sum(f_35_44), "Female 45 to 64 years" = sum(f_45_64))

#Export to CSV
# write.csv(DOD_byRaceSexCD_2_wider, "C:/Users/ga437/OneDrive - Drexel University/Congressional_Districts_BRFSS/Anfuso_Results/DOD By Race.csv", row.names = FALSE)

#Mortality Rates - White/Non-White 
DOD_byRace2SexCD_2 <- left_join(DOD_byRace2SexCD_deaths_2, population_byRace2SexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, RACE2, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population)) %>%
  select(-c(DOD, Population)) 

#Save as R Object
save(DOD_byRace2SexCD_2, file = "./ShinyApp/DOD_byRace2SexCD_2.Rdata")

#Mortality Rates - White/Non-White - cd116
DOD_byRace2SexCD_2_cd116 <- left_join(DOD_byRace2SexCD_deaths_2_cd116, population_byRace2SexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, RACE2, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population)) %>%
  select(-c(DOD, Population))

#Save as R Object
save(DOD_byRace2SexCD_2_cd116, file = "./ShinyApp/DOD_byRace2SexCD_2_cd116.Rdata")
  
################################################################################

#DOD by CD - White
DOD_byRaceSexCD_deaths_white <- group_by(filter(death_cd, as.integer(RACE) == 1), CD, CONGRESS, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#cd116 boundaries
DOD_byRaceSexCD_deaths_white_cd116 <- group_by(filter(death_cd116, as.integer(RACE) == 1), CD, CONGRESS, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#Combined congresses
DOD_byRaceSexCD_deaths_white_2 <- group_by(filter(death_cd, as.integer(RACE) == 1), CD, CONGRESS2, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#Combined congresses
DOD_byRaceSexCD_deaths_white_2_cd116 <- group_by(filter(death_cd116, as.integer(RACE) == 1), CD, CONGRESS2, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#DOD by CD & Race - Non-Hispanic White
DOD_byRaceSexCD_white <- inner_join(DOD_byRaceSexCD_deaths_white, population_byRaceSexAgeCD_byCongress) %>%
  group_by(CD, CONGRESS, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR_white = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

#Combined congresses
DOD_byRaceSexCD_white_2 <- inner_join(DOD_byRaceSexCD_deaths_white_2, population_byRaceSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR_white = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

#Combined congresses cd116
DOD_byRaceSexCD_white_2_cd116 <- inner_join(DOD_byRaceSexCD_deaths_white_2_cd116, population_byRaceSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR_white = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

#Mortality Rates - cd116
DOD_byRaceSexCD_2_cd116 <- left_join(DOD_byRaceSexCD_deaths_2_cd116, population_byRaceSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population), as.integer(RACE)!=3) %>%
  select(-c(DOD, Population))

#Absolute disparity by CD and Race
DOD_byRaceSexCD_absDisparity <- full_join(DOD_byRaceSexCD, 
                                          DOD_byRaceSexCD_white, 
                                          c("CD", "CONGRESS", "SEX", "AGE_CAT_EDUC")) %>%
  mutate(MR_absDisparity = (MR-MR_white)) %>%
  select(-c("DOD.x", "Population.x", "DOD.y", "RACE.y", "Population.y")) %>%
  rename(RACE = "RACE.x")

#Combined congresses
DOD_byRaceSexCD_absDisparity_2 <- full_join(DOD_byRaceSexCD_2, 
                                            DOD_byRaceSexCD_white_2, 
                                            c("CD", "CONGRESS2", "SEX", "AGE_CAT_EDUC")) %>%
  mutate(MR_absDisparity = (MR-MR_white)) %>%
  select(-c("DOD", "Population", "DOD", "RACE.y", "MR", "MR_white")) %>%
  rename(RACE = "RACE.x", MR = "MR_absDisparity")

#Save as R Object
save(DOD_byRaceSexCD_absDisparity_2, file = "./ShinyApp/DOD_byRaceSexCD_absDisparity_2.Rdata")

#Abs disparities - cd116 boundaries
DOD_byRaceSexCD_absDisparity_2_cd116 <- full_join(DOD_byRaceSexCD_2_cd116, 
                                                  DOD_byRaceSexCD_white_2_cd116, 
                                                  c("CD", "CONGRESS2", "SEX", "AGE_CAT_EDUC")) %>%
  mutate(MR_absDisparity = (MR-MR_white)) %>%
  select(-c("DOD", "Population", "DOD", "RACE.y", "MR", "MR_white")) %>%
  rename(RACE = "RACE.x", MR = "MR_absDisparity")

#Save as R Object
save(DOD_byRaceSexCD_absDisparity_2_cd116, file = "./ShinyApp/DOD_byRaceSexCD_absDisparity_2_cd116.Rdata")

################################################################################

DOD_byEducSexCD_deaths <- group_by(death_cd, CD, CONGRESS, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

DOD_byEducSexCD_deaths_2 <- group_by(death_cd, CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#cd116 boundaries
DOD_byEducSexCD_deaths_2_cd116 <- group_by(death_cd116, CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

DOD_byEducSexCD <- left_join(DOD_byEducSexCD_deaths, population_byEducSexAgeCD_byCongress) %>%
  group_by(CD, CONGRESS, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

#Combined congress
DOD_byEducSexCD_2 <- left_join(DOD_byEducSexCD_deaths_2, population_byEducSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population)) %>%
  select(-c(DOD, Population))

write.csv(DOD_byEducSexCD_2, file = "DOD_byEducSexCD_2.csv")

#Sensitivity analysis - original population pull
DOD_byEducSexCD_2_orig <- left_join(DOD_byEducSexCD_deaths_2, population_byEducSexAgeCD_byCongress2_orig) %>%
  group_by(CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population)) %>%
  select(-c(DOD, Population))

write.csv(DOD_byEducSexCD_2_orig, file = "DOD_byEducSexCD_2_orig.csv")

#Save as R Object
save(DOD_byEducSexCD_2, file = "./ShinyApp/DOD_byEducSexCD_2.Rdata")

#CD116
DOD_byEducSexCD_2_cd116_orig <- left_join(DOD_byEducSexCD_deaths_2_cd116, population_byEducSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

DOD_byEducSexCD_2_cd116 <- left_join(DOD_byEducSexCD_deaths_2_cd116, population_byEducSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population)) %>%
  select(-c(DOD, Population))

#Save as R Object
save(DOD_byEducSexCD_2_cd116, file = "./ShinyApp/DOD_byEducSexCD_2_cd116.Rdata")

################################################################################

#DOD by CD - College
DOD_byEducSexCD_deaths_college <- group_by(filter(death_cd, as.integer(EDUC) == 4), CD, CONGRESS, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#Combined congress
DOD_byEducSexCD_deaths_college_2 <- group_by(filter(death_cd, as.integer(EDUC) == 4), CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#Cd116 boundaries
DOD_byEducSexCD_deaths_college_2_cd116 <- group_by(filter(death_cd116, as.integer(EDUC) == 4), CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#DOD MR by CD & Educ - College
DOD_byEducSexCD_college <- inner_join(DOD_byEducSexCD_deaths_college, population_byEducSexAgeCD_byCongress) %>%
  group_by(CD, CONGRESS, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR_college = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

#Combined congress
DOD_byEducSexCD_college_2 <- inner_join(DOD_byEducSexCD_deaths_college_2, population_byEducSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR_college = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

#Cd116
DOD_byEducSexCD_college_2_cd116 <- inner_join(DOD_byEducSexCD_deaths_college_2_cd116, population_byEducSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR_college = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

#Absolute disparity by CD and Education
DOD_byEducSexCD_absDisparity <- full_join(DOD_byEducSexCD, 
                                       DOD_byEducSexCD_college,
                                       c("CD", "CONGRESS", "SEX", "AGE_CAT_EDUC")) %>%
  mutate(MR_absDisparity = (MR-MR_college)) %>%
  select(-c("DOD.x", "Population.x", "DOD.y", "EDUC.y", "Population.y")) %>%
  rename(EDUC = "EDUC.x")

#Combined congress
DOD_byEducSexCD_absDisparity_2 <- full_join(DOD_byEducSexCD_2, 
                                          DOD_byEducSexCD_college_2,
                                          c("CD", "CONGRESS2", "SEX", "AGE_CAT_EDUC")) %>%
  mutate(MR_absDisparity = (MR-MR_college), paired = as.integer(CD)) %>%
  select(-c("EDUC.y", "DOD", "Population")) %>%
  rename(EDUC = "EDUC.x")

#Save as R Object
save(DOD_byEducSexCD_absDisparity_2, file = "./ShinyApp/DOD_byEducSexCD_absDisparity_2.Rdata")

#Relative disparities
DOD_byEducSexCD_relDisparity_2 <- full_join(DOD_byEducSexCD_2, 
                                            DOD_byEducSexCD_college_2,
                                            c("CD", "CONGRESS2", "SEX", "AGE_CAT_EDUC")) %>%
  mutate(MR_relDisparity = (MR/MR_college), paired = as.integer(CD)) %>%
  select(-c("EDUC.y", "DOD", "Population")) %>%
  rename(EDUC = "EDUC.x")

#Combined congress - cd116
DOD_byEducSexCD_absDisparity_2_cd116 <- full_join(DOD_byEducSexCD_2_cd116, 
                                            DOD_byEducSexCD_college_2_cd116,
                                            c("CD", "CONGRESS2", "SEX", "AGE_CAT_EDUC")) %>%
  mutate(MR_absDisparity = (MR-MR_college), paired = as.integer(CD)) %>%
  select(-c("EDUC.y", "DOD", "Population")) %>%
  rename(EDUC = "EDUC.x")

#Save as R Object
save(DOD_byEducSexCD_absDisparity_2_cd116, file = "./ShinyApp/DOD_byEducSexCD_absDisparity_2_cd116.Rdata")

#Create table for paper
DOD_byEducSexCD_absDisparity_2_table <- DOD_byEducSexCD_absDisparity_2 %>%
  filter(as.integer(CONGRESS2) == 1, as.integer(EDUC) < 4) %>%
  mutate(EDUC_CONGRESS = paste0(EDUC, " - ", CONGRESS2) %>%
           str_replace_all("Less than High School", "LHS") %>%
           str_replace_all("High School", "HS") %>%
           str_replace_all("Some College/Associate Degree", "SCAD") %>%
           str_replace_all("Bachelor/Master/Doctorate/Professional Degree", "BMDP")) %>%
  pivot_wider(names_from = EDUC_CONGRESS, values_from = MR_absDisparity, values_fill = 0) %>%
  ungroup() %>%
  select(-c(CONGRESS2, EDUC, MR, MR_college, paired)) %>%
  group_by(CD, SEX, AGE_CAT_EDUC) %>%
  summarise_all(sum) %>%
  arrange(AGE_CAT_EDUC, SEX, CD) %>%
  bind_cols({
    DOD_byEducSexCD_absDisparity_2 %>%
      filter(as.integer(CONGRESS2) == 2, as.integer(EDUC) < 4) %>%
      mutate(EDUC_CONGRESS = paste0(EDUC, " - ", CONGRESS2) %>%
               str_replace_all("Less than High School", "LHS") %>%
               str_replace_all("High School", "HS") %>%
               str_replace_all("Some College/Associate Degree", "SCAD") %>%
               str_replace_all("Bachelor/Master/Doctorate/Professional Degree", "BMDP")) %>%
      pivot_wider(names_from = EDUC_CONGRESS, values_from = MR_absDisparity, values_fill = 0) %>%
      ungroup() %>%
      select(-c(CONGRESS2, EDUC, MR, MR_college, paired)) %>%
      add_row(CD = "CD 19", SEX = "Male", AGE_CAT_EDUC = "25 to 34 years") %>%
      add_row(CD = "CD 19", SEX = "Male", AGE_CAT_EDUC = "35 to 44 years") %>%
      add_row(CD = "CD 19", SEX = "Male", AGE_CAT_EDUC = "45 to 64 years") %>%
      add_row(CD = "CD 19", SEX = "Female", AGE_CAT_EDUC = "25 to 34 years") %>%
      add_row(CD = "CD 19", SEX = "Female", AGE_CAT_EDUC = "35 to 44 years") %>%
      add_row(CD = "CD 19", SEX = "Female", AGE_CAT_EDUC = "45 to 64 years") %>%
      group_by(CD, SEX, AGE_CAT_EDUC) %>%
      summarise_all(sum) %>%
      arrange(AGE_CAT_EDUC, SEX, CD) %>%
      ungroup() %>%
      select(-c(CD, SEX, AGE_CAT_EDUC))
  }) 

#Write to csv
write.csv(DOD_byEducSexCD_absDisparity_2_table, "./Final Results/DOD_byEducSexCD_absDisparity_2_table.csv", row.names = FALSE)

#Create table for paper - relative disparity
DOD_byEducSexCD_relDisparity_2_table <- DOD_byEducSexCD_relDisparity_2 %>%
  filter(as.integer(CONGRESS2) == 1, as.integer(EDUC) < 4) %>%
  mutate(EDUC_CONGRESS = paste0(EDUC, " - ", CONGRESS2) %>%
           str_replace_all("Less than High School", "LHS") %>%
           str_replace_all("High School", "HS") %>%
           str_replace_all("Some College/Associate Degree", "SCAD") %>%
           str_replace_all("Bachelor/Master/Doctorate/Professional Degree", "BMDP")) %>%
  pivot_wider(names_from = EDUC_CONGRESS, values_from = MR_relDisparity, values_fill = 0) %>%
  ungroup() %>%
  select(-c(CONGRESS2, EDUC, MR, MR_college, paired)) %>%
  group_by(CD, SEX, AGE_CAT_EDUC) %>%
  summarise_all(sum) %>%
  arrange(AGE_CAT_EDUC, SEX, CD) %>%
  bind_cols({
    DOD_byEducSexCD_relDisparity_2 %>%
      filter(as.integer(CONGRESS2) == 2, as.integer(EDUC) < 4) %>%
      mutate(EDUC_CONGRESS = paste0(EDUC, " - ", CONGRESS2) %>%
               str_replace_all("Less than High School", "LHS") %>%
               str_replace_all("High School", "HS") %>%
               str_replace_all("Some College/Associate Degree", "SCAD") %>%
               str_replace_all("Bachelor/Master/Doctorate/Professional Degree", "BMDP")) %>%
      pivot_wider(names_from = EDUC_CONGRESS, values_from = MR_relDisparity, values_fill = 0) %>%
      ungroup() %>%
      select(-c(CONGRESS2, EDUC, MR, MR_college, paired)) %>%
      add_row(CD = "CD 19", SEX = "Male", AGE_CAT_EDUC = "25 to 34 years") %>%
      add_row(CD = "CD 19", SEX = "Male", AGE_CAT_EDUC = "35 to 44 years") %>%
      add_row(CD = "CD 19", SEX = "Male", AGE_CAT_EDUC = "45 to 64 years") %>%
      add_row(CD = "CD 19", SEX = "Female", AGE_CAT_EDUC = "25 to 34 years") %>%
      add_row(CD = "CD 19", SEX = "Female", AGE_CAT_EDUC = "35 to 44 years") %>%
      add_row(CD = "CD 19", SEX = "Female", AGE_CAT_EDUC = "45 to 64 years") %>%
      group_by(CD, SEX, AGE_CAT_EDUC) %>%
      summarise_all(sum) %>%
      arrange(AGE_CAT_EDUC, SEX, CD) %>%
      ungroup() %>%
      select(-c(CD, SEX, AGE_CAT_EDUC))
  }) 

#Write to csv
write.csv(DOD_byEducSexCD_relDisparity_2_table, "./Final Results/DOD_byEducSexCD_relDisparity_2_table.csv", row.names = FALSE)

################################################################################

#MLD Calculation

#cobine congresss into one dataset
total_deaths_DOD<-DOD_byEducSexCD_2_cd116_orig%>%
  ungroup()%>%
  filter(CONGRESS2=="113 - 114 (2013 - 2015)")%>%
  mutate(CONGRESS="116 (2019-2020)")%>%
  select(-CONGRESS2)%>%
  rbind(DOD_byEducSexCD)%>%
  
  #limit to cd 114 and 116
  filter(CONGRESS%in%c("114 (2015 - 2016)", "116 (2019-2020)"))%>%
  group_by(CONGRESS, CD, EDUC)%>%
  summarize(DOD=sum(DOD),
            Population=sum(Population))%>%
  mutate(MR=DOD/Population*10000)

#calculate MLD for each CD separately
# Following Haper 2005, calculate the population average rate as a weighted average of the subgroup-specific age-adjusted rates,
# with the overall proportion of each subgroup in the population as weights
#pj= education group pop/total pop (in CD)
#rj=education grp DOD rate/toatl DOD rate for CD

mld_Educ1<-total_deaths_DOD%>%
  group_by(CONGRESS, CD) %>%
  group_modify(~{
    total_pop<-sum(.x$Population)
    pj<-.x$Population/total_pop
    total_deaths<-sum(.x$DOD)
    DODr_total<-total_deaths/total_pop*10000
    rj<-.x$MR/DODr_total
    MLD<-pj*(-log(rj))
    data.frame(
      CD_MLD=sum(MLD)
    ) %>% as_tibble
  })
write.csv(mld_Educ1, "Final Results\\mld_educ.csv")

#take mean MLD across CD's
mean_mld_educ<-mld_Educ1%>%
  group_by(CONGRESS)%>%
  summarize(mean_mld=mean(CD_MLD))

#repeat MLD, now using CD's as the subgroup, and do separately for each race/ethnicty (so no final summary step)
mld_CD_educ<-total_deaths_DOD%>%
  group_by(CONGRESS, EDUC) %>%
  group_modify(~{
    total_pop<-sum(.x$Population)
    pj<-.x$Population/total_pop
    total_deaths<-sum(.x$DOD)
    DODr_total<-total_deaths/total_pop*10000
    rj<-.x$MR/DODr_total
    MLD<-pj*(-log(rj))
    data.frame(
      ED_MLD=sum(MLD)
    ) %>% as_tibble
  })
write.csv(mld_CD_educ, "Final Results/mld_CD_educ.csv", row.names = FALSE)

################################################################################

#Get CD Maps 
#Bountaries for cd112 from tidycensus were off
# cd112 <- congressional_districts(state = "Pennsylvania", cb = FALSE, 
#                                  resolution = '20m', year = 2011) %>%
#   mutate(CD112FP = as.numeric(CD112FP), CDSESSN = as.numeric(CDSESSN)) %>%
#   rename(CD = CD112FP, CONGRESS = CDSESSN)

#Pulled shapefiles for cd111 and cd112 (same districts) from https://cdmaps.polisci.ucla.edu/

cd111 <- st_read("./Data/districts111.shp") %>% filter(STATENAME == "Pennsylvania") %>%
  mutate(DISTRICT = as.numeric(DISTRICT), CONGRESS = 1) %>%
  rename(CD = DISTRICT) %>%
  select(-c("DISTRICTSI", "COUNTY", "PAGE", "LAW", "NOTE", "BESTDEC", "FINALNOTE", 
            "RNOTE", "LASTCHANGE", "FROMCOUNTY", "STARTCONG", "ENDCONG", "STATENAME", "ID")) %>%
  st_as_sf()

cd112 <- st_read("./Data/districts112.shp") %>% filter(STATENAME == "Pennsylvania") %>%
  mutate(DISTRICT = as.numeric(DISTRICT), CONGRESS = 2) %>%
  rename(CD = DISTRICT) %>%
  select(-c("DISTRICTSI", "COUNTY", "PAGE", "LAW", "NOTE", "BESTDEC", "FINALNOTE", 
            "RNOTE", "LASTCHANGE", "FROMCOUNTY", "STARTCONG", "STATENAME", "ID", "ENDCONG")) %>%
  st_as_sf()

#113 and 114 boundaries are the same
cd113 <- st_read("./Data/districts113.shp") %>% filter(STATENAME == "Pennsylvania") %>%
  mutate(DISTRICT = as.numeric(DISTRICT), CONGRESS = 3) %>%
  rename(CD = DISTRICT) %>%
  select(-c("DISTRICTSI", "COUNTY", "PAGE", "LAW", "NOTE", "BESTDEC", "FINALNOTE", 
            "RNOTE", "LASTCHANGE", "FROMCOUNTY", "STARTCONG", "STATENAME", "ID", "ENDCONG")) %>%
  st_as_sf()


cd114 <- st_read("./Data/districts114.shp") %>% filter(STATENAME == "Pennsylvania") %>%
  mutate(DISTRICT = as.numeric(DISTRICT), CONGRESS = 4) %>%
  rename(CD = DISTRICT) %>%
  select(-c("DISTRICTSI", "COUNTY", "PAGE", "LAW", "NOTE", "BESTDEC", "FINALNOTE", 
            "RNOTE", "LASTCHANGE", "FROMCOUNTY", "STARTCONG", "STATENAME", "ID", "ENDCONG")) %>%
  st_as_sf()

cd116 <- st_read("./Data/tl_2019_us_cd116.shp") %>% filter(STATEFP == 42) %>%
  mutate(CD = as.numeric(CD116FP) %>%
           factor(levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),
                  labels = c("CD 01", "CD 02", "CD 03", "CD 04", "CD 05",
                             "CD 06", "CD 07", "CD 08", "CD 09", "CD 10",
                             "CD 11", "CD 12", "CD 13", "CD 14", "CD 15",
                             "CD 16", "CD 17", "CD 18", "CD 19"))) %>%
  select(c("CD", "geometry")) %>%
  st_as_sf()

cd_outlines <- bind_rows(cd111, cd112, cd113, cd114) %>%
  mutate(
    CD = factor(CD,
                levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),
                labels = c("CD 01", "CD 02", "CD 03", "CD 04", "CD 05",
                           "CD 06", "CD 07", "CD 08", "CD 09", "CD 10",
                           "CD 11", "CD 12", "CD 13", "CD 14", "CD 15",
                           "CD 16", "CD 17", "CD 18", "CD 19")),
    CONGRESS = factor(CONGRESS,
      levels = c(1,2,3,4),
      labels = c("111 (2009 - 2010)", "112 (2011 - 2012)", 
                 "113 (2013 - 2014)", "114 (2015 - 2016)")))

#Combined congresses
cd_outlines_2 <- filter(cd_outlines, as.integer(CONGRESS) == 1 | as.integer(CONGRESS) == 3) %>%
  inner_join(CONGRESS2)

IMR_Maps_byCD <- inner_join(cd_outlines, IMR_byCD) %>%
  group_by(CD, CONGRESS) %>%
  summarise(CD, IMR = sum(InfantMortality)/sum(LiveBirths)*1000, geometry)

#Combined Congresses
IMR_Maps_byCD_2 <- inner_join(cd_outlines_2, IMR_byCD_2) %>%
  group_by(CD, CONGRESS2) %>%
  summarise(IMR = sum(InfantMortality)/sum(LiveBirths)*1000, geometry)

#Save as R Object
save(IMR_Maps_byCD_2, file = "./ShinyApp/IMR_Maps_byCD_2.Rdata")

#IMR with CD116 Boundaries
IMR_Maps_cd116 <- inner_join(cd116, IMR_byCD_2) %>%
  select(-c(InfantMortality, LiveBirths)) %>%
  filter(as.integer(CONGRESS2)==2)

#Save as R Object
save(IMR_Maps_cd116, file = "./ShinyApp/IMR_Maps_cd116.Rdata")

  
DOD_Maps_byCD <- inner_join(cd_outlines, DOD_byCD) %>%
  group_by(CD, CONGRESS) %>%
  summarise(CD, MR = sum(DOD)/sum(Population)*10000, geometry)

#Combined congresses
DOD_Maps_byCD_2 <- inner_join(cd_outlines_2, DOD_byCD_2) %>%
  group_by(CD, CONGRESS2) %>%
  summarise(CD, MR)

#Save as R Object
save(DOD_Maps_byCD_2, file = "./ShinyApp/DOD_Maps_byCD_2.Rdata")

#IMR with CD116 Boundaries
DOD_Maps_cd116 <- inner_join(cd116, DOD_byCD_2) %>%
  select(-c(DOD, Population))

#Save as R Object
save(DOD_Maps_cd116, file = "./ShinyApp/DOD_Maps_cd116.Rdata")

#CD Geometries 
cd_geometries <- DOD_Maps_byCD_2 %>% select(c("CD", "CONGRESS2", "geometry"))

#Combined IMR/DOD Maps
IMR_DOD_Maps_byCD <- cd_geometries %>% inner_join(IMR_byCD_2) %>% inner_join(DOD_byCD_2, by = c("CD", "CONGRESS2"))

################################################################################

#Current maps -- attempt at mapping tracts and districts together
cd116 <- congressional_districts(state = 'Pennsylvania', year = 2020)

cd116_tracts <- tracts(state = 'Pennsylvania', year = 2020)

cd116_districts_tracts_data <- bind_rows(cd116, cd116_tracts)

cd116_districts_tracts <- ggplot() + 
  geom_sf(data = cd116, size = 1, aes(fill = CD116FP, geometry = geometry)) + 
  geom_sf(data = cd116_tracts, fill = NA, size = .1, aes(geometry = geometry))
cd116_districts_tracts  

################################################################################

#counties -- urban rural

#Looks like the shapefiles still are over-extending the northwest boundary

#counties
# counties2014 <- counties(state = "Pennsylvania", cb = FALSE, resolution = '20m', year = 2012)
# 
# counties_urbanRural <- urbanRural_counties %>%
#   group_by(GEOID) %>%
#   summarize(urban = sum(urban), urban_pct = round(urban/n()*100, digits = 2), 
#             suburban = sum(suburban), suburban_pct = round(suburban/n()*100, digits = 2),
#             rural = sum(rural), rural_pct = round(rural/n()*100, digits = 2),
#             total = sum(urban, suburban, rural), n = n()) %>%
#   mutate(
#     min = pmin(urban_pct, suburban_pct, rural_pct),
#     CONGRESS = 2,
#     urbanRural = case_when(
#       urban_pct >= 60 ~ 1,
#       suburban_pct >= 60 ~ 2,
#       rural_pct >= 60 ~ 3,
#       rural_pct == pmin(urban_pct, suburban_pct, rural_pct) ~ 4,
#       suburban_pct == pmin(urban_pct, suburban_pct, rural_pct) ~ 5,
#       urban_pct == pmin(urban_pct, suburban_pct, rural_pct) ~ 6) %>%
#       factor(levels = c(1,2,3,4,5,6), labels = c("Urban", "Suburban", "Rural", "Urban/Suburban", 
#                                                  "Urban/Rural","Suburban/Rural"))) %>%
#   inner_join(counties2014)


################################################################################
#IMR Map - CD111-CD112
IMR_Map_cd111_112 <- ggplot(filter(IMR_Maps_byCD_2, as.integer(CONGRESS2)==1)) +
  geom_sf(aes(fill = IMR), lwd = .1) + 
  theme_void() +
  theme(axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = "bottom", strip.text = element_text(size = 10),
        legend.title = element_text(size = 15)) + 
  labs(fill = "IMR per 1,000 Live Births")
IMR_Map_cd111_112

#IMR Map - CD113-CD114
IMR_Map_cd113_114 <- ggplot(filter(IMR_Maps_byCD_2, as.integer(CONGRESS2)==2)) +
  geom_sf(aes(fill = IMR), lwd = .1) + 
  theme_void() +
  theme(axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = "bottom", strip.text = element_text(size = 10),
        legend.title = element_text(size = 15)) + 
  ggtitle("IMR by Congressional District - 113th & 114th Congresses (2013 - 2015)") +
  scale_fill_scico(palette = "lajolla") +
  labs(fill = "IMR per 1,000 Live Births")
IMR_Map_cd113_114

#IMR Maps - Combined Congresses
IMR_Maps_2 <- ggplot(IMR_Maps_byCD_2) +
  geom_sf(aes(fill = IMR), lwd = .1) + 
  theme_void() +
  theme(axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5, vjust = 3),
        legend.position = "bottom", strip.text = element_text(size = 12), 
        legend.text = element_text(size = 10), legend.title = element_text(size = 12)) + 
  facet_wrap(~ CONGRESS2, ncol = 1) +
  #scale_colour_stepsn(colors = terrain.colors(10)) +
  scale_fill_continuous(type = "viridis") +
  #scale_fill_scico(palette = "lajolla") +
  labs(fill = "IMR per 1,000 Live Births")
IMR_Maps_2
ggsave(IMR_Maps_2, file=paste0(results_folder, "IMR_Maps_2.png"), device = agg_png, res = 300, units = "in",
       width = 10, height = 7, dpi = 300)

################################################################################

#Combined Congresses
IMR_byCD_byRace_Plots_2 <- ggplot(filter(IMR_byCD_byRace_2, as.integer(RACEHISP) < 6, IMR > 0),
                                  aes(x = CD, y = IMR)) + 
  geom_line() + 
  geom_point(size = 3, aes(color = RACEHISP)) +
  scale_color_manual(values = c("Hispanic" = color_Hispanic,
                                "Non-Hispanic Asian American Pacific Islander" = color_AAPI, 
                                "Non-Hispanic Black" = color_Black, "Non-Hispanic White" = color_White)) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15),
        legend.position = "bottom", legend.text=element_text(size=15),
        strip.text = element_text(size = 15), legend.title = element_text(size = 15)) + 
  guides(color = guide_legend(nrow = 2,  byrow = TRUE)) +
  xlab("Congressional District") +
  ylab("IMR per 1,000 Live Births") +
  labs(color = "Race/Hispanic Origin") + 
  facet_wrap(~ CONGRESS2, ncol = 1) +
  coord_flip()
IMR_byCD_byRace_Plots_2
ggsave(IMR_byCD_byRace_Plots_2, file=paste0(results_folder, "IMR_byCD_byRace_Plots_2.png"),  device = agg_png, 
       res = 300, units = "in", width = 10, height = 7, dpi = 300)
ggplotly(IMR_byCD_byRace_Plots_2)

#CD111-CD112
IMR_byCD_byRace_Plots_cd111_112 <- ggplot(filter(IMR_byCD_byRace_2, as.integer(RACEHISP) < 6, IMR > 0, as.integer(CONGRESS2) == 1),
                                          aes(x = CD, y = IMR)) + 
  geom_line() + 
  geom_point(size = 3, aes(color = RACEHISP)) +
  scale_color_manual(values = c("Hispanic" = color_Hispanic, "Non-Hispanic American Indian, Alaska Native" = color_AmericanIndian,
                                "Non-Hispanic Asian American Pacific Islander" = color_AAPI, 
                                "Non-Hispanic Black" = color_Black, "Non-Hispanic White" = color_White)) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 15), axis.text = element_text(size = 10),
        legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2)) +
  ggtitle("IMR - 111th & 112th Congresses (2010 - 2012)") + 
  xlab("Congressional District") +
  ylab("IMR per 1,000 Live Births") +
  labs(color = "Race/Hispanic Origin") + 
  coord_flip()
IMR_byCD_byRace_Plots_cd111_112
ggsave(IMR_byCD_byRace_Plots_cd111_112, file=paste0(results_folder, "IMR_byCD_byRace_Plots_cd111_112.png"),  device = agg_png, 
       res = 300, units = "in", width = 10, height = 7, dpi = 300)

#CD113-CD114
IMR_byCD_byRace_Plots_cd113_114 <- ggplot(filter(IMR_byCD_byRace_2, as.integer(RACEHISP) < 6, IMR > 0, as.integer(CONGRESS2) == 2),
                                          aes(x = CD, y = IMR)) + 
  geom_line() + 
  geom_point(size = 3, aes(color = RACEHISP)) +
  scale_color_manual(values = c("Hispanic" = color_Hispanic, "Non-Hispanic American Indian, Alaska Native" = color_AmericanIndian,
                                "Non-Hispanic Asian American Pacific Islander" = color_AAPI, 
                                "Non-Hispanic Black" = color_Black, "Non-Hispanic White" = color_White)) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 15), axis.text = element_text(size = 10),
        legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2)) +
  ggtitle("IMR - 113th & 114th Congresses (2013 - 2015)") + 
  xlab("Congressional District") +
  ylab("IMR per 1,000 Live Births") +
  labs(color = "Race/Hispanic Origin") + 
  coord_flip()
IMR_byCD_byRace_Plots_cd113_114
ggsave(IMR_byCD_byRace_Plots_cd113_114, file=paste0(results_folder, "IMR_byCD_byRace_Plots_cd113_114.png"),  device = agg_png, 
       res = 300, units = "in", width = 10, height = 7, dpi = 300)

#Absolute Disparities -- Combined Congresses
IMR_absDisparity_2 <- ggplot(filter(IMR_byCD_byRace_absDisparity_2, as.integer(RACEHISP) != 2,
                                  as.integer(RACEHISP) < 6), aes(x = CD, y = IMR)) + 
  geom_line() + 
  geom_point(size = 3, aes(color = RACEHISP)) +
  scale_color_manual(values = c("Hispanic" = color_Hispanic, "Non-Hispanic American Indian, Alaska Native" = color_AmericanIndian,
                                "Non-Hispanic Asian American Pacific Islander" = color_AAPI, "Non-Hispanic Black" = color_Black)) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 12),
        legend.position = "bottom", legend.text=element_text(size=12),
        strip.text = element_text(size = 12), legend.title = element_text(size = 12)) + 
  guides(color = guide_legend(nrow = 2)) +
  xlab("Congressional District") +
  ylab("Absolute Disparities in IMR") +
  labs(color = "Race/Hispanic Origin") +
  facet_wrap(~ CONGRESS2, ncol = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  coord_flip()
IMR_absDisparity_2
ggsave(IMR_absDisparity_2, file=paste0(results_folder, "IMR_absDisparity_2.png"),  device = agg_png, 
       res = 300, units = "in", width = 10, height = 7, dpi = 300)
ggplotly(IMR_absDisparity_2)

#CD111 - CD112
IMR_absDisparity_cd111_112 <- ggplot(filter(IMR_byCD_byRace_absDisparity_2, as.integer(RACEHISP) != 2,
                                    as.integer(RACEHISP) < 6, as.integer(CONGRESS2) == 1), aes(x = CD, y = IMR)) + 
  geom_line() + 
  geom_point(size = 3, aes(color = RACEHISP)) +
  scale_color_manual(values = c("Hispanic" = color_Hispanic, "Non-Hispanic American Indian, Alaska Native" = color_AmericanIndian,
                                "Non-Hispanic Asian American Pacific Islander" = color_AAPI, "Non-Hispanic Black" = color_Black)) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 15), axis.text = element_text(size = 10),
        legend.position = "bottom") + 
  guides(color = guide_legend(nrow = 2)) +
  ggtitle("IMR Absolute Disparities - 111th & 112th Congresses (2010 - 2012)") + 
  xlab("Congressional District") +
  ylab("Absolute Disparities in IMR") +
  labs(color = "Race/Hispanic Origin") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  coord_flip()
IMR_absDisparity_cd111_112
ggsave(IMR_absDisparity_cd111_112, file=paste0(results_folder, "IMR_absDisparity_cd111_112.png"),  device = agg_png, 
       res = 300, units = "in", width = 10, height = 7, dpi = 300)

#CD113 - CD114
IMR_absDisparity_cd113_114 <- ggplot(filter(IMR_byCD_byRace_absDisparity_2, as.integer(RACEHISP) != 2,
                                            as.integer(RACEHISP) < 6, as.integer(CONGRESS2) == 2), aes(x = CD, y = IMR)) + 
  geom_line() + 
  geom_point(size = 3, aes(color = RACEHISP)) +
  scale_color_manual(values = c("Hispanic" = color_Hispanic, "Non-Hispanic American Indian, Alaska Native" = color_AmericanIndian,
                                "Non-Hispanic Asian American Pacific Islander" = color_AAPI, "Non-Hispanic Black" = color_Black)) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 15), axis.text = element_text(size = 10),
        legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2)) +
  ggtitle("IMR Absolute Disparities - 113th & 114th Congresses (2013 - 2015)") + 
  xlab("Congressional District") +
  ylab("Absolute Disparities in IMR") +
  labs(color = "Race/Hispanic Origin") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  coord_flip()
IMR_absDisparity_cd113_114
ggsave(IMR_absDisparity_cd113_114, file=paste0(results_folder, "IMR_absDisparity_cd113_114.png"),  device = agg_png, 
       res = 300, units = "in", width = 10, height = 7, dpi = 300)

################################################################################
#DOD Maps - CD111-CD112
DOD_Map_cd111_112 <- ggplot(filter(DOD_Maps_byCD_2, as.integer(CONGRESS2)==1)) +
  geom_sf(aes(fill = MR), lwd = .1) +
  theme_void() +
  theme(axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = "bottom", strip.text = element_text(size = 10),
        legend.title = element_text(size = 15)) + 
  ggtitle("DOD Mortality Rates by Congressional District - 111th & 112th Congresses (2010 - 2012)") +
  scale_fill_scico(palette = "lajolla") +
  labs(fill = "Mortality Rate, per 10,000 People")
DOD_Map_cd111_112
ggsave(DOD_Map_cd111_112, file=paste0(results_folder, "DOD_Map_cd111_112.png"), device = agg_png, res = 300, units = "in",
       width = 10, height = 7, dpi = 300)

#DOD Maps - CD113-CD114
DOD_Map_cd113_114 <- ggplot(filter(DOD_Maps_byCD_2, as.integer(CONGRESS2)==2)) +
  geom_sf(aes(fill = MR), lwd = .1) +
  theme_void() +
  theme(axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = "bottom", strip.text = element_text(size = 10),
        legend.title = element_text(size = 15)) + 
  ggtitle("DOD Mortality Rates by Congressional District - 113th & 114th Congresses (2013 - 2015)") +
  scale_fill_scico(palette = "lajolla") +
  labs(fill = "Mortality Rate, per 10,000 People")
DOD_Map_cd113_114
ggsave(DOD_Map_cd113_114, file=paste0(results_folder, "DOD_Map_cd113_114.png"), device = agg_png, res = 300, units = "in",
       width = 10, height = 7, dpi = 300)

#DOD Maps - Combined congresses
DOD_Maps_2 <- ggplot(DOD_Maps_byCD_2) +
  geom_sf(aes(fill = MR), lwd = .1) +
  theme_void() +
  theme(axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5, vjust = 3),
        legend.position = "bottom", strip.text = element_text(size = 12), 
        legend.text = element_text(size = 10), legend.title = element_text(size = 12)) + 
  facet_wrap(~ CONGRESS2, ncol = 1) +
  scale_fill_continuous(type = "viridis") +
  #scale_fill_brewer(palette = "YlOrRd") +
  labs(fill = "Mortality Rate, per 10,000 People")
DOD_Maps_2
ggsave(DOD_Maps_2, file=paste0(results_folder, "DOD_Maps_2.png"), device = agg_png, res = 300, units = "in",
       width = 10, height = 7, dpi = 300)

################################################################################

#DOD Lollipop Plots by Race, Stratified by Sex and Age Group
#CD111-CD112
DOD_byRaceSexAge_Plot_cd111_112 <- ggplot(filter(DOD_byRaceSexCD_2, as.integer(CONGRESS2) == 1, MR > 0, as.integer(RACE) < 5),
                                          aes(x = CD, y = MR)) + 
  geom_line() + 
  geom_point(size = 3, aes(color = RACE)) +
  scale_color_manual(values = c("American Indian, Alaska Native" = color_AmericanIndian, "Asian American Pacific Islander" = color_AAPI, 
                                "Black" = color_Black, "White" = color_White)) + 
  ggtitle("DOD Mortality Rates - 111th & 112th Congresses (2010 - 2012)") + 
  xlab("Congressional District") +
  ylab("Mortality Rate, per 10,000 People") +
  labs(color = "Race") + 
  facet_wrap(~ SEX + AGE_CAT_EDUC) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 15), axis.text = element_text(size = 10),
        legend.position = "bottom", legend.text=element_text(size=10)) +
  guides(color = guide_legend(nrow = 2)) +
  scale_y_continuous(trans = 'log10', breaks = c(1,10,100,1000), limits=c(.1, NA), labels = c(1,10,100,1000))+
  scale_x_discrete(expand=expansion(mult=c(0.1, 0.05)))+
  annotation_logticks(sides="b") + 
  coord_flip()
DOD_byRaceSexAge_Plot_cd111_112
ggsave(DOD_byRaceSexAge_Plot_cd111_112, file=paste0(results_folder, "DOD_byRaceSexAge_Plot_cd111_112.png"),  
       device = agg_png, res = 300, units = "in", width = 10, height = 7, dpi = 300)


#CD111-CD112 - White/Non-White
DOD_byRace2SexAge_Plot_cd111_112 <- ggplot(filter(DOD_byRace2SexCD_2, as.integer(CONGRESS2) == 1, MR > 0), 
                                           aes(x = CD, y = MR)) + 
  geom_line() + 
  geom_point(size = 3, aes(color = RACE2)) +
  scale_color_manual(values = c("Non-White" = color_Black, "White" = color_White)) + 
  xlab("Congressional District") +
  ylab("Mortality Rate, per 10,000 People") +
  labs(color = "Race") + 
  facet_wrap(~ SEX + AGE_CAT_EDUC) +
  theme_bw() + 
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 12),
        legend.position = "bottom", legend.text=element_text(size=12),
        strip.text = element_text(size = 12), legend.title = element_text(size = 12)) + 
  scale_y_continuous(trans = 'log10', breaks = c(1,10,100,1000), limits=c(.1, NA), labels = c(1,10,100,1000))+
  scale_x_discrete(expand=expansion(mult=c(0.1, 0.05)))+
  annotation_logticks(sides="b") + 
  coord_flip()
DOD_byRace2SexAge_Plot_cd111_112
ggsave(DOD_byRace2SexAge_Plot_cd111_112, file=paste0(results_folder, "DOD_byRace2SexAge_Plot_cd111_112.png"),  
       device = agg_png, res = 300, units = "in", width = 10, height = 7, dpi = 300)

#CD113-CD114
DOD_byRaceSexAge_Plot_cd113_114 <- ggplot(filter(DOD_byRaceSexCD_2, as.integer(CONGRESS2) == 2, MR > 0, as.integer(RACE) < 5),
                                          aes(x = CD, y = MR)) + 
  geom_line() + 
  geom_point(size = 3, aes(color = RACE)) +
  scale_color_manual(values = c("American Indian, Alaska Native" = color_AmericanIndian, "Asian American Pacific Islander" = color_AAPI, 
                                "Black" = color_Black, "White" = color_White)) + 
  xlab("Congressional District") +
  ylab("Mortality Rate, per 10,000 People") +
  labs(color = "Race") + 
  facet_wrap(~ SEX + AGE_CAT_EDUC) +
  theme_bw() + 
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 12),
        legend.position = "bottom", legend.text=element_text(size=12),
        strip.text = element_text(size = 12), legend.title = element_text(size = 12)) + 
  guides(color = guide_legend(nrow = 2)) +
  scale_y_continuous(trans = 'log10', breaks = c(1,10,100,1000), limits=c(.1, NA), labels = c(1,10,100,1000))+
  scale_x_discrete(expand=expansion(mult=c(0.1, 0.05)))+
  annotation_logticks(sides="b") + 
  coord_flip()
DOD_byRaceSexAge_Plot_cd113_114
ggsave(DOD_byRaceSexAge_Plot_cd113_114, file=paste0(results_folder, "DOD_byRaceSexAge_Plot_cd113_114.png"),  
       device = agg_png, res = 300, units = "in", width = 10, height = 7, dpi = 300)

#CD113-CD114 - White/Non-White
DOD_byRace2SexAge_Plot_cd113_114 <- ggplot(filter(DOD_byRace2SexCD_2, as.integer(CONGRESS2) == 2, MR > 0), 
                                           aes(x = CD, y = MR)) + 
  geom_line() + 
  geom_point(size = 3, aes(color = RACE2)) +
  scale_color_manual(values = c("Non-White" = color_Black, "White" = color_White)) + 
  xlab("Congressional District") +
  ylab("Mortality Rate, per 10,000 People") +
  labs(color = "Race") + 
  facet_wrap(~ SEX + AGE_CAT_EDUC) +
  theme_bw() + 
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 12),
        legend.position = "bottom", legend.text=element_text(size=12),
        strip.text = element_text(size = 12), legend.title = element_text(size = 12)) + 
  scale_y_continuous(trans = 'log10', breaks = c(1,10,100,1000), limits=c(.1, NA), labels = c(1,10,100,1000))+
  scale_x_discrete(expand=expansion(mult=c(0.1, 0.05)))+
  annotation_logticks(sides="b") + 
  coord_flip()
DOD_byRace2SexAge_Plot_cd113_114
ggsave(DOD_byRace2SexAge_Plot_cd113_114, file=paste0(results_folder, "DOD_byRace2SexAge_Plot_cd113_114.png"),  
       device = agg_png, res = 300, units = "in", width = 10, height = 7, dpi = 300)

#Absolute Disparities by Congress
#CD111-CD112
DOD_absDisparity_race_cd111_112 <- ggplot(filter(DOD_byRaceSexCD_absDisparity_2, as.integer(RACE) != 1,
                                          as.integer(RACE) < 5, as.integer(CONGRESS2) == 1),
                                          aes(x = CD, y = MR)) + 
  geom_line() + 
  geom_point(size = 3, aes(color = RACE)) +
  scale_color_manual(values = c("American Indian, Alaska Native" = color_AmericanIndian, "Asian American Pacific Islander" = color_AAPI,
                                "Black" = color_Black)) + 
  ggtitle("DOD Mortality Rate Absolute Disparities - 111th & 112th Congresses (2010 - 2012)") + 
  xlab("Congressional District") +
  ylab("Absolute Disparities in DOD Mortality Rates") +
  labs(color = "Race") +
  facet_wrap(~ SEX + AGE_CAT_EDUC) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 15), axis.text = element_text(size = 10),
        legend.position = "bottom") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  coord_flip()
DOD_absDisparity_race_cd111_112
ggsave(DOD_absDisparity_race_cd111_112, file=paste0(results_folder, "DOD_absDisparity_race_cd111_112.png"),  device = agg_png, 
       res = 300, units = "in", width = 10, height = 7, dpi = 300)

#CD113-CD114
DOD_absDisparity_race_cd113_114 <- ggplot(filter(DOD_byRaceSexCD_absDisparity_2, as.integer(RACE) != 1,
                                          as.integer(RACE) < 5, as.integer(CONGRESS2) == 2),
                                          aes(x = CD, y = MR)) + 
  geom_line() + 
  geom_point(size = 3, aes(color = RACE)) +
  scale_color_manual(values = c("American Indian, Alaska Native" = color_AmericanIndian, "Asian American Pacific Islander" = color_AAPI,
                                "Black" = color_Black)) + 
  ggtitle("DOD Mortality Rate Absolute Disparities - 113th & 114th Congresses (2013 - 2015)") + 
  xlab("Congressional District") +
  ylab("Absolute Disparities in DOD Mortality Rates") +
  labs(color = "Race") +
  facet_wrap(~ SEX + AGE_CAT_EDUC) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 15), axis.text = element_text(size = 10),
        legend.position = "bottom") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  coord_flip()
DOD_absDisparity_race_cd113_114
ggsave(DOD_absDisparity_race_cd113_114, file=paste0(results_folder, "DOD_absDisparity_race_cd113_114.png"),  device = agg_png, 
       res = 300, units = "in", width = 10, height = 7, dpi = 300)

################################################################################

#DOD Lollipop Plots by Education, Stratified by Sex and Age Group
#All years 
DOD_byEducSexAge_Plot <- ggplot(filter(DOD_byEducSexCD_2, MR > 0, as.integer(EDUC) < 5),
                                          aes(x = CD, y = MR)) + 
  geom_line() + 
  geom_point(size = 3, aes(color = EDUC)) +
  scale_color_manual(values = c("Less than High School" = color_LessHS, "High School" = color_HS, 
                                "Some College/Associate Degree" = color_SomeCollege, "Bachelor/Master/Doctorate/Professional Degree" = color_Bachelor)) + 
  xlab("Congressional District") +
  ylab("Mortality Rate, per 10,000 People") +
  labs(color = "Education") + 
  facet_nested_wrap(vars(CONGRESS2, AGE_CAT_EDUC, SEX), dir = "v", nrow = 2, ncol = 6) +
  theme_bw() + 
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15),
        legend.position = "bottom", legend.text=element_text(size=15),
        strip.text = element_text(size = 15), legend.title = element_text(size = 15)) + 
  guides(color = guide_legend(nrow = 2)) +
  scale_y_continuous(trans = 'log10', breaks = c(1,10,100,1000), limits=c(.1, NA), labels = c(1,10,100,1000)) +
  scale_x_discrete(expand=expansion(mult=c(0.1, 0.05))) +
  annotation_logticks(sides="b") + 
  coord_flip()
DOD_byEducSexAge_Plot
ggsave(DOD_byEducSexAge_Plot, file=paste0(results_folder, "DOD_byEducSexAge_Plot.png"),  
       device = agg_png, res = 300, units = "in", width = 10, height = 7, dpi = 300)

#CD111 - CD112 
DOD_byEducSexAge_Plot_cd111_112 <- ggplot(filter(DOD_byEducSexCD_2, as.integer(CONGRESS2) == 1, MR > 0, as.integer(EDUC) < 5),
                                          aes(x = CD, y = MR)) + 
  geom_line() + 
  geom_point(size = 3, aes(color = EDUC)) +
  scale_color_manual(values = c("Less than High School" = color_LessHS, "High School" = color_HS, 
                                "Some College/Associate Degree" = color_SomeCollege, "Bachelor/Master/Doctorate/Professional Degree" = color_Bachelor)) + 
  xlab("Congressional District") +
  ylab("Mortality Rate, per 10,000 People") +
  labs(color = "Education") + 
  facet_wrap(~ SEX + AGE_CAT_EDUC) +
  theme_bw() + 
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 20),
        legend.position = "bottom", legend.text=element_text(size=20),
        strip.text = element_text(size = 20), legend.title = element_text(size = 20)) + 
  guides(color = guide_legend(nrow = 2)) +
  scale_y_continuous(trans = 'log10', breaks = c(1,10,100,1000), limits=c(.1, NA), labels = c(1,10,100,1000)) +
  scale_x_discrete(expand=expansion(mult=c(0.1, 0.05))) +
  annotation_logticks(sides="b") + 
  coord_flip()
DOD_byEducSexAge_Plot_cd111_112
ggsave(DOD_byEducSexAge_Plot_cd111_112, file=paste0(results_folder, "DOD_byEducSexAge_Plot_cd111_112.png"),  
       device = agg_png, res = 300, units = "in", width = 10, height = 7, dpi = 300)

#CD113-CD114 
DOD_byEducSexAge_Plot_cd113_114 <- ggplot(filter(DOD_byEducSexCD_2, as.integer(CONGRESS2) == 2, MR > 0, as.integer(EDUC) < 5),
                                          aes(x = CD, y = MR)) + 
  geom_line() + 
  geom_point(size = 3, aes(color = EDUC)) +
  scale_color_manual(values = c("Less than High School" = color_LessHS, "High School" = color_HS, 
                                "Some College/Associate Degree" = color_SomeCollege, "Bachelor/Master/Doctorate/Professional Degree" = color_Bachelor)) + 
  xlab("Congressional District") +
  ylab("Mortality Rate, per 10,000 People") +
  labs(color = "Education") + 
  facet_wrap(~ SEX + AGE_CAT_EDUC) +
  theme_bw() + 
  theme(axis.title = element_text(size = 30), axis.text = element_text(size = 25),
        legend.position = "bottom", legend.text=element_text(size=25),
        strip.text = element_text(size = 25), legend.title = element_text(size = 25)) + 
  guides(color = guide_legend(nrow = 2)) +
  scale_y_continuous(trans = 'log10', breaks = c(1,10,100,1000), limits=c(.1, NA), labels = c(1,10,100,1000))+
  scale_x_discrete(expand=expansion(mult=c(0.1, 0.05)))+
  annotation_logticks(sides="b") + 
  coord_flip()
DOD_byEducSexAge_Plot_cd113_114
ggsave(DOD_byEducSexAge_Plot_cd113_114, file=paste0(results_folder, "DOD_byEducSexAge_Plot_cd113_114.png"),  
       device = agg_png, res = 300, units = "in", width = 10, height = 7, dpi = 300)

#Absolute Disparities by Congress
DOD_absDisparity_educ_cd111_112 <- ggplot(filter(DOD_byEducSexCD_absDisparity_2, as.integer(EDUC) != 4,
                                                 as.integer(EDUC) < 5, as.integer(CONGRESS2) == 1),
                                                 aes(x = CD, y = MR)) + 
  geom_line() + 
  geom_point(size = 3, aes(color = EDUC)) +
  scale_color_manual(values = c("Less than High School" = color_LessHS, "High School" = color_HS, 
                                "Some College/Associate Degree" = color_SomeCollege)) + 
  xlab("Congressional District") +
  ylab("Absolute Disparities in DOD Mortality Rates") +
  labs(color = "Education") +
  facet_wrap(~ SEX + AGE_CAT_EDUC) +
  theme_bw() + 
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 12),
        legend.position = "bottom", legend.text=element_text(size=12),
        strip.text = element_text(size = 12), legend.title = element_text(size = 12)) +  
  geom_hline(yintercept = 0, linetype = "dashed") + 
  coord_flip()
DOD_absDisparity_educ_cd111_112
ggsave(DOD_absDisparity_educ_cd111_112, file=paste0(results_folder, "DOD_absDisparity_educ_cd111_112.png"),  device = agg_png, 
       res = 300, units = "in", width = 10, height = 7, dpi = 300)
ggplotly(DOD_absDisparity_educ_cd111_112)

DOD_absDisparity_educ_cd113_114 <- ggplot(filter(DOD_byEducSexCD_absDisparity_2, as.integer(EDUC) != 4,
                                                 as.integer(EDUC) < 5, as.integer(CONGRESS2) == 2),
                                                 aes(x = CD, y = MR)) + 
  geom_line() + 
  geom_point(size = 3, aes(color = EDUC)) +
  scale_color_manual(values = c("Less than High School" = color_LessHS, "High School" = color_HS, 
                                "Some College/Associate Degree" = color_SomeCollege)) + 
  xlab("Congressional District") +
  ylab("Absolute Disparities in DOD Mortality Rates") +
  labs(color = "Education") +
  facet_wrap(~ SEX + AGE_CAT_EDUC) +
  theme_bw() +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 12),
        legend.position = "bottom", legend.text=element_text(size=12),
        strip.text = element_text(size = 12), legend.title = element_text(size = 12)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  coord_flip()
DOD_absDisparity_educ_cd113_114
ggsave(DOD_absDisparity_educ_cd113_114, file=paste0(results_folder, "DOD_absDisparity_educ_cd113_114.png"),  device = agg_png, 
       res = 300, units = "in", width = 10, height = 7, dpi = 300)
ggplotly(DOD_absDisparity_educ_cd113_114)
################################################################################

IMR_DOD <- inner_join(IMR_byCD_2, DOD_byCD_2) %>%
  select(c(CONGRESS2, CD, IMR, MR)) %>%
  rename(DODMR = MR)

IMR_DOD_Scatter <- ggplot(IMR_DOD, aes(x = IMR, y = DODMR)) +
  geom_point(aes(color = CD)) +
  facet_wrap(~CONGRESS2) + 
  theme_bw() +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15),
        legend.position = "bottom", strip.text = element_text(size = 15),
        legend.text = element_text(size = 10)) +
  guides(color=guide_legend(nrow=3,byrow=TRUE)) +
  xlab("Infant Mortality Rate, per 1,000 Live Births") +
  ylab("Deaths of Despair Mortality Rate, per 10,000")
IMR_DOD_Scatter


################################################################################
 #Urban/Rural/Suburban Analysis
# cd111_urbanRural <- inner_join(tract_to_cd111, urbanRural) %>%
#   mutate(pop_tract = as.numeric(gsub(",","", pop_tract)),
#          pop_urban = pop_tract*urban,
#          pop_suburban = pop_tract*suburban,
#          pop_rural = pop_tract*rural) %>%
#   group_by(cd111) %>%
#   summarize(urban = sum(urban), urban_pct = round(urban/n()*100, digits = 2), 
#             suburban = sum(suburban), suburban_pct = round(suburban/n()*100, digits = 2),
#             rural = sum(rural), rural_pct = round(rural/n()*100, digits = 2),
#             total = sum(urban, suburban, rural), n = n(), population_urban = sum(pop_urban),
#             population_suburban = sum(pop_suburban), population_rural = sum(pop_rural),
#             population = sum(pop_tract), urban_pct_pop = round(population_urban/population*100, digits = 2),
#             suburban_pct_pop = round(population_suburban/population*100, digits = 2),
#             rural_pct_pop = round(population_rural/population*100, digits = 2)) %>%
#   rename(CD = cd111) %>%
#   mutate(CONGRESS = 1)
# 
# cd112_urbanRural <- inner_join(tract_to_cd111, urbanRural) %>%
#   mutate(pop_tract = as.numeric(gsub(",","", pop_tract)),
#          pop_urban = pop_tract*urban,
#          pop_suburban = pop_tract*suburban,
#          pop_rural = pop_tract*rural) %>%
#   group_by(cd111) %>%
#   summarize(urban = sum(urban), urban_pct = round(urban/n()*100, digits = 2), 
#             suburban = sum(suburban), suburban_pct = round(suburban/n()*100, digits = 2),
#             rural = sum(rural), rural_pct = round(rural/n()*100, digits = 2),
#             total = sum(urban, suburban, rural), n = n(), population_urban = sum(pop_urban),
#             population_suburban = sum(pop_suburban), population_rural = sum(pop_rural),
#             population = sum(pop_tract), urban_pct_pop = round(population_urban/population*100, digits = 2),
#             suburban_pct_pop = round(population_suburban/population*100, digits = 2),
#             rural_pct_pop = round(population_rural/population*100, digits = 2)) %>%
#   rename(CD = cd111) %>%
#   mutate(CONGRESS = 2)
# 
# cd113_urbanRural <- inner_join(tract_to_cd113, urbanRural) %>%
#   mutate(pop_tract = as.numeric(gsub(",","", pop_tract)),
#          pop_urban = pop_tract*urban,
#          pop_suburban = pop_tract*suburban,
#          pop_rural = pop_tract*rural) %>%
#   group_by(cd113) %>%
#   summarize(urban = sum(urban), urban_pct = round(urban/n()*100, digits = 2), 
#             suburban = sum(suburban), suburban_pct = round(suburban/n()*100, digits = 2),
#             rural = sum(rural), rural_pct = round(rural/n()*100, digits = 2),
#             total = sum(urban, suburban, rural), n = n(), population_urban = sum(pop_urban),
#             population_suburban = sum(pop_suburban), population_rural = sum(pop_rural),
#             population = sum(pop_tract), urban_pct_pop = round(population_urban/population*100, digits = 2),
#             suburban_pct_pop = round(population_suburban/population*100, digits = 2),
#             rural_pct_pop = round(population_rural/population*100, digits = 2)) %>%
#   rename(CD = cd113) %>%
#   mutate(CONGRESS = 3)
# 
# cd114_urbanRural <- inner_join(tract_to_cd114, urbanRural) %>%
#   mutate(pop_tract = as.numeric(gsub(",","", pop_tract)),
#          pop_urban = pop_tract*urban,
#          pop_suburban = pop_tract*suburban,
#          pop_rural = pop_tract*rural) %>%
#   group_by(cd114) %>%
#   summarize(urban = sum(urban), urban_pct = round(urban/n()*100, digits = 2), 
#             suburban = sum(suburban), suburban_pct = round(suburban/n()*100, digits = 2),
#             rural = sum(rural), rural_pct = round(rural/n()*100, digits = 2),
#             total = sum(urban, suburban, rural), n = n(), population_urban = sum(pop_urban),
#             population_suburban = sum(pop_suburban), population_rural = sum(pop_rural),
#             population = sum(pop_tract), urban_pct_pop = round(population_urban/population*100, digits = 2),
#             suburban_pct_pop = round(population_suburban/population*100, digits = 2),
#             rural_pct_pop = round(population_rural/population*100, digits = 2)) %>%
#   rename(CD = cd114) %>%
#   mutate(CONGRESS = 4)
# 
# cd_urbanRural <- bind_rows(cd111_urbanRural, cd112_urbanRural, cd113_urbanRural, cd114_urbanRural) %>%    
#   mutate(min = pmin(urban_pct, suburban_pct, rural_pct),    
#          urbanRural = case_when(    
#            urban_pct_pop >= 60 ~ 1,    
#            suburban_pct_pop >= 60 ~ 2,    
#            rural_pct_pop >= 60 ~ 3,    
#            rural_pct_pop == pmin(urban_pct_pop, suburban_pct_pop, rural_pct_pop) ~ 4,    
#            suburban_pct_pop == pmin(urban_pct_pop, suburban_pct_pop, rural_pct_pop) ~ 5,    
#            urban_pct_pop == pmin(urban_pct_pop, suburban_pct_pop, rural_pct_pop) ~ 6) %>%    
#            factor(levels = c(1,2,3,4,5,6),     labels = c("Urban", "Suburban", "Rural", "Urban/Suburban", "Urban/Rural","Suburban/Rural")),    
#          CONGRESS = factor(CONGRESS, 
#                            levels = c(1,2,3,4),
#                            labels = c("111 (2009 - 2010)", "112 (2011 - 2012)", 
#                                       "113 (2013 - 2014)", "114 (2015 - 2016)")),     
#          CD = factor(CD, labels = c("CD 01", "CD 02", "CD 03", "CD 04", "CD 05", 
#                                     "CD 06", "CD 07", "CD 08", "CD 09", "CD 10",    
#                                     "CD 11", "CD 12", "CD 13", "CD 14", "CD 15",     
#                                     "CD 16", "CD 17", "CD 18", "CD 19"))) %>%    
#   inner_join(cd_outlines)
# 
# cd_urbanRural_2 <- filter(cd_urbanRural, as.integer(CONGRESS) == 1 | as.integer(CONGRESS) == 3) %>%
#   inner_join(CONGRESS2)
# 
# cd_urbanRural_IMR <- inner_join(cd_urbanRural, IMR_byCD) %>%
#   select(c("CD", "CONGRESS", "urbanRural", "IMR")) %>%
#   inner_join(CONGRESS2)
# 
# cd_urbanRural_IMR_2 <- inner_join(cd_urbanRural_2, IMR_byCD_2) %>%
#   select(c("CD", "CONGRESS2", "urbanRural", "IMR"))
# 
# urbanRural_Maps_2 <- ggplot(cd_urbanRural_2) +
#   geom_sf(aes(fill = urbanRural, geometry = geometry), lwd = .1) +
#   theme_void() +
#   theme(axis.title = element_blank(), axis.text = element_blank(), 
#         axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5, vjust = 3),
#         legend.position = "bottom", strip.text = element_text(size = 10)) + 
#   ggtitle("Urban/Suburban/Rural Designation by Congressional District") +
#   facet_wrap(~ CONGRESS2) +
#   labs(fill = "Designation") 
# urbanRural_Maps_2
# ggsave(urbanRural_Maps_2, file=paste0(results_folder, "urbanRural_Maps_2.png"), device = agg_png, res = 300, units = "in",
#        width = 10, height = 7, dpi = 300)
# 
# urbanRural_IMR_boxPlot <- ggplot(cd_urbanRural_IMR, aes(x = urbanRural, y = IMR)) + 
#   geom_boxplot(aes(fill = urbanRural)) + 
#   geom_jitter() +
#   xlab("Urban/Rural Designation") +
#   labs(fill = "Designation") +
#   theme_bw() +
#   facet_wrap(~CONGRESS2)
# urbanRural_IMR_boxPlot
# ggsave(urbanRural_IMR_boxPlot, file=paste0(results_folder, "urbanRural_IMR_boxPlot.png"), device = agg_png, res = 300, units = "in",
#        width = 10, height = 7, dpi = 300)

################################################################################

#Sankey diagram

tract_to_111_113 <- inner_join(tract_to_cd111, tract_to_cd113) %>%
  group_by(cd111,cd113) %>%
  select(cd111,cd113) %>%
  mutate(cd111 = paste0('cd111-',sprintf('%02d',cd111)), cd113 = paste0('cd113-',sprintf('%02d',cd113)))

sankey_111_113 <- data_to_sankey(tract_to_111_113) %>%
  arrange(to, from)

template<-expand.grid(from=unique(sankey_111_113$from), to=unique(sankey_111_113$to))

sankey_111_113_full <- full_join(sankey_111_113, template) %>%
  arrange(from, to) %>%
  mutate_at(3, ~replace_na(.,0))

#Save as R Object
save(sankey_111_113_full, file = "./ShinyApp/sankey_111_113_full.Rdata")

sankey <- ggplot(sankey_111_113_full, aes(y = weight, axis1 = from, axis2 = to)) +
  geom_alluvium(aes(fill = from)) + 
  geom_stratum() + 
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_void() +
  theme(axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), legend.position = "none")
sankey
