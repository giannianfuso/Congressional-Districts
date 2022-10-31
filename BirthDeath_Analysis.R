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
library(classInt)
library(ggmap)
select <- dplyr::select

################################################################################


#Define global color themes to manually apply to charts
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
infMort_byCD <- group_by(death_cd, CONGRESS, CD) %>%
  summarise(InfantMortality = sum(INFMORT_CD, na.rm = TRUE))

#Infant Mortality by CD - combined Congresses
infMort_byCD_2 <- group_by(death_cd, CONGRESS2, CD) %>%
  summarise(InfantMortality = sum(INFMORT_CD, na.rm = TRUE))

#Live Births by CD
liveBirths_byCD <- group_by(birth_cd, CONGRESS, CD) %>%
  summarise(LiveBirths = sum(ALLOCATION))

#Live Births by CD - combined Congresses
liveBirths_byCD_2 <- group_by(birth_cd, CONGRESS2, CD) %>%
  summarise(LiveBirths = sum(ALLOCATION))

#IMR by CD
IMR_byCD <- inner_join(infMort_byCD, liveBirths_byCD) %>%
  mutate(IMR = InfantMortality/LiveBirths * 1000)

#IMR by CD - combined Congresses
IMR_byCD_2 <- inner_join(infMort_byCD_2, liveBirths_byCD_2) %>%
  mutate(IMR = InfantMortality/LiveBirths * 1000)

################################################################################

#Infant Mortality by CD & race-ethnicity
infMort_byCD_byRace <- group_by(death_cd, CONGRESS, CD, RACEHISP) %>%
  summarise(InfantMortality = sum(INFMORT_CD, na.rm = TRUE)) %>%
  filter(as.integer(RACEHISP)!=4) #Remove American Indian Alaska Native

#Infant Mortality by CD & race-ethnicity - combined Congresses
infMort_byCD_byRace_2 <- group_by(death_cd, CONGRESS2, CD, RACEHISP) %>%
  summarise(InfantMortality = sum(INFMORT_CD, na.rm = TRUE)) %>%
  filter(as.integer(RACEHISP)!=4) #Remove American Indian Alaska Native

#Infant Mortality by CD & race-ethnicity - combined Congresses - 116 boundaries
infMort_byCD_byRace_2_cd116 <- group_by(death_cd116, CONGRESS2, CD, RACEHISP) %>%
  summarise(InfantMortality = sum(INFMORT_CD, na.rm = TRUE)) %>%
  filter(as.integer(RACEHISP)!=4) #Remove American Indian Alaska Native

#Live Births by CD & race-ethnicity
liveBirths_byCD_byRace <- group_by(birth_cd, CONGRESS, CD, RACEHISP) %>%
  summarise(LiveBirths = sum(ALLOCATION)) %>%
  filter(as.integer(RACEHISP)!=4) #Remove American Indian Alaska Native

#Live Births by CD & race-ethnicity - combined Congresses
liveBirths_byCD_byRace_2 <- group_by(birth_cd, CONGRESS2, CD, RACEHISP) %>%
  summarise(LiveBirths = sum(ALLOCATION)) %>%
  filter(as.integer(RACEHISP)!=4) #Remove American Indian Alaska Native

#Live Births by CD & race-ethnicity - combined Congresses - 116 boundaries
liveBirths_byCD_byRace_2_cd116 <- group_by(birth_cd116, CONGRESS2, CD, RACEHISP) %>%
  summarise(LiveBirths = sum(ALLOCATION)) %>%
  filter(as.integer(RACEHISP)!=4) #Remove American Indian Alaska Native

#IMR by CD & race-ethnicity
IMR_byCD_byRace <- inner_join(infMort_byCD_byRace, liveBirths_byCD_byRace) %>%
  mutate(IMR = InfantMortality/LiveBirths * 1000)

#IMR by CD & race-ethnicity - combined Congresses
IMR_byCD_byRace_2 <- inner_join(infMort_byCD_byRace_2, liveBirths_byCD_byRace_2) %>%
  mutate(IMR = InfantMortality/LiveBirths * 1000, paired = as.integer(CD)) %>%
  select(-c(InfantMortality, LiveBirths))

#Save as R Object for Shiny app
save(IMR_byCD_byRace_2, file = "./ShinyApp/IMR_byCD_byRace_2.Rdata")

#IMR by CD & race-ethnicity - combined Congresses - with details
IMR_byCD_byRace_2_details <- inner_join(infMort_byCD_byRace_2, liveBirths_byCD_byRace_2) %>%
  filter(RACEHISP!="Unknown")%>%
  mutate(IMR = InfantMortality/LiveBirths * 1000, paired = as.integer(CD))

#IMR by CD & race-ethnicity - combined Congresses - 116 boundaries - with details
IMR_byCD_byRace_2_cd116_orig <- inner_join(infMort_byCD_byRace_2_cd116, liveBirths_byCD_byRace_2_cd116) %>%
  mutate(IMR = InfantMortality/LiveBirths * 1000, paired = as.integer(CD))

#IMR by CD & race-ethnicity - combined Congresses - 116 boundaries - details removed
IMR_byCD_byRace_2_cd116 <- inner_join(infMort_byCD_byRace_2_cd116, liveBirths_byCD_byRace_2_cd116) %>%
  mutate(IMR = InfantMortality/LiveBirths * 1000, paired = as.integer(CD)) %>%
  select(-c(InfantMortality, LiveBirths))

#Save as R Object for Shiny app
save(IMR_byCD_byRace_2_cd116, file = "./ShinyApp/IMR_byCD_byRace_2_cd116.Rdata")

################################################################################

#Infant Mortality by CD - Non-Hispanic White
infMort_byCD_byRace_nhw <- group_by(filter(death_cd, as.integer(RACEHISP) ==2), CONGRESS, CD, RACEHISP) %>%
  summarise(InfantMortality = sum(INFMORT_CD, na.rm = TRUE))

#Infant Mortality by CD - Non-Hispanic White - combined Congresses
infMort_byCD_byRace_nhw_2 <- group_by(filter(death_cd, as.integer(RACEHISP) ==2), CONGRESS2, CD, RACEHISP) %>%
  summarise(InfantMortality = sum(INFMORT_CD, na.rm = TRUE))

#Infant Mortality by CD - Non-Hispanic White - combined Congresses - 116 boundaries
infMort_byCD_byRace_nhw_2_cd116 <- group_by(filter(death_cd116, as.integer(RACEHISP) ==2), CONGRESS2, CD, RACEHISP) %>%
  summarise(InfantMortality = sum(INFMORT_CD, na.rm = TRUE))

#Live Births by CD - Non-Hispanic White
liveBirths_byCD_byRace_nhw <- group_by(filter(birth_cd, as.integer(RACEHISP)==2), CONGRESS, CD, RACEHISP) %>%
  summarise(LiveBirths = sum(ALLOCATION))

#Live Births by CD - Non-Hispanic White - combined Congresses
liveBirths_byCD_byRace_nhw_2 <- group_by(filter(birth_cd, as.integer(RACEHISP)==2), CONGRESS2, CD, RACEHISP) %>%
  summarise(LiveBirths = sum(ALLOCATION))

#Live Births by CD - Non-Hispanic White - combined Congresses - 116 boundaries
liveBirths_byCD_byRace_nhw_2_cd116 <- group_by(filter(birth_cd116, as.integer(RACEHISP)==2), CONGRESS2, CD, RACEHISP) %>%
  summarise(LiveBirths = sum(ALLOCATION))

#IMR by CD - Non-Hispanic White
IMR_byCD_byRace_nhw <- inner_join(infMort_byCD_byRace_nhw, liveBirths_byCD_byRace_nhw) %>%
  mutate(IMR_nhw = InfantMortality/LiveBirths * 1000)

#IMR by CD - Non-Hispanic White - combined Congresses
IMR_byCD_byRace_nhw_2 <- inner_join(infMort_byCD_byRace_nhw_2, liveBirths_byCD_byRace_nhw_2) %>%
  mutate(IMR_nhw = InfantMortality/LiveBirths * 1000)

#IMR by CD - Non-Hispanic White - combined Congresses - 116 boundaries
IMR_byCD_byRace_nhw_2_cd116 <- inner_join(infMort_byCD_byRace_nhw_2_cd116, liveBirths_byCD_byRace_nhw_2_cd116) %>%
  mutate(IMR_nhw = InfantMortality/LiveBirths * 1000)

#Absolute disparity by CD and race-ethnicity
IMR_byCD_byRace_absDisparity <- full_join(IMR_byCD_byRace, 
                                          IMR_byCD_byRace_nhw, 
                                          by = c("CONGRESS", "CD")) %>%
  select(-c("RACEHISP.y")) %>%
  rename(RACEHISP = RACEHISP.x) %>%
  mutate(IMR_absDisparity = (IMR-IMR_nhw))

#Absolute disparity by CD and race-ethnicity - combined Congresses
IMR_byCD_byRace_absDisparity_2 <- full_join(IMR_byCD_byRace_2_details, 
                                          IMR_byCD_byRace_nhw_2, 
                                          by = c("CONGRESS2", "CD")) %>%
  rename(RACEHISP = RACEHISP.x, LiveBirths = LiveBirths.x, LiveBirths_nhw = LiveBirths.y) %>%
  filter(as.integer(RACEHISP) < 6) %>%
  mutate(IMR_absDisparity = (IMR-IMR_nhw), paired = as.integer(CD), IMR_raw = IMR/1000, IMR_nhw_raw = IMR_nhw/1000,
         se = sqrt(IMR_raw*(1-IMR_raw)/LiveBirths + IMR_nhw_raw*(1-IMR_nhw_raw)/LiveBirths_nhw)*1000, lci = str_trim(format(round(IMR_absDisparity-1.96*se,2),nsmall=2),side="both"),
         uci = str_trim(format(round(IMR_absDisparity+1.96*se,2),nsmall=2),side="both")) %>%
  select(c(CONGRESS2, CD, RACEHISP, IMR_absDisparity, paired, se, lci, uci)) %>%
  rename(IMR = IMR_absDisparity)

#Save as R Object for Shiny app
save(IMR_byCD_byRace_absDisparity_2, file = "./ShinyApp/IMR_byCD_byRace_absDisparity_2.Rdata")

#Relative disparities by CD and race-ethnicity - combined Congresses
IMR_byCD_byRace_relDisparity_2 <- full_join(IMR_byCD_byRace_2_details, 
                                            IMR_byCD_byRace_nhw_2, 
                                            by = c("CONGRESS2", "CD")) %>%
  rename(RACEHISP = RACEHISP.x, LiveBirths = LiveBirths.x, LiveBirths_nhw = LiveBirths.y, 
         InfantMortality = InfantMortality.x, InfantMortality_nhw = InfantMortality.y) %>%
  filter(as.integer(RACEHISP) != 7) %>%
  mutate(IMR_relDisparity = (IMR/IMR_nhw), paired = as.integer(CD),
         se = sqrt((LiveBirths-InfantMortality)/(InfantMortality*LiveBirths)+(LiveBirths_nhw-InfantMortality_nhw)/(LiveBirths_nhw*InfantMortality_nhw)), 
         lci = str_trim(format(round(exp(log(IMR_relDisparity)-1.96*se),2),nsmall=2),side="both"), uci = str_trim(format(round(exp(log(IMR_relDisparity)+1.96*se),2),nsmall=2),side="both")) %>%
  select(c(CONGRESS2, CD, RACEHISP, IMR_relDisparity, paired, se, lci, uci)) %>%
  rename(IMR = IMR_relDisparity)

#Absolute disparity table
IMR_byCD_byRace_absDisparity_2_table <- IMR_byCD_byRace_absDisparity_2 %>%
  filter(as.integer(CONGRESS2) == 1, as.integer(RACEHISP) <= 6, RACEHISP != "Non-Hispanic White",
         RACEHISP!="Non-Hispanic Other") %>%
  mutate(IMR = str_trim(format(round(IMR,2),nsmall=2),side="both"),
    RACEHISP_CONGRESS = paste0(RACEHISP, " - ", CONGRESS2) %>%
           str_replace_all("Hispanic -", "H -") %>%
           str_replace_all("Non-Hispanic Black", "NHB") %>%
           str_replace_all("Non-Hispanic Asian American Pacific Islander", "NHAAPI")) %>%
  relocate(IMR, .before=lci)%>%
  unite("output", IMR:lci, sep=" (")%>%
  unite("output", output:uci, sep=", ")%>%
  mutate(output = paste0(output, ")"))%>%
  pivot_wider(names_from = RACEHISP_CONGRESS, values_from = output, values_fill = NA) %>%
  ungroup() %>%
  select(-c(CONGRESS2, RACEHISP, paired, se)) %>%
  group_by(CD) %>%
  summarise_all(list(~.[which.min(is.na(.))])) %>%
  bind_cols({
    IMR_byCD_byRace_absDisparity_2 %>%
      filter(as.integer(CONGRESS2) == 2, as.integer(RACEHISP) <= 6, RACEHISP != "Non-Hispanic White",
             RACEHISP!="Non-Hispanic Other") %>%
      mutate(IMR = str_trim(format(round(IMR,2),nsmall=2),side="both"),
             RACEHISP_CONGRESS = paste0(RACEHISP, " - ", CONGRESS2) %>%
               str_replace_all("Hispanic -", "H -") %>%
               str_replace_all("Non-Hispanic Black", "NHB") %>%
               str_replace_all("Non-Hispanic Asian American Pacific Islander", "NHAAPI")) %>%
      relocate(IMR, .before=lci)%>%
      unite("output", IMR:lci, sep=" (")%>%
      unite("output", output:uci, sep=", ")%>%
      mutate(output = paste0(output, ")"))%>%
      pivot_wider(names_from = RACEHISP_CONGRESS, values_from = output, values_fill = NA) %>%
      ungroup() %>%
      select(-c(CONGRESS2, RACEHISP, paired, se)) %>%
      add_row(CD = "CD 19") %>%
      group_by(CD) %>%
      summarise_all(list(~.[which.min(is.na(.))]))
  }) %>%
  select(-c(CD...5)) %>%
  rename(CD = CD...1)

#Absolute disparity table - write to csv
write.csv(IMR_byCD_byRace_absDisparity_2_table, "./Final Results/IMR_byCD_byRace_absDisparity_2_table.csv", row.names = FALSE)

#Relative disparity table
IMR_byCD_byRace_relDisparity_2_table <- IMR_byCD_byRace_relDisparity_2 %>%
  filter(as.integer(CONGRESS2) == 1, as.integer(RACEHISP) <= 6, RACEHISP != "Non-Hispanic White",
         RACEHISP!="Non-Hispanic Other") %>%
  mutate(IMR = str_trim(format(round(IMR,2),nsmall=2),side="both"),
         RACEHISP_CONGRESS = paste0(RACEHISP, " - ", CONGRESS2) %>%
           str_replace_all("Hispanic -", "H -") %>%
           str_replace_all("Non-Hispanic Black", "NHB") %>%
           str_replace_all("Non-Hispanic Asian American Pacific Islander", "NHAAPI")) %>%
  relocate(IMR, .before=lci)%>%
  unite("output", IMR:lci, sep=" (")%>%
  unite("output", output:uci, sep=", ")%>%
  mutate(output = ifelse(is.finite(se), paste0(output, ")"), NA)) %>%
  pivot_wider(names_from = RACEHISP_CONGRESS, values_from = output, values_fill = NA) %>%
  ungroup() %>%
  select(-c(CONGRESS2, RACEHISP, paired, se)) %>%
  group_by(CD) %>%
  summarise_all(list(~.[which.min(is.na(.))])) %>%
  bind_cols({
    IMR_byCD_byRace_relDisparity_2 %>%
      filter(as.integer(CONGRESS2) == 2, as.integer(RACEHISP) <= 6, RACEHISP != "Non-Hispanic White",
             RACEHISP!="Non-Hispanic Other") %>%
      mutate(IMR = str_trim(format(round(IMR,2),nsmall=2),side="both"),
             RACEHISP_CONGRESS = paste0(RACEHISP, " - ", CONGRESS2) %>%
               str_replace_all("Hispanic -", "H -") %>%
               str_replace_all("Non-Hispanic Black", "NHB") %>%
               str_replace_all("Non-Hispanic Asian American Pacific Islander", "NHAAPI")) %>%
      relocate(IMR, .before=lci)%>%
      unite("output", IMR:lci, sep=" (")%>%
      unite("output", output:uci, sep=", ")%>%
      mutate(output = ifelse(is.finite(se), paste0(output, ")"), NA)) %>%
      pivot_wider(names_from = RACEHISP_CONGRESS, values_from = output, values_fill = NA) %>%
      ungroup() %>%
      select(-c(CONGRESS2, RACEHISP, paired, se)) %>%
      add_row(CD = "CD 19") %>%
      group_by(CD) %>%
      summarise_all(list(~.[which.min(is.na(.))]))
  }) %>%
  select(-c(CD...5)) %>%
  rename(CD = CD...1)

#Relative disparity table - write to csv
write.csv(IMR_byCD_byRace_relDisparity_2_table, "./Final Results/IMR_byCD_byRace_relDisparity_2_table.csv", row.names = FALSE)

#Absolute disparity by CD and race-ethnicity - combined Congresses - 116 boundaries
IMR_byCD_byRace_absDisparity_2_cd116 <- full_join(IMR_byCD_byRace_2_cd116, 
                                            IMR_byCD_byRace_nhw_2_cd116, 
                                            by = c("CONGRESS2", "CD")) %>%
  select(-c("RACEHISP.y")) %>%
  rename(RACEHISP = RACEHISP.x) %>%
  mutate(IMR_absDisparity = (IMR-IMR_nhw)) %>%
  select(c(CONGRESS2, CD, RACEHISP, IMR_absDisparity, paired)) %>%
  rename(IMR = IMR_absDisparity) %>%
  filter(as.integer(RACEHISP) < 6)

#Save as R Object for Shiny app
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

#DoD by CD
DOD_byCD <- group_by(death_cd, year, CD) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE)) %>%
  mutate(year = as.numeric(year)) %>%
  left_join(population_pop_25_64_byCD) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population)) %>%
  inner_join(CONGRESS)

#DoD by CD - combined Congresses
DOD_byCD_2 <- group_by(death_cd, year, CD) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE)) %>%
  mutate(year = as.numeric(year)) %>%
  inner_join(CONGRESS) %>%
  left_join(population_pop_25_64_byCD) %>%
  group_by(CD, CONGRESS2) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population, na.rm = TRUE)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

################################################################################
#DoD by CD, Race, Sex, and Age
DOD_byRaceSexCD_deaths <- group_by(death_cd, CD, CONGRESS, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE)) %>%
  filter(as.integer(RACE)!=3) #Remove american indian alaksa native

#DoD by CD, Race, Sex, and Age - combined Congresses
DOD_byRaceSexCD_deaths_2 <- group_by(death_cd, CD, CONGRESS2, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE)) %>%
  filter(as.integer(RACE)!=3) #Remove american indian alaksa native

#DoD by CD, Race, and Age
DOD_byRaceCD_deaths_2 <- group_by(death_cd, CD, CONGRESS2, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE)) %>%
  filter(as.integer(RACE)!=3) #Remove american indian alaksa native

#DoD by CD, Race, Sex, and Age - combined Congresses - 116 boundaries
DOD_byRaceSexCD_deaths_2_cd116 <- group_by(death_cd116, CD, CONGRESS2, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE)) %>%
  filter(as.integer(RACE)!=3) #Remove american indian alaksa native

#DoD by CD, Race, and Age - combined Congresses - 166 boundaries
DOD_byRaceCD_deaths_2_cd116 <- group_by(death_cd116, CD, CONGRESS2, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE)) %>%
  filter(as.integer(RACE)!=3) #Remove american indian alaksa native

#DoD - White/Non-White - combined Congresses
DOD_byRace2SexCD_deaths_2 <- group_by(death_cd, CD, CONGRESS2, SEX, RACE2, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#DoD - White/Non-White - combined Congresses - 116 boundaries
DOD_byRace2SexCD_deaths_2_cd116 <- group_by(death_cd116, CD, CONGRESS2, SEX, RACE2, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#DoD MR by CD, Race, Sex, and Age
DOD_byRaceSexCD <- left_join(DOD_byRaceSexCD_deaths, population_byRaceSexAgeCD_byCongress) %>%
  group_by(CD, CONGRESS, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population), as.integer(RACE)!=3)

#DoD MR by CD, Race, Sex, and Age - combined Congresses 
DOD_byRaceSexCD_2 <- left_join(DOD_byRaceSexCD_deaths_2, population_byRaceSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population), as.integer(RACE)!=3) %>%
  select(-c(DOD, Population))

#Save as R Object for Shiny app
save(DOD_byRaceSexCD_2, file = "./ShinyApp/DOD_byRaceSexCD_2.Rdata")

#DoD MR by CD, Race, and Age - combined Congresses 
DOD_byRaceCD_2 <- left_join(DOD_byRaceSexCD_deaths_2, population_byRaceSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population), as.integer(RACE)!=3) %>%
  select(-c(DOD, Population))

#Save as R Object for Shiny app
save(DOD_byRaceCD_2, file = "./ShinyApp/DOD_byRaceCD_2.Rdata")

#DoD MR by CD, Race, Sex, and Age - combined Congresses - with details
DOD_byRaceSexCD_2_details <- left_join(DOD_byRaceSexCD_deaths_2, population_byRaceSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population), as.integer(RACE)!=3)

#DoD MR by CD, Race, and Age - combined Congresses - with details
DOD_byRaceCD_2_details <- inner_join(DOD_byRaceCD_deaths_2, population_byRaceAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, AGE_CAT_EDUC, RACE) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD))

#DoD MR by CD, Race, Sex, and Age - combined Congresses - 116 boundaries
DOD_byRaceSexCD_2_cd116 <- left_join(DOD_byRaceSexCD_deaths_2_cd116, population_byRaceSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population), as.integer(RACE)!=3) %>%
  select(-c(DOD, Population))

#DoD MR by CD, Race, and Age - combined Congresses - 116 boundaries
DOD_byRaceCD_2_cd116 <- left_join(DOD_byRaceSexCD_deaths_2_cd116, population_byRaceSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population), as.integer(RACE)!=3) %>%
  select(-c(DOD, Population))

#Save as R Object for Shiny app
save(DOD_byRaceCD_2_cd116, file = "./ShinyApp/DOD_byRaceCD_2_cd116.Rdata")

#DoD MR by CD, Race, Sex, and Age - combined Congresses - 116 boundaries - with details
DOD_byRaceSexCD_2_cd116_details <- left_join(DOD_byRaceSexCD_deaths_2_cd116, population_byRaceSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population), as.integer(RACE)!=3)

#DoD MR by CD, Race, and Age - combined Congresses - 116 boundaries - with details
DOD_byRaceCD_2_cd116_details <- left_join(DOD_byRaceCD_deaths_2_cd116, population_byRaceAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population), as.integer(RACE)!=3)

#DoD MR by CD, Race, Sex, and Age - combined Congresses - White/Non-White 
DOD_byRace2SexCD_2 <- left_join(DOD_byRace2SexCD_deaths_2, population_byRace2SexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, RACE2, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population)) %>%
  select(-c(DOD, Population))

#DoD MR by CD, Race, and Age - combined Congresses - White/Non-White 
DOD_byRace2CD_2 <- left_join(DOD_byRace2SexCD_deaths_2, population_byRace2SexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, RACE2, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population)) %>%
  select(-c(DOD, Population))

#Save as R Object for Shiny app
save(DOD_byRace2CD_2, file = "./ShinyApp/DOD_byRace2CD_2.Rdata")

#DoD MR by CD, Race, Sex, and Age - combined Congresses - White/Non-White - 116 boundaries
DOD_byRace2SexCD_2_cd116 <- left_join(DOD_byRace2SexCD_deaths_2_cd116, population_byRace2SexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, RACE2, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population)) %>%
  select(-c(DOD, Population))

#DoD MR by CD, Race, and Age - combined Congresses - White/Non-White - 116 boundaries
DOD_byRace2CD_2_cd116 <- left_join(DOD_byRace2SexCD_deaths_2_cd116, population_byRace2SexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, RACE2, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population)) %>%
  select(-c(DOD, Population))

#Save as R Object for Shiny app
save(DOD_byRace2CD_2_cd116, file = "./ShinyApp/DOD_byRace2CD_2_cd116.Rdata")
  
################################################################################

#DoD by CD, Race, Sex, and Age - White
DOD_byRaceSexCD_deaths_white <- group_by(filter(death_cd, as.integer(RACE) == 1), CD, CONGRESS, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#DoD by CD, Race, Sex, and Age - White - 116 boundaries
DOD_byRaceSexCD_deaths_white_cd116 <- group_by(filter(death_cd116, as.integer(RACE) == 1), CD, CONGRESS, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#DoD by CD, Race, Sex, and Age - White - combined Congresses
DOD_byRaceSexCD_deaths_white_2 <- group_by(filter(death_cd, as.integer(RACE) == 1), CD, CONGRESS2, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#DoD by CD, Race, Sex, and Age - White - 116 boundaries - combined Congresses
DOD_byRaceSexCD_deaths_white_2_cd116 <- group_by(filter(death_cd116, as.integer(RACE) == 1), CD, CONGRESS2, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#DoD MR by CD, Race, Sex, and Age - White
DOD_byRaceSexCD_white <- inner_join(DOD_byRaceSexCD_deaths_white, population_byRaceSexAgeCD_byCongress) %>%
  group_by(CD, CONGRESS, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR_white = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

#DoD MR by CD, Race, Sex, and Age - White - combined Congresses
DOD_byRaceSexCD_white_2 <- inner_join(DOD_byRaceSexCD_deaths_white_2, population_byRaceSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR_white = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

#DoD MR by CD, Race, and Age - White - combined Congresses
DOD_byRaceCD_white_2 <- inner_join(DOD_byRaceSexCD_deaths_white_2, population_byRaceSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR_white = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

#DoD MR by CD, Race, Sex, and Age - White - combined Congresses - 116 boundaries
DOD_byRaceSexCD_white_2_cd116 <- inner_join(DOD_byRaceSexCD_deaths_white_2_cd116, population_byRaceSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR_white = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

#DoD MR by CD, Race, and Age - White - combined Congresses - 116 boundaries
DOD_byRaceCD_white_2_cd116 <- inner_join(DOD_byRaceSexCD_deaths_white_2_cd116, population_byRaceSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR_white = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

#Absolute disparity by CD, Race, Sex, & Age
DOD_byRaceSexCD_absDisparity <- full_join(DOD_byRaceSexCD, 
                                          DOD_byRaceSexCD_white, 
                                          c("CD", "CONGRESS", "SEX", "AGE_CAT_EDUC")) %>%
  mutate(MR_absDisparity = (MR-MR_white)) %>%
  select(-c("DOD.x", "Population.x", "DOD.y", "RACE.y", "Population.y")) %>%
  rename(RACE = "RACE.x")

#Absolute disparity by CD, Race, Sex, & Age - combined Congresses
DOD_byRaceSexCD_absDisparity_2 <- full_join(DOD_byRaceSexCD_2_details, 
                                            DOD_byRaceSexCD_white_2, 
                                            c("CD", "CONGRESS2", "SEX", "AGE_CAT_EDUC")) %>%
  rename(RACE = RACE.x, DOD = DOD.x, DOD_white= DOD.y, Population = Population.x, Population_white = Population.y) %>%
  filter(as.integer(RACE) < 5) %>%
  mutate(MR_absDisparity = (MR-MR_white), MR_raw = MR/1000, MR_white_raw = MR_white/1000,
         se = sqrt(MR_raw*(1-MR_raw)/Population + MR_white_raw*(1-MR_white_raw)/Population_white)*1000, lci = str_trim(format(round(MR_absDisparity-1.96*se,2),nsmall=2),side="both"),
         uci = str_trim(format(round(MR_absDisparity+1.96*se,2),nsmall=2),side="both")) %>%
  select(c(CONGRESS2, CD, AGE_CAT_EDUC, RACE, MR_absDisparity, paired, se, lci, uci)) %>%
  rename(MR = MR_absDisparity)

#Absolute disparity by CD, Race, & Age - combined Congresses
DOD_byRaceCD_absDisparity_2 <- full_join(DOD_byRaceCD_2_details, 
                                            DOD_byRaceCD_white_2, 
                                            c("CD", "CONGRESS2", "AGE_CAT_EDUC")) %>%
  rename(RACE = RACE.x, DOD = DOD.x, DOD_white= DOD.y, Population = Population.x, Population_white = Population.y) %>%
  filter(as.integer(RACE) < 5) %>%
  mutate(MR_absDisparity = (MR-MR_white), MR_raw = MR/1000, MR_white_raw = MR_white/1000,
         se = sqrt(MR_raw*(1-MR_raw)/Population + MR_white_raw*(1-MR_white_raw)/Population_white)*1000, lci = str_trim(format(round(MR_absDisparity-1.96*se,2),nsmall=2),side="both"),
         uci = str_trim(format(round(MR_absDisparity+1.96*se,2),nsmall=2),side="both")) %>%
  select(c(CONGRESS2, CD, AGE_CAT_EDUC, RACE, MR_absDisparity, paired, se, lci, uci)) %>%
  rename(MR = MR_absDisparity)

#Save as R Object for Shiny app
save(DOD_byRaceCD_absDisparity_2, file = "./ShinyApp/DOD_byRaceCD_absDisparity_2.Rdata")

#Absolute disparity by CD, Race, Sex, & Age - combined Congresses - 116 boundaries
DOD_byRaceSexCD_absDisparity_2_cd116 <- full_join(DOD_byRaceSexCD_2_cd116, 
                                                  DOD_byRaceSexCD_white_2_cd116, 
                                                  c("CD", "CONGRESS2", "SEX", "AGE_CAT_EDUC")) %>%
  mutate(MR_absDisparity = (MR-MR_white)) %>%
  select(-c("DOD", "Population", "DOD", "RACE.y", "MR", "MR_white")) %>%
  rename(RACE = "RACE.x", MR = "MR_absDisparity")

#Absolute disparity by CD, Race, & Age - combined Congresses - 116 boundaries
DOD_byRaceCD_absDisparity_2_cd116 <- full_join(DOD_byRaceCD_2_cd116_details, 
                                                  DOD_byRaceCD_white_2_cd116, 
                                                  c("CD", "CONGRESS2", "AGE_CAT_EDUC")) %>%
  rename(RACE = RACE.x, DOD = DOD.x, DOD_white= DOD.y, Population = Population.x, Population_white = Population.y) %>%
  filter(as.integer(RACE) < 5) %>%
  mutate(MR_absDisparity = (MR-MR_white)) %>%
  mutate(MR_absDisparity = (MR-MR_white), MR_raw = MR/1000, MR_white_raw = MR_white/1000,
         se = sqrt(MR_raw*(1-MR_raw)/Population + MR_white_raw*(1-MR_white_raw)/Population_white)*1000, lci = str_trim(format(round(MR_absDisparity-1.96*se,2),nsmall=2),side="both"),
         uci = str_trim(format(round(MR_absDisparity+1.96*se,2),nsmall=2),side="both")) %>%
  select(c(CONGRESS2, CD, AGE_CAT_EDUC, RACE, MR_absDisparity, paired, se, lci, uci)) %>%
  rename(MR = MR_absDisparity)

#Save as R Object for Shiny app
save(DOD_byRaceCD_absDisparity_2_cd116, file = "./ShinyApp/DOD_byRaceCD_absDisparity_2_cd116.Rdata")

################################################################################
#DoD by CD, Education, Sex, and Age
DOD_byEducSexCD_deaths <- group_by(death_cd, CD, CONGRESS, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#DoD by CD, Education, Sex, and Age - combined Congresses
DOD_byEducSexCD_deaths_2 <- group_by(death_cd, CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#DoD by CD, Education, and Age - combined Congresses
DOD_byEducCD_deaths_2 <- group_by(death_cd, CD, CONGRESS2, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#DoD by CD, Education, Sex, and Age - combined Congresses - 116 boundaries
DOD_byEducSexCD_deaths_2_cd116 <- group_by(death_cd116, CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#DoD by CD, Education, and Age - combined Congresses - 116 boundaries
DOD_byEducCD_deaths_2_cd116 <- group_by(death_cd116, CD, CONGRESS2, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#DoD MR by CD, Education, Sex, and Age
DOD_byEducSexCD <- left_join(DOD_byEducSexCD_deaths, population_byEducSexAgeCD_byCongress) %>%
  group_by(CD, CONGRESS, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

#DoD MR by CD, Education, Sex, and Age - combined Congresses
DOD_byEducSexCD_2 <- left_join(DOD_byEducSexCD_deaths_2, population_byEducSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population)) %>%
  select(-c(DOD, Population))

#DoD MR by CD, Education, and Age - combined Congresses
DOD_byEducCD_2 <- left_join(DOD_byEducSexCD_deaths_2, population_byEducSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population)) %>%
  select(-c(DOD, Population))

#Save as R Object for Shiny app
save(DOD_byEducCD_2, file = "./ShinyApp/DOD_byEducCD_2.Rdata")

#DoD MR by CD, Education, Sex, and Age - combined Congresses - with details
DOD_byEducSexCD_2_details <- left_join(DOD_byEducSexCD_deaths_2, population_byEducSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population))

#DoD MR by CD, Education, and Age - combined Congresses - with details
DOD_byEducCD_2_details <- inner_join(DOD_byEducCD_deaths_2, population_byEducAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, AGE_CAT_EDUC, EDUC) %>%
  summarise(DOD = sum(DOD), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD))

#DoD MR by CD, Education, Sex, and Age - combined Congresses - with details - 116 boundaries
DOD_byEducSexCD_2_cd116_orig <- left_join(DOD_byEducSexCD_deaths_2_cd116, population_byEducSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

#DoD MR by CD, Education, and Age - combined Congresses - with details - 116 boundaries
DOD_byEducCD_2_cd116_details <- inner_join(DOD_byEducCD_deaths_2_cd116, population_byEducAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

#DoD MR by CD, Education, Sex, and Age - combined Congresses - 116 boundaries
DOD_byEducSexCD_2_cd116 <- left_join(DOD_byEducSexCD_deaths_2_cd116, population_byEducSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population)) %>%
  select(-c(DOD, Population))

#DoD MR by CD, Education, and Age - combined Congresses - 116 boundaries
DOD_byEducCD_2_cd116 <- left_join(DOD_byEducSexCD_deaths_2_cd116, population_byEducSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population)) %>%
  select(-c(DOD, Population))

#Save as R Object
save(DOD_byEducCD_2_cd116, file = "./ShinyApp/DOD_byEducCD_2_cd116.Rdata")

################################################################################
#DoD by CD, Education, Sex, and Age - college
DOD_byEducSexCD_deaths_college <- group_by(filter(death_cd, as.integer(EDUC) == 4), CD, CONGRESS, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#DoD by CD, Education, Sex, and Age - college - combined Congresses
DOD_byEducSexCD_deaths_college_2 <- group_by(filter(death_cd, as.integer(EDUC) == 4), CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#DoD by CD, Education, Sex, and Age - college - combined Congresses - 116 boundaries
DOD_byEducSexCD_deaths_college_2_cd116 <- group_by(filter(death_cd116, as.integer(EDUC) == 4), CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#DoD MR by CD, Education, Sex, and Age - college
DOD_byEducSexCD_college <- inner_join(DOD_byEducSexCD_deaths_college, population_byEducSexAgeCD_byCongress) %>%
  group_by(CD, CONGRESS, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR_college = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

#DoD MR by CD, Education, Sex, and Age - college - combined Congresses
DOD_byEducSexCD_college_2 <- inner_join(DOD_byEducSexCD_deaths_college_2, population_byEducSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR_college = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

#DoD MR by CD, Education, and Age - college - combined Congresses
DOD_byEducCD_college_2 <- DOD_byEducCD_2_details %>%
  filter(as.integer(EDUC)==4) %>%
  rename(MR_college = MR)

#DoD MR by CD, Education, Sex, and Age - college - combined Congresses - 116 boundaries
DOD_byEducSexCD_college_2_cd116 <- inner_join(DOD_byEducSexCD_deaths_college_2_cd116, population_byEducSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, SEX, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR_college = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

#DoD MR by CD, Education, and Age - college - combined Congresses - 116 boundaries
DOD_byEducCD_college_2_cd116 <- inner_join(DOD_byEducSexCD_deaths_college_2_cd116, population_byEducSexAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR_college = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

#DoD absolute disparity by CD, Education, Sex, and Age
DOD_byEducSexCD_absDisparity <- full_join(DOD_byEducSexCD, 
                                       DOD_byEducSexCD_college,
                                       c("CD", "CONGRESS", "SEX", "AGE_CAT_EDUC")) %>%
  mutate(MR_absDisparity = (MR-MR_college)) %>%
  select(-c("DOD.x", "Population.x", "DOD.y", "EDUC.y", "Population.y")) %>%
  rename(EDUC = "EDUC.x")

#DoD absolute disparity by CD, Education, Sex, and Age - combined Congresses
DOD_byEducSexCD_absDisparity_2 <- full_join(DOD_byEducSexCD_2, 
                                          DOD_byEducSexCD_college_2,
                                          c("CD", "CONGRESS2", "SEX", "AGE_CAT_EDUC")) %>%
  mutate(MR_absDisparity = (MR-MR_college), paired = as.integer(CD)) %>%
  select(-c("EDUC.y", "DOD", "Population")) %>%
  rename(EDUC = "EDUC.x")

#DoD absolute disparities by CD, Education, and Age - combined Congresses
DOD_byEducCD_absDisparity_2 <- full_join(DOD_byEducCD_2_details, 
                                         DOD_byEducCD_college_2,
                                         c("CD", "CONGRESS2", "AGE_CAT_EDUC")) %>%
  rename(EDUC = EDUC.x, DOD = DOD.x, DOD_college = DOD.y, Population = Population.x, Population_college = Population.y) %>%
  filter(as.integer(EDUC) < 5) %>%
  mutate(MR_absDisparity = (MR-MR_college), MR_raw = MR/1000, MR_college_raw = MR_college/1000,
         se = sqrt(MR_raw*(1-MR_raw)/Population + MR_college_raw*(1-MR_college_raw)/Population_college)*1000, lci = str_trim(format(round(MR_absDisparity-1.96*se,2),nsmall=2),side="both"),
         uci = str_trim(format(round(MR_absDisparity+1.96*se,2),nsmall=2),side="both"), paired = as.integer(CD)) %>%
  select(c(CONGRESS2, CD, AGE_CAT_EDUC, EDUC, MR_absDisparity, paired, se, lci, uci)) %>%
  rename(MR = MR_absDisparity)

#Save as R Object for Shiny app
save(DOD_byEducCD_absDisparity_2, file = "./ShinyApp/DOD_byEducCD_absDisparity_2.Rdata")

#DoD relative disparities by CD, Education, Sex, and Age - combined Congresses
DOD_byEducSexCD_relDisparity_2 <- full_join(DOD_byEducSexCD_2, 
                                            DOD_byEducSexCD_college_2,
                                            c("CD", "CONGRESS2", "SEX", "AGE_CAT_EDUC")) %>%
  mutate(MR_relDisparity = (MR/MR_college), paired = as.integer(CD)) %>%
  select(-c("EDUC.y", "DOD", "Population")) %>%
  rename(EDUC = "EDUC.x")

#DoD relative disparities by CD, Education, and Age - combined Congresses
DOD_byEducCD_relDisparity_2 <- full_join(DOD_byEducCD_2_details, 
                                            DOD_byEducCD_college_2,
                                            c("CD", "CONGRESS2", "AGE_CAT_EDUC")) %>%
  rename(EDUC = EDUC.x, Population = Population.x, Population_college = Population.y, 
         DOD = DOD.x, DOD_college = DOD.y) %>%
  filter(as.integer(EDUC)!=4) %>%
  mutate(MR_relDisparity = (MR/MR_college), paired = as.integer(CD),
         se = sqrt((Population-DOD)/(DOD*Population)+(Population_college-DOD_college)/(Population_college*DOD_college)), 
         lci = str_trim(format(round(exp(log(MR_relDisparity)-1.96*se),2),nsmall=2),side="both"), 
         uci = str_trim(format(round(exp(log(MR_relDisparity)+1.96*se),2),nsmall=2),side="both")) %>%
  select(c(CONGRESS2, CD, EDUC, AGE_CAT_EDUC, MR_relDisparity, paired, se, lci, uci)) %>%
  rename(MR = MR_relDisparity)

#DoD relative disparities by CD, Education, Sex, and Age - combined Congresses - 116 boundaries
DOD_byEducSexCD_absDisparity_2_cd116 <- full_join(DOD_byEducSexCD_2_cd116, 
                                            DOD_byEducSexCD_college_2_cd116,
                                            c("CD", "CONGRESS2", "SEX", "AGE_CAT_EDUC")) %>%
  mutate(MR_absDisparity = (MR-MR_college), paired = as.integer(CD)) %>%
  select(-c("EDUC.y", "DOD", "Population")) %>%
  rename(EDUC = "EDUC.x")

#DoD relative disparities by CD, Education, and Age - combined Congresses - 116 boundaries
DOD_byEducCD_absDisparity_2_cd116 <- full_join(DOD_byEducCD_2_cd116_details, 
                                                  DOD_byEducCD_college_2_cd116,
                                                  c("CD", "CONGRESS2", "AGE_CAT_EDUC")) %>%
  rename(EDUC = EDUC.x, DOD = DOD.x, DOD_college = DOD.y, Population = Population.x, Population_college = Population.y) %>%
  filter(as.integer(EDUC) < 5) %>%
  mutate(MR_absDisparity = (MR-MR_college), MR_raw = MR/1000, MR_college_raw = MR_college/1000,
         se = sqrt(MR_raw*(1-MR_raw)/Population + MR_college_raw*(1-MR_college_raw)/Population_college)*1000, lci = str_trim(format(round(MR_absDisparity-1.96*se,2),nsmall=2),side="both"),
         uci = str_trim(format(round(MR_absDisparity+1.96*se,2),nsmall=2),side="both"), paired = as.integer(CD)) %>%
  select(c(CONGRESS2, CD, AGE_CAT_EDUC, EDUC, MR_absDisparity, paired, se, lci, uci)) %>%
  rename(MR = MR_absDisparity)

#Save as R Object for Shiny app
save(DOD_byEducCD_absDisparity_2_cd116, file = "./ShinyApp/DOD_byEducCD_absDisparity_2_cd116.Rdata")

#DoD absolute disparities by CD, Education, Sex, and Age table
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

#DoD absolute disparities by CD, Education, Sex, and Age table - write to csv
write.csv(DOD_byEducSexCD_absDisparity_2_table, "./Final Results/DOD_byEducSexCD_absDisparity_2_table.csv", row.names = FALSE)

#DoD absolute disparities by CD, Education, and Age table
DOD_byEducCD_absDisparity_2_table <- DOD_byEducCD_absDisparity_2 %>%
  filter(as.integer(CONGRESS2) == 1, as.integer(EDUC) < 4) %>%
  mutate(
    MR = str_trim(format(round(MR,2),nsmall=2),side="both"),
    EDUC_CONGRESS = paste0(EDUC, " - ", CONGRESS2) %>%
           str_replace_all("Less than High School", "LHS") %>%
           str_replace_all("High School", "HS") %>%
           str_replace_all("Some College/Associate Degree", "SCAD") %>%
           str_replace_all("Bachelor/Master/Doctorate/Professional Degree", "BMDP")) %>%
  relocate(MR, .before=lci)%>%
  unite("output", MR:lci, sep=" (")%>%
  unite("output", output:uci, sep=", ")%>%
  mutate(output = paste0(output, ")"))%>%
  pivot_wider(names_from = EDUC_CONGRESS, values_from = output, values_fill = NA) %>%
  ungroup() %>%
  select(-c(CONGRESS2, EDUC, paired, se)) %>%
  group_by(CD, AGE_CAT_EDUC) %>%
  summarise_all(list(~.[which.min(is.na(.))])) %>%
  arrange(AGE_CAT_EDUC, CD) %>%
  bind_cols({
    DOD_byEducCD_absDisparity_2 %>%
      filter(as.integer(CONGRESS2) == 2, as.integer(EDUC) < 4) %>%
      mutate(
        MR = str_trim(format(round(MR,2),nsmall=2),side="both"),
        EDUC_CONGRESS = paste0(EDUC, " - ", CONGRESS2) %>%
               str_replace_all("Less than High School", "LHS") %>%
               str_replace_all("High School", "HS") %>%
               str_replace_all("Some College/Associate Degree", "SCAD") %>%
               str_replace_all("Bachelor/Master/Doctorate/Professional Degree", "BMDP")) %>%
      relocate(MR, .before=lci)%>%
      unite("output", MR:lci, sep=" (")%>%
      unite("output", output:uci, sep=", ")%>%
      mutate(output = paste0(output, ")"))%>%
      pivot_wider(names_from = EDUC_CONGRESS, values_from = output, values_fill = NA) %>%
      ungroup() %>%
      select(-c(CONGRESS2, EDUC, paired, se)) %>%
      add_row(CD = "CD 19", AGE_CAT_EDUC = "25 to 34 years") %>%
      add_row(CD = "CD 19", AGE_CAT_EDUC = "35 to 44 years") %>%
      add_row(CD = "CD 19", AGE_CAT_EDUC = "45 to 64 years") %>%
      group_by(CD, AGE_CAT_EDUC) %>%
      summarise_all(list(~.[which.min(is.na(.))])) %>%
      arrange(AGE_CAT_EDUC, CD) %>%
      ungroup() %>%
      select(-c(CD, AGE_CAT_EDUC))
  })

#DoD absolute disparities by CD, Education, and Age table - write to csv
write.csv(DOD_byEducCD_absDisparity_2_table, "./Final Results/DOD_byEducCD_absDisparity_2_table.csv", row.names = FALSE)

#DoD relative disparities by CD, Education, Sex, and Age table
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

#DoD relative disparities by CD, Education, Sex, and Age table - write to csv
write.csv(DOD_byEducSexCD_relDisparity_2_table, "./Final Results/DOD_byEducSexCD_relDisparity_2_table.csv", row.names = FALSE)

#DoD relative disparities by CD, Education, and Age table
DOD_byEducCD_relDisparity_2_table <- DOD_byEducCD_relDisparity_2 %>%
  filter(as.integer(CONGRESS2) == 1, as.integer(EDUC) < 4) %>%
  mutate(
    MR = str_trim(format(round(MR,2),nsmall=2),side="both"),
    EDUC_CONGRESS = paste0(EDUC, " - ", CONGRESS2) %>%
           str_replace_all("Less than High School", "LHS") %>%
           str_replace_all("High School", "HS") %>%
           str_replace_all("Some College/Associate Degree", "SCAD") %>%
           str_replace_all("Bachelor/Master/Doctorate/Professional Degree", "BMDP")) %>%
  relocate(MR, .before=lci)%>%
  unite("output", MR:lci, sep=" (")%>%
  unite("output", output:uci, sep=", ")%>%
  mutate(output = ifelse(is.finite(se), paste0(output, ")"), NA)) %>%
  pivot_wider(names_from = EDUC_CONGRESS, values_from = output, values_fill = NA) %>%
  ungroup() %>%
  select(-c(paired, CONGRESS2, EDUC, se)) %>%
  group_by(CD, AGE_CAT_EDUC) %>%
  summarise_all(list(~.[which.min(is.na(.))])) %>%
  arrange(AGE_CAT_EDUC, CD) %>%
  bind_cols({
    DOD_byEducCD_relDisparity_2 %>%
      filter(as.integer(CONGRESS2) == 2, as.integer(EDUC) < 4) %>%
      mutate(
        MR = str_trim(format(round(MR,2),nsmall=2),side="both"),
        EDUC_CONGRESS = paste0(EDUC, " - ", CONGRESS2) %>%
               str_replace_all("Less than High School", "LHS") %>%
               str_replace_all("High School", "HS") %>%
               str_replace_all("Some College/Associate Degree", "SCAD") %>%
               str_replace_all("Bachelor/Master/Doctorate/Professional Degree", "BMDP")) %>%
      relocate(MR, .before=lci)%>%
      unite("output", MR:lci, sep=" (")%>%
      unite("output", output:uci, sep=", ")%>%
      mutate(output = ifelse(is.finite(se), paste0(output, ")"), NA)) %>%
      pivot_wider(names_from = EDUC_CONGRESS, values_from = output, values_fill = NA) %>%
      ungroup() %>%
      select(-c(paired, CONGRESS2, EDUC)) %>%
      add_row(CD = "CD 19", AGE_CAT_EDUC = "25 to 34 years") %>%
      add_row(CD = "CD 19", AGE_CAT_EDUC = "35 to 44 years") %>%
      add_row(CD = "CD 19", AGE_CAT_EDUC = "45 to 64 years") %>%
      group_by(CD, AGE_CAT_EDUC) %>%
      summarise_all(list(~.[which.min(is.na(.))])) %>%
      arrange(AGE_CAT_EDUC, CD) %>%
      ungroup() %>%
      select(-c(CD, AGE_CAT_EDUC, se))
  })

#DoD relative disparities by CD, Education, and Age table - write to csv
write.csv(DOD_byEducCD_relDisparity_2_table, "./Final Results/DOD_byEducCD_relDisparity_2_table.csv", row.names = FALSE)

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

################################################################################

#Data frames for IMR Maps
#IMR Maps by CD
IMR_Maps_byCD <- inner_join(cd_outlines, IMR_byCD) %>%
  group_by(CD, CONGRESS) %>%
  summarise(CD, IMR = sum(InfantMortality)/sum(LiveBirths)*1000, geometry)

#IMR Maps by CD - combined Congresses
IMR_Maps_byCD_2 <- inner_join(cd_outlines_2, IMR_byCD_2) %>%
  group_by(CD, CONGRESS2) %>%
  summarise(IMR = sum(InfantMortality)/sum(LiveBirths)*1000, geometry)%>%
  ungroup() %>%
  mutate(jenks=cut(IMR, breaks=classIntervals(IMR,n=5,style="jenks")$brks,include.lowest=T))

#Save as R Object for Shiny app
save(IMR_Maps_byCD_2, file = "./ShinyApp/IMR_Maps_byCD_2.Rdata")

#IMR Maps by CD - combined Congresses - 116 boundaries
IMR_Maps_cd116 <- inner_join(cd116, IMR_byCD_2) %>%
  select(-c(InfantMortality, LiveBirths)) %>%
  filter(as.integer(CONGRESS2)==2)%>%
  ungroup() %>%
  mutate(jenks=cut(IMR, breaks=classIntervals(IMR,n=5,style="jenks")$brks,include.lowest=T))

#Save as R Object for Shiny app
save(IMR_Maps_cd116, file = "./ShinyApp/IMR_Maps_cd116.Rdata")

################################################################################

#Data frames for DoD Maps
#DoD Maps by CD
DOD_Maps_byCD <- inner_join(cd_outlines, DOD_byCD) %>%
  group_by(CD, CONGRESS) %>%
  summarise(CD, MR = sum(DOD)/sum(Population)*10000, geometry)

#DoD Maps by CD - combined Congresses
DOD_Maps_byCD_2 <- inner_join(cd_outlines_2, DOD_byCD_2) %>%
  group_by(CD, CONGRESS2) %>%
  summarise(CD, MR)%>%
  ungroup() %>%
  mutate(jenks=cut(MR, breaks=classIntervals(MR,n=5,style="jenks")$brks,include.lowest=T))

#Save as R Object for Shiny app
save(DOD_Maps_byCD_2, file = "./ShinyApp/DOD_Maps_byCD_2.Rdata")

#By Congress2 for GIS maps
DOD_111_112 <- DOD_byCD_2 %>%
  ungroup() %>%
  mutate(jenks=cut(MR, breaks=classIntervals(MR,n=5,style="jenks")$brks,include.lowest=T)) %>%
  filter(as.integer(CONGRESS2)==1) %>%
  select(CD, MR, jenks)

DOD_113_114 <- DOD_byCD_2 %>%
  ungroup() %>%
  mutate(jenks=cut(MR, breaks=classIntervals(MR,n=5,style="jenks")$brks,include.lowest=T)) %>%
  filter(as.integer(CONGRESS2)==2) %>%
  select(CD, MR, jenks)

#Save as CSV
write.csv(DOD_111_112, "./Final Results/DOD_111_112.csv", row.names = F)
write.csv(DOD_113_114, "./Final Results/DOD_113_114.csv", row.names = F)

#DoD Maps by CD - combined Congresses - 116 boundaries
DOD_Maps_cd116 <- inner_join(cd116, DOD_byCD_2) %>%
  select(-c(DOD, Population))%>%
  ungroup() %>%
  mutate(jenks=cut(MR, breaks=classIntervals(MR,n=5,style="jenks")$brks,include.lowest=T))

#Save as R Object for Shiny app
save(DOD_Maps_cd116, file = "./ShinyApp/DOD_Maps_cd116.Rdata")

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

#IMR Maps - combined Congresses
IMR_Maps_2 <- ggplot(IMR_Maps_byCD_2) +
  geom_sf(aes(fill = jenks), lwd = .1) + 
  annotate("point", x = -75.1652, y = 39.9526, colour = "black", size = 2) +
  annotate("point", x = -79.9959, y = 40.4406, colour = "black", size = 2) +
  coord_sf(default_crs = sf::st_crs(4326)) +
  theme_void() +
  theme(axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5, vjust = 3),
        legend.position = "bottom", strip.text = element_text(size = 20), 
        legend.text = element_text(size = 20), legend.title = element_text(size = 20)) + 
  facet_wrap(~ CONGRESS2, ncol = 1) +
  guides(fill=guide_legend(nrow=2, byrow = T)) +
  scale_fill_manual(values = colorRampPalette(colors = c("white", "red"))(5))+
  labs(fill = "IMR per 1,000 Live Births")
IMR_Maps_2

################################################################################
#IMR plots by CD & race-ethnicity - combined Congresses
IMR_byCD_byRace_Plots_2 <- ggplot(filter(IMR_byCD_byRace_2, as.integer(RACEHISP) < 6, IMR > 0),
                                  aes(x = CD, y = IMR)) + 
  geom_line() + 
  geom_point(size = 3, aes(color = RACEHISP)) +
  scale_color_manual(values = c("Hispanic" = color_Hispanic,
                                "Non-Hispanic Asian American Pacific Islander" = color_AAPI, 
                                "Non-Hispanic Black" = color_Black, "Non-Hispanic White" = color_White)) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15),
        legend.position = "bottom", legend.text=element_text(size=20),
        strip.text = element_text(size = 16, face = "bold"), legend.title = element_blank(),
        text = element_text(color="black"), strip.background = element_blank()) + 
  guides(color = guide_legend(nrow = 2,  byrow = TRUE)) +
  xlab("Congressional District") +
  ylab("IMR per 1,000 Live Births") +
  labs(color = "Race/Hispanic Origin") + 
  facet_wrap(~ CONGRESS2, ncol = 1) +
  coord_flip()
IMR_byCD_byRace_Plots_2

#IMR plots by CD & race-ethnicity - combined Congresses - CD111-CD112
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

#IMR plots by CD & race-ethnicity - combined Congresses - CD113-CD114
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

#DOD Maps - Combined congresses
DOD_Maps_2 <- ggplot(DOD_Maps_byCD_2) +
  geom_sf(aes(fill = jenks), lwd = .1) +
  annotate("point", x = -75.1652, y = 39.9526, colour = "black", size = 2) +
  annotate("point", x = -79.9959, y = 40.4406, colour = "black", size = 2) +
  coord_sf(default_crs = sf::st_crs(4326)) +
  theme_void() +
  theme(axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5, vjust = 3),
        legend.position = "bottom", strip.text = element_text(size = 20), 
        legend.text = element_text(size = 20), legend.title = element_text(size = 20)) + 
  facet_wrap(~ CONGRESS2, ncol = 1) +
  guides(fill=guide_legend(nrow=2, byrow = T)) +
  scale_fill_manual(values = colorRampPalette(colors = c("white", "red"))(5))+
  labs(fill = "Mortality Rate, \nper 10,000 People")
DOD_Maps_2

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
        legend.position = "bottom", legend.text=element_text(size=20),
        strip.text = element_text(size = 16, face = "bold"), legend.title = element_blank(),
        text = element_text(color="black"), strip.background = element_blank()) + 
  guides(color = guide_legend(nrow = 2)) +
  scale_y_continuous(trans = 'log10', breaks = c(1,10,100,1000), limits=c(.1, NA), labels = c(1,10,100,1000)) +
  scale_x_discrete(expand=expansion(mult=c(0.1, 0.05))) +
  annotation_logticks(sides="b") + 
  coord_flip()
DOD_byEducSexAge_Plot

#All years - no sex
DOD_byEducAge_Plot <- ggplot(filter(DOD_byEducCD_2, MR > 0, as.integer(EDUC) < 5),
                                aes(x = CD, y = MR)) + 
  geom_line() + 
  geom_point(size = 3, aes(color = EDUC)) +
  scale_color_manual(values = c("Less than High School" = color_LessHS, "High School" = color_HS, 
                                "Some College/Associate Degree" = color_SomeCollege, "Bachelor/Master/Doctorate/Professional Degree" = color_Bachelor)) + 
  xlab("Congressional District") +
  ylab("Mortality Rate, per 10,000 People") +
  labs(color = "Education") + 
  facet_nested_wrap(vars(CONGRESS2, AGE_CAT_EDUC), dir = "h", nrow = 2, ncol = 3) +
  theme_bw() + 
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15),
        legend.position = "bottom", legend.text=element_text(size=20),
        strip.text = element_text(size = 16, face = "bold"), legend.title = element_blank(),
        text = element_text(color="black"), strip.background = element_blank()) + 
  guides(color = guide_legend(nrow = 2)) +
  scale_y_continuous(trans = 'log10', breaks = c(1,10,100,1000), limits=c(.1, NA), labels = c(1,10,100,1000)) +
  scale_x_discrete(expand=expansion(mult=c(0.1, 0.05))) +
  annotation_logticks(sides="b") + 
  coord_flip()
DOD_byEducAge_Plot

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
