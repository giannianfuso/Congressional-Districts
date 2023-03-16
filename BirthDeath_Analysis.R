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
################################# Colors for Charts ############################
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
################################## Map Boundaries ##############################
################################################################################

#Get CD Map Boundaries

#Pulled shapefiles from https://cdmaps.polisci.ucla.edu/

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

#CD Map Boundaries by Congress
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

#CD Map Boundaries by Congress - combined Congresses
cd_outlines_2 <- filter(cd_outlines, as.integer(CONGRESS) == 1 | as.integer(CONGRESS) == 3) %>%
  inner_join(CONGRESS2)

################################################################################
########################## Total IMR Calculations ##############################
################################################################################

#Create data tables for subsequent analysis

#Total IMR By CD tables

#Infant Mortality by CD - combined Congresses
infMort_byCD_2 <- group_by(death_cd, CONGRESS2, CD) %>%
  summarise(InfantMortality = sum(INFMORT_CD, na.rm = TRUE))

#Live Births by CD - combined Congresses
liveBirths_byCD_2 <- group_by(birth_cd, CONGRESS2, CD) %>%
  summarise(LiveBirths = sum(ALLOCATION))

#IMR by CD - combined Congresses
IMR_byCD_2 <- inner_join(infMort_byCD_2, liveBirths_byCD_2) %>%
  mutate(IMR = InfantMortality/LiveBirths * 1000)

#IMR by CD - combined Congresses - Wide for Appendix Table 6
IMR_byCD_wide <- IMR_byCD_2 %>%
  pivot_wider(values_from = IMR, names_from = CONGRESS2, values_fill = 0) %>%
  select(-c(InfantMortality, LiveBirths)) %>%
  group_by(CD) %>%
  summarise(across(where(is.numeric), sum))

#Write to CSV - for Appendix Table 6
write.csv(IMR_byCD_wide, "./Final Results/IMR_byCD_wide.csv", row.names = F)

#Break out by CONGRESS2 to create data for Figure 1
#Data for IMR Map for GIS - 111/112
IMR_111_112 <- IMR_byCD_2 %>%
  ungroup() %>%
  mutate(jenks=cut(IMR, breaks=classIntervals(IMR,n=5,style="jenks")$brks,include.lowest=T)) %>%
  filter(as.integer(CONGRESS2)==1) %>%
  select(CD, IMR, jenks)

#Data for IMR Map for GIS - 113/114
IMR_113_114 <- IMR_byCD_2 %>%
  ungroup() %>%
  mutate(jenks=cut(IMR, breaks=classIntervals(IMR,n=5,style="jenks")$brks,include.lowest=T)) %>%
  filter(as.integer(CONGRESS2)==2) %>%
  select(CD, IMR, jenks)

#Save as CSVs to create IMR maps in GIS
write.csv(IMR_111_112, "./Final Results/IMR_111_112.csv", row.names = F)
write.csv(IMR_113_114, "./Final Results/IMR_113_114.csv", row.names = F)

################################################################################
########################### IMR by Race-Ethnicity ##############################
################################################################################

#IMR by CD and Race-Ethnicity tables

#Infant Mortality by CD & race-ethnicity - combined Congresses
infMort_byCD_byRace_2 <- group_by(death_cd, CONGRESS2, CD, RACEHISP) %>%
  summarise(InfantMortality = sum(INFMORT_CD, na.rm = TRUE)) %>%
  filter(as.integer(RACEHISP)!=4) #Remove American Indian Alaska Native

#Live Births by CD & race-ethnicity - combined Congresses
liveBirths_byCD_byRace_2 <- group_by(birth_cd, CONGRESS2, CD, RACEHISP) %>%
  summarise(LiveBirths = sum(ALLOCATION)) %>%
  filter(as.integer(RACEHISP)!=4) #Remove American Indian Alaska Native

#IMR by CD & race-ethnicity - combined Congresses
IMR_byCD_byRace_2 <- inner_join(infMort_byCD_byRace_2, liveBirths_byCD_byRace_2) %>%
  mutate(IMR = InfantMortality/LiveBirths * 1000, paired = as.integer(CD)) %>%
  select(-c(InfantMortality, LiveBirths))

#Save as R Object for Shiny app
save(IMR_byCD_byRace_2, file = "./ShinyApp/IMR_byCD_byRace_2.Rdata")

#IMR by CD & race-ethnicity - combined Congresses - with details
IMR_byCD_byRace_2_details <- inner_join(infMort_byCD_byRace_2, liveBirths_byCD_byRace_2) %>%
  filter(RACEHISP!="Unknown")%>%
  mutate(IMR = InfantMortality/LiveBirths * 1000)

#IMR by CD - race-specific (for disparities analysis)

#Infant Mortality by CD - Non-Hispanic White - combined Congresses
infMort_byCD_byRace_nhw_2 <- group_by(filter(death_cd, as.integer(RACEHISP) ==2), CONGRESS2, CD, RACEHISP) %>%
  summarise(InfantMortality = sum(INFMORT_CD, na.rm = TRUE))

#Live Births by CD - Non-Hispanic White - combined Congresses
liveBirths_byCD_byRace_nhw_2 <- group_by(filter(birth_cd, as.integer(RACEHISP)==2), CONGRESS2, CD, RACEHISP) %>%
  summarise(LiveBirths = sum(ALLOCATION))

#IMR by CD - Non-Hispanic White - combined Congresses
IMR_byCD_byRace_nhw_2 <- inner_join(infMort_byCD_byRace_nhw_2, liveBirths_byCD_byRace_nhw_2) %>%
  mutate(IMR_nhw = InfantMortality/LiveBirths * 1000)

#Absolute disparity by CD and race-ethnicity - combined Congresses
IMR_byCD_byRace_absDisparity_2 <- full_join(IMR_byCD_byRace_2_details, 
                                          IMR_byCD_byRace_nhw_2, 
                                          by = c("CONGRESS2", "CD")) %>%
  rename(RACEHISP = RACEHISP.x, LiveBirths = LiveBirths.x, LiveBirths_nhw = LiveBirths.y) %>%
  filter(as.integer(RACEHISP) < 6) %>%
  mutate(IMR_absDisparity = 
           case_when(
             IMR == 0 ~ NA_real_,
             IMR != 0 ~ (IMR-IMR_nhw)), 
         paired = as.integer(CD), IMR_raw = IMR/1000, IMR_nhw_raw = IMR_nhw/1000,
         se = sqrt(IMR_raw*(1-IMR_raw)/LiveBirths + IMR_nhw_raw*(1-IMR_nhw_raw)/LiveBirths_nhw)*1000, lci = str_trim(format(round(IMR_absDisparity-1.96*se,2),nsmall=2),side="both"),
         uci = str_trim(format(round(IMR_absDisparity+1.96*se,2),nsmall=2),side="both")) %>%
  select(c(CONGRESS2, CD, RACEHISP, IMR_absDisparity, paired, se, lci, uci)) %>%
  rename(IMR = IMR_absDisparity)

#Save as R Object for IMR by race-ethnicity absolute disparity chart on Shiny app
save(IMR_byCD_byRace_absDisparity_2, file = "./ShinyApp/IMR_byCD_byRace_absDisparity_2.Rdata")

#Relative disparities by CD and race-ethnicity - combined Congresses - Appendix Table 6
IMR_byCD_byRace_relDisparity_2 <- full_join(IMR_byCD_byRace_2_details, 
                                            IMR_byCD_byRace_nhw_2, 
                                            by = c("CONGRESS2", "CD")) %>%
  rename(RACEHISP = RACEHISP.x, LiveBirths = LiveBirths.x, LiveBirths_nhw = LiveBirths.y, 
         InfantMortality = InfantMortality.x, InfantMortality_nhw = InfantMortality.y) %>%
  filter(as.integer(RACEHISP) != 7) %>%
  mutate(IMR_relDisparity = 
           case_when(
             IMR == 0 ~ NA_real_,
             IMR != 0 ~ (IMR/IMR_nhw)), 
         paired = as.integer(CD),
         se = sqrt((LiveBirths-InfantMortality)/(InfantMortality*LiveBirths)+(LiveBirths_nhw-InfantMortality_nhw)/(LiveBirths_nhw*InfantMortality_nhw)), 
         lci = str_trim(format(round(exp(log(IMR_relDisparity)-1.96*se),2),nsmall=2),side="both"), uci = str_trim(format(round(exp(log(IMR_relDisparity)+1.96*se),2),nsmall=2),side="both")) %>%
  select(c(CONGRESS2, CD, RACEHISP, IMR_relDisparity, paired, se, lci, uci)) %>%
  rename(IMR = IMR_relDisparity)

#IMR disparity tables for Appendix

#Absolute disparity table - Appendix Table 8
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
  mutate(output = ifelse(grepl("NA", output), NA , paste0(output, ")")))%>%
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
      mutate(output = ifelse(grepl("NA", output), NA , paste0(output, ")")))%>%
      pivot_wider(names_from = RACEHISP_CONGRESS, values_from = output, values_fill = NA) %>%
      ungroup() %>%
      select(-c(CONGRESS2, RACEHISP, paired, se)) %>%
      add_row(CD = "CD 19") %>%
      group_by(CD) %>%
      summarise_all(list(~.[which.min(is.na(.))]))
  }) %>%
  select(-c(CD...5)) %>%
  rename(CD = CD...1)

#Appendix Table 8
#Absolute disparity table - write to csv
write.csv(IMR_byCD_byRace_absDisparity_2_table, "./Final Results/IMR_byCD_byRace_absDisparity_2_table.csv", row.names = FALSE)

#Relative disparity table - Appendix Table 9
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

#Appendix Table 9
#Relative disparity table - write to csv
write.csv(IMR_byCD_byRace_relDisparity_2_table, "./Final Results/IMR_byCD_byRace_relDisparity_2_table.csv", row.names = FALSE)

################################################################################
########################## Total DOD Calculations ##############################
################################################################################

#Total DOD by CD tables 

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

DoD_byCD_wide <- DOD_byCD_2 %>%
  pivot_wider(values_from = MR, names_from = CONGRESS2, values_fill = 0) %>%
  select(-c(DOD, Population)) %>%
  group_by(CD) %>%
  summarise(across(where(is.numeric), sum))

#Write to CSV
write.csv(DoD_byCD_wide, "./Final Results/DoD_byCD_wide.csv", row.names = F)

#Data for DoD Map for GIS - 111/112
DOD_111_112 <- DOD_byCD_2 %>%
  ungroup() %>%
  mutate(jenks=cut(MR, breaks=classIntervals(MR,n=5,style="jenks")$brks,include.lowest=T)) %>%
  filter(as.integer(CONGRESS2)==1) %>%
  select(CD, MR, jenks)

#Data for DoD Map for GIS - 113/114
DOD_113_114 <- DOD_byCD_2 %>%
  ungroup() %>%
  mutate(jenks=cut(MR, breaks=classIntervals(MR,n=5,style="jenks")$brks,include.lowest=T)) %>%
  filter(as.integer(CONGRESS2)==2) %>%
  select(CD, MR, jenks)

#Save as CSVs to create DoD maps in GIS
write.csv(DOD_111_112, "./Final Results/DOD_111_112.csv", row.names = F)
write.csv(DOD_113_114, "./Final Results/DOD_113_114.csv", row.names = F)

################################################################################
############################# DOD by Race and Age ##############################
################################################################################
#DOD by CD, race, and age tables

#DoD by CD, Race, and Age
DOD_byRaceCD_deaths_2 <- group_by(death_cd, CD, CONGRESS2, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE)) %>%
  filter(as.integer(RACE)!=3) #Remove american indian alaksa native

#DoD MR by CD, Race, and Age - combined Congresses 
DOD_byRaceCD_2 <- left_join(DOD_byRaceCD_deaths_2, population_byRaceAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population), as.integer(RACE)!=3) %>%
  select(-c(DOD, Population))

#Save as R Object for DOD by Race plots on Shiny app
save(DOD_byRaceCD_2, file = "./ShinyApp/DOD_byRaceCD_2.Rdata")

#DoD MR by CD, Race, and Age - combined Congresses - with details
DOD_byRaceCD_2_details <- inner_join(DOD_byRaceCD_deaths_2, population_byRaceAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, AGE_CAT_EDUC, RACE) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD))

#DoD by CD, Race, and Age - combined Congresses - White/Non-White 
DOD_byRace2CD_deaths_2 <- group_by(death_cd, CD, CONGRESS2, RACE2, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#DoD MR by CD, Race, and Age - combined Congresses - White/Non-White 
DOD_byRace2CD_2 <- left_join(DOD_byRace2CD_deaths_2, population_byRace2AgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, RACE2, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population)) %>%
  select(-c(DOD, Population))

#Save as R Object for DOD by Race (Rolled-up) plots on Shiny app
save(DOD_byRace2CD_2, file = "./ShinyApp/DOD_byRace2CD_2.Rdata")

#DoD by CD, race, and age - race-specific (for disparities analysis)

#DoD by CD, Race, and Age - White - combined Congresses
DOD_byRaceCD_deaths_white_2 <- group_by(filter(death_cd, as.integer(RACE) == 1), CD, CONGRESS2, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#DoD MR by CD, Race, and Age - White - combined Congresses
DOD_byRaceCD_white_2 <- inner_join(DOD_byRaceCD_deaths_white_2, population_byRaceAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, RACE, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR_white = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

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

#Save as R Object for DoD by Race absolute disparity plots on Shiny app
save(DOD_byRaceCD_absDisparity_2, file = "./ShinyApp/DOD_byRaceCD_absDisparity_2.Rdata")

################################################################################
########################### DOD by Education and Age ###########################
################################################################################

#DoD by CD, Education, and Age - combined Congresses
DOD_byEducCD_deaths_2 <- group_by(death_cd, CD, CONGRESS2, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#DoD MR by CD, Education, and Age - combined Congresses
DOD_byEducCD_2 <- left_join(DOD_byEducCD_deaths_2, population_byEducAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000), paired = as.integer(CD)) %>%
  filter(!is.na(Population)) %>%
  select(-c(DOD, Population))

#Save as R Object for DOD by education for Shiny app
save(DOD_byEducCD_2, file = "./ShinyApp/DOD_byEducCD_2.Rdata")

#DoD MR by CD, Education, and Age - combined Congresses - with details
DOD_byEducCD_2_details <- inner_join(DOD_byEducCD_deaths_2, population_byEducAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, AGE_CAT_EDUC, EDUC) %>%
  summarise(DOD = sum(DOD), Population = sum(Population)) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000))

#DoD by CD, Education, and Age - education-specific (for disparities analysis)

#DoD by CD, Education, and Age - college - combined Congresses
DOD_byEducCD_deaths_college_2 <- group_by(filter(death_cd, as.integer(EDUC) == 4), CD, CONGRESS2, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE))

#DoD MR by CD, Education, and Age - college - combined Congresses
DOD_byEducCD_college_2 <- inner_join(DOD_byEducCD_deaths_college_2, population_byEducAgeCD_byCongress2) %>%
  group_by(CD, CONGRESS2, EDUC, AGE_CAT_EDUC) %>%
  summarise(DOD = sum(DOD, na.rm = TRUE), Population = sum(Population)) %>%
  mutate(MR_college = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population))

#DoD absolute disparities by CD, Education, and Age - combined Congresses
DOD_byEducCD_absDisparity_2 <- full_join(DOD_byEducCD_2_details, 
                                         DOD_byEducCD_college_2,
                                         c("CD", "CONGRESS2", "AGE_CAT_EDUC")) %>%
  rename(EDUC = EDUC.x, DOD = DOD.x, DOD_college = DOD.y, Population = Population.x, Population_college = Population.y) %>%
  filter(as.integer(EDUC) < 5) %>%
  mutate(MR_absDisparity = 
           case_when(
             MR == 0 ~ NA_real_,
             MR != 0 ~ (MR-MR_college)), 
         MR_raw = MR/1000, MR_college_raw = MR_college/1000,
         se = sqrt(MR_raw*(1-MR_raw)/Population + MR_college_raw*(1-MR_college_raw)/Population_college)*1000, lci = str_trim(format(round(MR_absDisparity-1.96*se,2),nsmall=2),side="both"),
         uci = str_trim(format(round(MR_absDisparity+1.96*se,2),nsmall=2),side="both"), paired = as.integer(CD)) %>%
  select(c(CONGRESS2, CD, AGE_CAT_EDUC, EDUC, MR_absDisparity, paired, se, lci, uci)) %>%
  rename(MR = MR_absDisparity)

#Save as R Object for DoD Absolute Disparities charts on Shiny app
save(DOD_byEducCD_absDisparity_2, file = "./ShinyApp/DOD_byEducCD_absDisparity_2.Rdata")

#DoD relative disparities by CD, Education, and Age - combined Congresses
DOD_byEducCD_relDisparity_2 <- full_join(DOD_byEducCD_2_details, 
                                            DOD_byEducCD_college_2,
                                            c("CD", "CONGRESS2", "AGE_CAT_EDUC")) %>%
  rename(EDUC = EDUC.x, Population = Population.x, Population_college = Population.y, 
         DOD = DOD.x, DOD_college = DOD.y) %>%
  filter(as.integer(EDUC)!=4) %>%
  mutate(MR_relDisparity = 
           case_when(
             MR == 0 ~ NA_real_,
             MR != 0 ~ (MR/MR_college)), 
         paired = as.integer(CD),
         se = sqrt((Population-DOD)/(DOD*Population)+(Population_college-DOD_college)/(Population_college*DOD_college)), 
         lci = str_trim(format(round(exp(log(MR_relDisparity)-1.96*se),2),nsmall=2),side="both"), 
         uci = str_trim(format(round(exp(log(MR_relDisparity)+1.96*se),2),nsmall=2),side="both")) %>%
  select(c(CONGRESS2, CD, EDUC, AGE_CAT_EDUC, MR_relDisparity, paired, se, lci, uci)) %>%
  rename(MR = MR_relDisparity)

#DoD by CD, education and age - Appendix tables

#DoD absolute disparities by CD, Education, and Age table - for Appendix Table 10
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
  mutate(output = ifelse(grepl("NA", output), NA , paste0(output, ")")))%>%
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
      mutate(output = ifelse(grepl("NA", output), NA , paste0(output, ")")))%>%
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

#Appendix Table 10
#DoD absolute disparities by CD, Education, and Age table - write to csv
write.csv(DOD_byEducCD_absDisparity_2_table, "./Final Results/DOD_byEducCD_absDisparity_2_table.csv", row.names = FALSE)

#DoD relative disparities by CD, Education, and Age table - For Appendix Table 11
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

#Appendix Table 11
#DoD relative disparities by CD, Education, and Age table - write to csv
write.csv(DOD_byEducCD_relDisparity_2_table, "./Final Results/DOD_byEducCD_relDisparity_2_table.csv", row.names = FALSE)

################################################################################
############################ IMR and DOD Map Data ##############################
################################################################################
#Data frames for Maps (Figures 1 & 2 and dashboard maps) - Data + dashboard map code displayed here, but final article maps created outside of R

#Data for IMR Maps by CD - combined Congresses
IMR_Maps_byCD_2 <- inner_join(cd_outlines_2, IMR_byCD_2) %>%
  group_by(CD, CONGRESS2) %>%
  summarise(IMR = sum(InfantMortality)/sum(LiveBirths)*1000, geometry)%>%
  ungroup() %>%
  mutate(jenks=cut(IMR, breaks=classIntervals(IMR,n=5,style="jenks")$brks,include.lowest=T))

#Save as R Object for IMR Maps on Shiny app
save(IMR_Maps_byCD_2, file = "./ShinyApp/IMR_Maps_byCD_2.Rdata")

#Data for DoD Maps by CD - combined Congresses
DOD_Maps_byCD_2 <- inner_join(cd_outlines_2, DOD_byCD_2) %>%
  group_by(CD, CONGRESS2) %>%
  summarise(CD, MR)%>%
  ungroup() %>%
  mutate(jenks=cut(MR, breaks=classIntervals(MR,n=5,style="jenks")$brks,include.lowest=T))

#Save as R Object for DoD map on Shiny app
save(DOD_Maps_byCD_2, file = "./ShinyApp/DOD_Maps_byCD_2.Rdata")

################################################################################
############################### IMR and DOD Maps ###############################
################################################################################

#R versions of IMR and DoD Maps

#Figure 1 (re-done in GIS for paper)
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

#Figure 2 (re-done in GIS for paper)
#DoD Maps - Combined congresses
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
############################### IMR and DOD Plots ##############################
################################################################################

#IMR and DoD Plots (Figure 3 and Figure 4)

#Figure 3
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

#Figure 4
#DoD by Education for each age group and CD
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

#Data frame for IMR DOD Scatterplot - Appendix Figure 2
IMR_DOD <- inner_join(IMR_byCD_2, DOD_byCD_2) %>%
  select(c(CONGRESS2, CD, IMR, MR)) %>%
  rename(DODMR = MR)

#Appendix Figure 2
#Scatterplot of DOD x IMR
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
################################# Sankey Diagram ###############################
################################################################################

#Data frame for sankey showing relationship between census tracts from 111/112 to 113/114

#Appendix Figure 1 (and dashboard redistricting figure)
#Data frame for sankey diagram
sankey_111_113 <- inner_join(tract_to_cd111, tract_to_cd113) %>%
  group_by(cd111,cd113) %>%
  select(cd111,cd113) %>%
  mutate(cd111 = paste0('cd111-',sprintf('%02d',cd111)), cd113 = paste0('cd113-',sprintf('%02d',cd113))) %>%
  data_to_sankey() %>%
  arrange(to, from)

template<-expand.grid(from=unique(sankey_111_113$from), to=unique(sankey_111_113$to))

sankey_111_113_full <- full_join(sankey_111_113, template) %>%
  arrange(from, to) %>%
  mutate_at(3, ~replace_na(.,0))

#Save as R Object for Shiny app redistricting figure
save(sankey_111_113_full, file = "./ShinyApp/sankey_111_113_full.Rdata")

#Appendix Figure 1 - sankey diagram displaying districts in 111/112 and 113/114
sankey <- ggplot(sankey_111_113_full, aes(y = weight, axis1 = from, axis2 = to)) +
  geom_alluvium(aes(fill = from)) + 
  geom_stratum() + 
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_void() +
  theme(axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), legend.position = "none")
sankey
