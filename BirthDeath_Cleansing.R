rm(list=ls())
library(tidyverse)
library(foreign)
library(tidycensus)
library(data.table)
library(stringi)
library(stringr)
library(bit64)
library("writexl")
library(tidyselect)
select <- dplyr::select


################################################################################

#Create variable to house the directory path to create ease during import process
dir_name <- "//files.drexel.edu\\encrypted\\SOPH\\UHC\\SchnakeMahl_PAVitalStat\\Data\\"

#Set working directory for the data
setwd("C:/Users/ga437/OneDrive - Drexel University/Documents/Congressional-Districts-2/")


################################################################################

#Urban/Rural Analysis File
# urbanRural_original <- fread("Data/urbanRural2010_PA.csv")
# 
# urbanRural <- urbanRural_original %>% rename(
#                                       GEOID10 = "State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)",
#                                       primary_RUCA = "Primary RUCA Code 2010",
#                                       pop_tract = "Tract Population, 2010",
#                                       area = "Land Area (square miles), 2010",
#                                       density = "Population Density (per square mile), 2010") %>%
#   select(-c("State-County FIPS Code", "Select State", "Select County", "Secondary RUCA Code, 2010 (see errata)")) %>%
#   mutate(
#     primary_RUCA = factor(primary_RUCA),
#     urban_rural = case_when(
#       #urban
#       primary_RUCA == 1 ~ 1,
#       #suburban
#       primary_RUCA %in%c(2:6) ~ 2,
#       #rural
#       primary_RUCA %in%c(7:10) ~ 3,
#       #missing
#       TRUE ~ 99),
#     urban_rural = factor(urban_rural, labels=c("Urban", "Suburban", "Rural", "Missing")),
#     urban = case_when(
#       as.numeric(urban_rural) == 1 ~ 1,
#       TRUE ~ 0),
#     suburban = case_when(
#       as.numeric(urban_rural) == 2 ~ 1,
#       TRUE ~ 0),
#     rural = case_when(
#       as.numeric(urban_rural) == 3 ~ 1,
#       TRUE ~0))
#   
# urbanRural_counties <- urbanRural %>%
#   mutate(GEOID = substr(GEOID10,1,5))
# 
# #Pull data to csv 
# write_csv(urbanRural_counties,"C:\\Users\\ga437\\OneDrive - Drexel University\\Congressional_Districts_BRFSS\\Data_ByCounty_Anfuso\\urbanRural_counties.csv")


  ################################################################################

#Columns we need for birth files
keep_birth <- c("FILENO", "SEX", "CHILDDOB", "MOTHEDU", "MOTHHISP0315", 
                "MOTHHISP8902", "MOTHRACE0315", "mothrace7902", "FATHEDU",
                "FATHHISP0315", "FATHHISP8902", "FATHRACE0315", "fathrace7902", 
                "BWEIGHT", "LOP", "GEOID10", "RESCNTY", "uniqueID", "year")

#Import birth data
birth2010 <- fread(paste0(dir_name, "birthgeo2010.csv")) %>%
  mutate(RegNum = as.character(RegNum), reszipcln = as.character(reszipcln), 
         momreszip = as.character(momreszip), year = "2010") %>%
  select(one_of(keep_birth))


birth2011 <- fread(paste0(dir_name, "birthgeo2011.csv")) %>%
  mutate(RegNum = as.character(RegNum), reszipcln = as.character(reszipcln), 
         momreszip = as.character(momreszip), year = "2011") %>%
  select(one_of(keep_birth))

birth2012 <- fread(paste0(dir_name, "birthgeo2012.csv")) %>%
  mutate(RegNum = as.character(RegNum), reszipcln = as.character(reszipcln), 
         momreszip = as.character(momreszip), year = "2012") %>%
  select(one_of(keep_birth))

birth2013 <- fread(paste0(dir_name, "birthgeo2013.csv")) %>%
  mutate(RegNum = as.character(RegNum), reszipcln = as.character(reszipcln), 
         momreszip = as.character(momreszip), year = "2013") %>%
  select(one_of(keep_birth))

birth2014 <- fread(paste0(dir_name, "birthgeo2014.csv")) %>%
  mutate(RegNum = as.character(RegNum), reszipcln = as.character(reszipcln), 
         momreszip = as.character(momreszip), year = "2014") %>%
  select(one_of(keep_birth))

birth2015 <- fread(paste0(dir_name, "birthgeo2015.csv")) %>%
  mutate(RegNum = as.character(RegNum), reszipcln = as.character(reszipcln), 
         momreszip = as.character(momreszip), year = "2015") %>%
  select(one_of(keep_birth))

birth_original <- bind_rows(birth2010, birth2012, birth2012, 
                            birth2013, birth2014, birth2015)

#Columns we need for death files
keep_death <- c("FILENO", "SEX", "AGE6011", "AGE1215", "AGE", "DOB", "PLCDTH", "HISPANIC", 
                "RACE9011", "race1215", "EDUC8711", "EDUC1215", "EDUC", 
                "MARRIED", "MANDEATH", "CODICD10", "INFMORT", 
                "CTYDEATH","GEOID10", "uniqueID", "year")

#Import death data
death2010 <- fread(paste0(dir_name, "deathgeo2010.csv")) %>%
  mutate(EDUC = strtoi(EDUC), year = "2010") %>%
  rename(RACE9011 = race9011) %>%
  select(one_of(keep_death))

death2011 <- fread(paste0(dir_name, "deathgeo2011.csv")) %>%
  mutate(EDUC = strtoi(EDUC), year = "2011") %>%
  rename(RACE9011 = race9011) %>%
  select(one_of(keep_death))

death2012 <- fread(paste0(dir_name, "deathgeo2012.csv")) %>%
  rename(GEOID10 = geoid10) %>%
  mutate(EDUC = strtoi(EDUC), year = "2012") %>%
  select(one_of(keep_death))

death2013 <- fread(paste0(dir_name, "deathgeo2013.csv")) %>%
  rename(GEOID10 = geoid10) %>%
  mutate(EDUC = strtoi(EDUC), year = "2013") %>%
  select(one_of(keep_death))

death2014 <- fread(paste0(dir_name, "deathgeo2014.csv")) %>%
  rename(GEOID10 = geoid10) %>%
  mutate(EDUC = strtoi(EDUC), year = "2014") %>%
  select(one_of(keep_death))

death2015 <- fread(paste0(dir_name, "deathgeo2015.csv")) %>%
  rename(GEOID10 = geoid10) %>%
  mutate(EDUC = strtoi(EDUC), year = "2015") %>%
  select(one_of(keep_death))

death_original <- bind_rows(death2010, death2012, death2012, 
                            death2013, death2014, death2015)

################################################################################

#Crosswalk with Congressional District 111 (2009-2010)
tract_to_cd111 <- fread("Data/crosswalk_cd111.csv") %>%
  subset(select = -c(cntyname, pop10)) %>%
  mutate(
      tract = str_remove(tract, "^0+"),
      tract = as.numeric(tract),
      tract = tract * 100,
      tract = str_pad(tract, 6, "left", "0"),
      GEOID10 = as.numeric(paste0(county, tract)),
      GEOID10 = str_remove(GEOID10, "^0+"),
      GEOID10 = as.integer64(GEOID10)
    )

#Merge cd111 crosswalk with 2010 birth and death files
birth2010_cd <- inner_join(birth2010, tract_to_cd111) %>%
  rename(CD = cd111, ALLOCATION = afact111)

death2010_cd <- inner_join(death2010, tract_to_cd111) %>%
  rename(CD = cd111, ALLOCATION = afact111)

#Merge cd111 crosswalk with 2012 birth and death files
birth2011_cd <- inner_join(birth2011, tract_to_cd111) %>%
  rename(CD = cd111, ALLOCATION = afact111)

death2011_cd <- inner_join(death2011, tract_to_cd111) %>%
  rename(CD = cd111, ALLOCATION = afact111)

#Merge cd111 crosswalk with 2012 birth and death files
birth2012_cd <- inner_join(birth2012, tract_to_cd111) %>%
  rename(CD = cd111, ALLOCATION = afact111)

death2012_cd <- inner_join(death2012, tract_to_cd111) %>%
  rename(CD = cd111, ALLOCATION = afact111)

#Crosswalk with Congressional District 113 (2013-2014)
tract_to_cd113 <- fread("Data/crosswalk_cd113.csv") %>%
  subset(select = -c(cntyname, pop10)) %>%
  mutate(
    tract = str_remove(tract, "^0+"),
    tract = as.numeric(tract),
    tract = tract * 100,
    tract = str_pad(tract, 6, "left", "0"),
    GEOID10 = as.numeric(paste0(county, tract)),
    GEOID10 = str_remove(GEOID10, "^0+"),
    GEOID10 = as.integer64(GEOID10)
  )

#Merge cd113 crosswalk with 2013 birth and death files
birth2013_cd <- inner_join(birth2013, tract_to_cd113) %>%
  rename(CD = cd113, ALLOCATION = afact113)

death2013_cd <- inner_join(death2013, tract_to_cd113) %>%
  rename(CD = cd113, ALLOCATION = afact113)

#Merge cd113 crosswalk with 2014 birth and death files
birth2014_cd <- inner_join(birth2014, tract_to_cd113) %>%
  rename(CD = cd113, ALLOCATION = afact113)

death2014_cd <- inner_join(death2014, tract_to_cd113) %>%
  rename(CD = cd113, ALLOCATION = afact113)

#Crosswalk with Congressional District 114 (2015-2016)
tract_to_cd114 <- fread("Data/crosswalk_cd114.csv") %>%
  subset(select = -c(cntyname, pop10)) %>%
  mutate(
    tract = str_remove(tract, "^0+"),
    tract = as.numeric(tract),
    tract = tract * 100,
    tract = str_pad(tract, 6, "left", "0"),
    GEOID10 = as.numeric(paste0(county, tract)),
    GEOID10 = str_remove(GEOID10, "^0+"),
    GEOID10 = as.integer64(GEOID10)
  )

#Merge cd114 crosswalk with 2015 birth and death files
birth2015_cd <- inner_join(birth2015, tract_to_cd114) %>%
  rename(CD = cd114, ALLOCATION = afact114)

death2015_cd <- inner_join(death2015, tract_to_cd114) %>%
  rename(CD = cd114, ALLOCATION = afact114)

#Crosswalk with Congressional District 114 (2015-2016)
tract_to_cd116 <- fread("Data/crosswalk_cd116.csv") %>%
  subset(select = -c(cntyname, pop10)) %>%
  mutate(
    tract = str_remove(tract, "^0+"),
    tract = as.numeric(tract),
    tract = tract * 100,
    tract = str_pad(tract, 6, "left", "0"),
    GEOID10 = as.numeric(paste0(county, tract)),
    GEOID10 = str_remove(GEOID10, "^0+"),
    GEOID10 = as.integer64(GEOID10)
  )

################################################################################

#Re-categorize birth data

birth_mutations <- function(df) {
  x <- df %>%
    mutate(
      LOWBWEIGHT = case_when(
        BWEIGHT < 2500 ~ 1,
        BWEIGHT >= 2500 ~ 0),
      PRETERM = case_when(
        LOP < 37 ~ 1,
        LOP >= 37 ~ 0),
        #Test to make sure this works (create table of child hip mother hisp and see how they align)
      MOMHISP = case_when(
        #(MOTHHISP8902 == 0) | only works if we have pre-2010 data
          (MOTHHISP0315 == 1) ~ 1,
        #((MOTHHISP8902 >= 1) & (MOTHHISP8902 <= 5) | 
           (MOTHHISP0315 >= 2) & (MOTHHISP0315 <= 5) ~ 2,
        #(MOTHHISP8902 == 9) | 
          (MOTHHISP0315 == 9) ~ 3),
      DADHISP = case_when(
        #(FATHHISP8902 == 0) | 
          (FATHHISP0315 == 1) ~ 1,
        # ((FATHHISP8902 >= 1) & (FATHHISP8902 <= 5) | 
           (FATHHISP0315 >= 2) & (FATHHISP0315 <= 5) ~ 2,
        # (FATHHISP8902 == 9) | 
          (FATHHISP0315 == 9) ~ 3),
      HISP = case_when(
        !is.na(MOMHISP) ~ MOMHISP,
        is.na(MOMHISP) ~ DADHISP) %>%
          factor(labels = c("Hispanic", "Non-Hispanic", "Unknown")),
      MOMRACE = case_when(
        # (mothrace7902 == 1) | 
          (MOTHRACE0315 == 1)  ~ 1,
        # (mothrace7902 == 2) | 
          (MOTHRACE0315 == 2)  ~ 2,
        # (mothrace7902 == 3) | 
          (MOTHRACE0315 == 3)  ~ 3, 
        # (mothrace7902 >=4 & mothrace7902<= 6) | (mothrace7902 == 8)
          (MOTHRACE0315 >= 4 & MOTHRACE0315 <= 14) ~ 4,
        # (mothrace7902 == 7 | mothrace7902 == 0 | is.na(mothrace7902))
          (MOTHRACE0315 >= 15 | is.na(MOTHRACE0315)) ~ 5),
      DADRACE = case_when(
        # (fathrace7902 == 1)  |
          (FATHRACE0315 == 1) ~ 1,
        # (fathrace7902 == 2) | 
          (FATHRACE0315 == 2) ~ 2,
        # (fathrace7902 == 3) | 
          (FATHRACE0315 == 3) ~ 3, 
        # (fathrace7902 >=4 & fathrace7902<= 6) | (fathrace7902 == 8) |
          (FATHRACE0315 >= 4 & FATHRACE0315 <= 14) ~ 4,
        # (fathrace7902 == 7 | fathrace7902 == 0 | is.na(fathrace7902)) | 
          (FATHRACE0315 >= 15 | is.na(FATHRACE0315)) ~ 5),
      RACE = case_when(
        !is.na(MOMRACE) ~ MOMRACE,
        is.na(MOMRACE) ~ DADRACE) %>%
          factor(
            levels = c(1,2,3,4,5),
            labels = c("White", "Black", "American Indian, Alaska Native", 
                       "Asian American Pacific Islander", "Other")),
      RACE2 = case_when(
        as.integer(RACE) == 1 ~ 1,
        as.integer(RACE) > 1 ~ 2) %>%
        factor(
          levels = c(1,2),
          labels = c("White", "Non-White")),
      RACEHISP = case_when(
        as.integer(HISP) == 2 ~ 1,
        as.integer(HISP) == 1 & as.integer(RACE) == 1 ~ 2,
        as.integer(HISP) == 1 & as.integer(RACE) == 2 ~ 3,
        as.integer(HISP) == 1 & as.integer(RACE) == 3 ~ 4,
        as.integer(HISP) == 1 & as.integer(RACE) == 4 ~ 5,
        as.integer(HISP) == 1 & as.integer(RACE) == 5 ~ 6,
        as.integer(HISP) == 3 ~ 7) %>%
          factor(
            levels = c(1,2,3,4,5,6,7),
            labels = c("Hispanic", "Non-Hispanic White", "Non-Hispanic Black", 
                        "Non-Hispanic American Indian, Alaska Native",
                        "Non-Hispanic Asian American Pacific Islander", "Non-Hispanic Other", "Unknown")),
      RACEHISP2 = case_when(
        (as.integer(HISP) == 2) ~ 1,
        (as.integer(HISP) == 1) & (as.integer(RACE2) == 1) ~ 2,
        (as.integer(HISP) == 1) & (as.integer(RACE2) == 2) ~ 3,
        (as.integer(HISP) == 3) ~ 4) %>%
        factor(
          levels = c(1,2,3,4),
          labels = c("Hispanic", "Non-Hispanic White", "Non-Hispanic Non-White", "Unknown")),
      CD = factor(CD, 
                  levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),
                  labels = c("CD 01", "CD 02", "CD 03", "CD 04", "CD 05",
                                 "CD 06", "CD 07", "CD 08", "CD 09", "CD 10",
                                 "CD 11", "CD 12", "CD 13", "CD 14", "CD 15",
                                 "CD 16", "CD 17", "CD 18", "CD 19")),
      CONGRESS = case_when(
        year >= 2009 & year <= 2010 ~ 1,
        year >= 2011 & year <= 2012 ~ 2,
        year >= 2013 & year <= 2014 ~ 3,
        year == 2015 ~ 4) %>%
        factor(
          levels = c(1,2,3,4),
          labels = c("111 (2009 - 2010)", "112 (2011 - 2012)", 
                     "113 (2013 - 2014)", "114 (2015 - 2016)")),
      CONGRESS2 = case_when(
        year >= 2010 & year <= 2012 ~ 1,
        year >= 2013 & year <= 2015 ~ 2) %>%
        factor(
          levels = c(1,2),
          labels = c("111 - 112 (2010 - 2012)",
                     "113 - 114 (2013 - 2015)")))
  return(x)
}

birth_cd <- bind_rows(birth2010_cd, birth2011_cd, birth2012_cd,
                      birth2013_cd, birth2014_cd, birth2015_cd) %>%
  birth_mutations()

birth_cd116 <- bind_rows(birth2010, birth2011, birth2012, 
                         birth2013, birth2014, birth2015) %>%
  inner_join(tract_to_cd116) %>%
  rename(CD = cd116, ALLOCATION = afact116) %>% 
  birth_mutations()

#death mutations
death_mutations <- function(df) {
  x <- df %>%
    mutate(
      CD = factor(CD, 
                     levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),
                     labels = c("CD 01", "CD 02", "CD 03", "CD 04", "CD 05",
                                    "CD 06", "CD 07", "CD 08", "CD 09", "CD 10",
                                    "CD 11", "CD 12", "CD 13", "CD 14", "CD 15",
                                    "CD 16", "CD 17", "CD 18", "CD 19")),
      CONGRESS = case_when(
        year >= 2009 & year <= 2010 ~ 1,
        year >= 2011 & year <= 2012 ~ 2,
        year >= 2013 & year <= 2014 ~ 3,
        year == 2015 ~ 4) %>%
        factor(
          levels = c(1,2,3,4),
          labels = c("111 (2009 - 2010)","112 (2011 - 2012)", 
                     "113 (2013 - 2014)", "114 (2015 - 2016)")),
      CONGRESS2 = case_when(
        year >= 2010 & year <= 2012 ~ 1,
        year >= 2013 & year <= 2015 ~ 2) %>%
        factor(
          levels = c(1,2),
          labels = c("111 - 112 (2010 - 2012)",
                     "113 - 114 (2013 - 2015)")),
      RACE = case_when(
        RACE9011 == 1 | 
          race1215 == 01 ~ 1,
        RACE9011 == 2 | 
          race1215 == 02 ~ 2,
        RACE9011 == 3 | 
          race1215 == 03 ~ 3,
        ((RACE9011 >= 4 & RACE9011 <= 8) | RACE9011 == 0) | 
          (race1215 >= 04 & race1215 <= 14) ~ 4,
        RACE9011 == 9 | is.na(RACE9011) | 
          is.na(race1215) | race1215>= 15 ~ 5) %>%
            factor(
              levels = c(1,2,3,4,5),
              labels = c("White", "Black", 
              "American Indian, Alaska Native", "Asian American Pacific Islander", "Other")),
      RACE2 = case_when(
        as.integer(RACE) == 1 ~ 1,
        as.integer(RACE) > 1 ~ 2) %>%
          factor(
            levels = c(1,2),
            labels = c("White", "Non-White")),
      HISP = case_when(
        #Non-Hispanic
        HISPANIC == 1 ~ 1,
        #Hispanic
        HISPANIC >= 2 & HISPANIC <= 5 ~ 2,
        #Unknown
        HISPANIC == 9 ~ 3),
      HISP = factor(HISP, labels = c("Non-Hispanic", "Hispanic", "Unknown")),
      RACEHISP = case_when(
        (as.integer(HISP) == 2) ~ 1,
        (as.integer(HISP) == 1) & (as.integer(RACE) == 1) ~ 2,
        (as.integer(HISP) == 1) & (as.integer(RACE) == 2) ~ 3,
        (as.integer(HISP) == 1) & (as.integer(RACE) == 3) ~ 4,
        (as.integer(HISP) == 1) & (as.integer(RACE) == 4) ~ 5,
        (as.integer(HISP) == 1) & (as.integer(RACE) == 5) ~ 6,
        (as.integer(HISP) == 3) ~ 7) %>%
          factor(
            levels = c(1,2,3,4,5,6,7),
            labels = c("Hispanic", "Non-Hispanic White", "Non-Hispanic Black", 
            "Non-Hispanic American Indian, Alaska Native",
            "Non-Hispanic Asian American Pacific Islander", "Non-Hispanic Other", "Unknown")),
      RACEHISP2 = case_when(
        (as.integer(HISP) == 2) ~ 1,
        (as.integer(HISP) == 1) & (as.integer(RACE2) == 1) ~ 2,
        (as.integer(HISP) == 1) & (as.integer(RACE2) == 2) ~ 3,
        (as.integer(HISP) == 3) ~ 4) %>%
          factor(
            levels = c(1,2,3,4),
            labels = c("Hispanic", "Non-Hispanic White", "Non-Hispanic Non-White", "Unknown")),
      SEX = factor(SEX, levels = c(1,2,3), labels = c("Male", "Female", "Unknown")),
      AGE = case_when(
        year <= 2011 ~ AGE6011,
        year >= 2012 ~ AGE1215), 
      DESPAIR = case_when(
        #Suicide
        ((CODICD10 >= "X60" & CODICD10 <= "X84") | (CODICD10 == "Y87.0")) & 
          (AGE >= 25) & (AGE <= 64) ~ 1,
        #Chronic liver disease and cirrhosis 
        ((CODICD10 == "K70") | (CODICD10 == "K73") | (CODICD10 == "K74")) &
          (AGE >= 25) & (AGE <= 64) ~ 1,
        #Alcohol and drug poisonings
        ((CODICD10 >= "X40" & CODICD10 <= "X45") |
          (CODICD10 >= "Y10" & CODICD10 <= "Y15") |
          (CODICD10 == "Y45") | (CODICD10 == "Y47") | (CODICD10 == "Y45")) &
          (AGE >= 25) & (AGE <= 64) ~ 1,
        TRUE ~ 0),
       AGE_CAT = case_when(
           AGE < 5 ~ 1,
           AGE >= 5 & AGE <= 9 ~ 2,
           AGE >= 10 & AGE <= 14 ~ 3,
           AGE >= 15 & AGE <= 17 ~ 4,
           AGE >= 18 & AGE <= 19 ~ 5,
           AGE >= 20 & AGE <= 24 ~ 6,
           AGE >= 25 & AGE <= 29 ~ 7,
           AGE >= 30 & AGE <= 34 ~ 8,
           AGE >= 35 & AGE <= 44 ~ 9,
           AGE >= 45 & AGE <= 54 ~ 10,
           AGE >= 55 & AGE <= 64 ~ 11,
           AGE >= 65 & AGE <= 74 ~ 12,
           AGE >= 75 & AGE <= 84 ~ 13,
           AGE >= 85 ~ 14) %>%
        factor(levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),
               labels = c("Under 5 years", "05 to 09 years", "10 to 14 years",
                          "15 to 17 years", "18 and 19 years", "20 to 24 years",
                          "25 to 29 years", "30 to 34 years", "35 to 44 years",
                          "45 to 54 years", "55 to 64 years", "65 to 74 years",
                          "75 to 84 years", "85 years and over")),
      AGE_CAT_EDUC = case_when(
        AGE < 18 ~ 1,
        AGE >= 18 & AGE <= 24 ~ 2,
        AGE >= 25 & AGE <= 34 ~ 3,
        AGE >= 35 & AGE <= 44 ~ 4,
        AGE >= 45 & AGE <= 64 ~ 5,
        AGE >= 65 ~ 6) %>%
        factor(levels = c(1,2,3,4,5,6),
               labels = c("Under 18 years", "18 to 24 years", "25 to 34 years",
                          "35 to 44 years", "45 to 64 years", "65 years and over")),
      EDUC = case_when(
        EDUC == 0 ~ 1,
        EDUC == 1 ~ 2, 
        EDUC == 2 ~ 3,
        EDUC == 3 ~ 4,
        TRUE ~ 5) %>%
        factor(
          levels = c(1,2,3,4,5),
          labels = c("Less than High School", "High School", "Some College/Associate Degree",
                     "Bachelor/Master/Doctorate/Professional Degree", "Unknown")),
      INFMORT_CD = INFMORT*ALLOCATION,
      DESPAIR_CD = DESPAIR*ALLOCATION,
      year = as.numeric(year))
  return(x)
}

death_cd <- bind_rows(death2010_cd, death2011_cd, death2012_cd,
                      death2013_cd, death2014_cd, death2015_cd) %>%
  death_mutations()

#Repeat mutations above for cd116
death_cd116 <- bind_rows(death2010, death2011, death2012, 
                         death2013, death2014, death2015) %>%
  inner_join(tract_to_cd116) %>%
  rename(CD = cd116, ALLOCATION = afact116) %>%
  death_mutations()

################################################################################
#Year to congress crosswalk
CONGRESS <- death_cd %>%
  select(c(year, CONGRESS, CONGRESS2)) %>%
  unique() %>%
  mutate(
    year = as.numeric(year)
  )

#Congress to Congress 2 crosswalk
CONGRESS2 <- death_cd %>%
  select(c(CONGRESS, CONGRESS2)) %>%
  unique()

#Age category to age category for education crosswalk
AGE_CATS <- death_cd %>%
  select(AGE_CAT, AGE_CAT_EDUC) %>%
  unique()

#Race to Race 2 crosswalk
RACE2 <- death_cd %>%
  select(c(RACE, RACE2)) %>%
  unique()

################################################################################
var_names <- load_variables(2015, "acs5", cache = TRUE) %>%
  rename(variable = name)

pop_total <- c("B03002_001")

pop_byRace_vars <- c("B03002_012", "B03002_003", "B03002_004",
              "B03002_005", "B03002_006", "B03002_007", "B03002_008", "B03002_009")

#Pull ACS population data and re-categorize to match our categories. 
#I grouped all multiracial populations into 7 for unknown, but the numbers seem too high


popRaceSexAge_all <- c(paste0("B01001A_", sprintf("%03d", 9:13)), paste0("B01001A_", sprintf("%03d", 24:28)),#White
                       paste0("B01001B_", sprintf("%03d", 9:13)),paste0("B01001B_", sprintf("%03d", 24:28)),#Black
                       paste0("B01001C_", sprintf("%03d", 9:13)),paste0("B01001C_", sprintf("%03d", 24:28)),#AIAN
                       paste0("B01001D_", sprintf("%03d", 9:13)),paste0("B01001E_", sprintf("%03d", 9:13)),
                       paste0("B01001D_", sprintf("%03d", 24:28)),paste0("B01001E_", sprintf("%03d", 24:28)),#AAPI
                       paste0("B01001F_", sprintf("%03d", 9:13)),paste0("B01001F_", sprintf("%03d", 24:28)))#Other

#Pull ACS population data and re-categorize to match our education categories.
popEducSexAge_lessThanHSMale <- c("B15001_012", "B15001_013", "B15001_020", "B15001_021", "B15001_028", "B15001_029")
popEducSexAge_lessThanHSFemale <- c("B15001_053", "B15001_054", "B15001_061", "B15001_062", "B15001_069", "B15001_070")

popEducSexAge_HSMale <- c("B15001_014", "B15001_022", "B15001_030")
popEducSexAge_HSFemale <- c("B15001_055", "B15001_063", "B15001_071")

popEducSexAge_someCollegeMale <- c("B15001_015", "B15001_016", "B15001_023", "B15001_024", "B15001_031", "B15001_032")
popEducSexAge_someCollegeFemale <- c("B15001_056", "B15001_057", "B15001_064", "B15001_065", "B15001_072", "B15001_073")

popEducSexAge_BachPlusMale <- c("B15001_017", "B15001_018", "B15001_025", "B15001_026", "B15001_033", "B15001_034")
popEducSexAge_BachPlusFemale <- c("B15001_058", "B15001_059", "B15001_066", "B15001_067", "B15001_074", "B15001_075")

popEducSexAge_all <- c(popEducSexAge_lessThanHSMale, popEducSexAge_lessThanHSFemale,
                       popEducSexAge_HSMale, popEducSexAge_HSFemale, 
                       popEducSexAge_someCollegeMale, popEducSexAge_someCollegeFemale,
                       popEducSexAge_BachPlusMale, popEducSexAge_BachPlusFemale)

popRaceSexAge_all_2010 <- get_acs(geography = "congressional district", 
                                  variables = popRaceSexAge_all, 
                                  year = 2010, state = 42) %>%
  left_join(var_names) %>%
  mutate(year = 2010) 


popRaceSexAge_all_2011 <- get_acs(geography = "congressional district", 
                                  variables = popRaceSexAge_all, 
                                  year = 2011, state = 42) %>%
  left_join(var_names) %>%
  mutate(year = 2011)

popRaceSexAge_all_2012 <- get_acs(geography = "congressional district", 
                                  variables = popRaceSexAge_all, 
                                  year = 2012, state = 42) %>%
  left_join(var_names) %>%
  mutate(year = 2012) 

popRaceSexAge_all_2013 <- get_acs(geography = "congressional district", 
                                  variables = popRaceSexAge_all, 
                                  year = 2013, state = 42) %>%
  left_join(var_names) %>%
  mutate(year = 2013)

popRaceSexAge_all_2014 <- get_acs(geography = "congressional district", 
                                  variables = popRaceSexAge_all, 
                                  year = 2014, state = 42) %>%
  left_join(var_names) %>%
  mutate(year = 2014)


popRaceSexAge_all_2015 <- get_acs(geography = "congressional district", 
                                  variables = popRaceSexAge_all, 
                                  year = 2015, state = 42) %>%
  left_join(var_names) %>%
  mutate(year = 2015)

popRaceSexAge_all_2016 <- get_acs(geography = "congressional district", 
                                  variables = popRaceSexAge_all, 
                                  year = 2015, state = 42) %>%
  left_join(var_names) %>%
  mutate(year = 2015)


population_byRaceSexAgeCD_orig <- bind_rows(popRaceSexAge_all_2010, popRaceSexAge_all_2011, 
                                    popRaceSexAge_all_2012, popRaceSexAge_all_2013, 
                                    popRaceSexAge_all_2014, popRaceSexAge_all_2015) %>%
  rename(Population = estimate) %>%
  mutate(
    CD = as.integer(str_sub(GEOID, start=-2)) %>%
      factor(levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),
             labels = c("CD 01", "CD 02", "CD 03", "CD 04", "CD 05",
                        "CD 06", "CD 07", "CD 08", "CD 09", "CD 10",
                        "CD 11", "CD 12", "CD 13", "CD 14", "CD 15",
                        "CD 16", "CD 17", "CD 18", "CD 19")),
    SEX = case_when(
      grepl("Male", label, fixed = TRUE) == TRUE ~ 1,
      grepl("Female", label, fixed = TRUE) == TRUE ~ 2) %>%
      factor(levels = c(1,2), labels = c("Male", "Female")),
    "RACE" = case_when(
      grepl("WHITE", concept, fixed = TRUE) == TRUE ~ 1,
      grepl("BLACK", concept, fixed = TRUE) == TRUE ~ 2,
      grepl("AMERICAN INDIAN", concept, fixed = TRUE) == TRUE ~ 3,
      grepl("ASIAN", concept, fixed = TRUE) == TRUE ~ 4,
      grepl("NATIVE HAWAIIAN", concept, fixed = TRUE) == TRUE ~ 4,
      grepl("OTHER RACE", concept, fixed = TRUE) == TRUE ~ 5,
      grepl("TWO OR MORE", concept, fixed = TRUE) == TRUE ~ 5) %>%
      factor(levels = c(1,2,3,4,5),
             labels = c("White", "Black", "American Indian, Alaska Native", 
                              "Asian American Pacific Islander", "Other")),
    AGE_CAT = case_when(
      grepl(25, label, fixed = TRUE) == TRUE ~ 7,
      grepl(30, label, fixed = TRUE) == TRUE ~ 8,
      grepl(35, label, fixed = TRUE) == TRUE ~ 9,
      grepl(45, label, fixed = TRUE) == TRUE ~ 10,
      grepl(55, label, fixed = TRUE) == TRUE ~ 11) %>%
      factor(levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),
             labels = c("Under 5 years", "05 to 09 years", "10 to 14 years",
                        "15 to 17 years", "18 and 19 years", "20 to 24 years",
                        "25 to 29 years", "30 to 34 years", "35 to 44 years",
                        "45 to 54 years", "55 to 64 years", "65 to 74 years",
                        "75 to 84 years", "85 years and over"))) %>%
  select(c("CD", "SEX", "RACE", "AGE_CAT", "year", "Population")) %>%
  group_by(CD, SEX, RACE, AGE_CAT, year) %>%
  summarise(Population = sum(Population)) %>%
  ungroup() %>%
  inner_join(CONGRESS) %>%
  inner_join(AGE_CATS)

#For 111 - 112, we are using the acs5 from 2011 because it is the latest acs5 with CD19. To estimate 2010-2012, we take our estimates
#and multiply by 3. For 113 - -114, we are using the acs5 from 2016 (2012 - 2016) and multiplying by 3.
population_byRaceSexAgeCD <- bind_rows(popRaceSexAge_all_2011, popRaceSexAge_all_2016) %>%
  rename(Population = estimate) %>%
  mutate(
    Population = Population * 3,
    CD = as.integer(str_sub(GEOID, start=-2)) %>%
      factor(levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),
             labels = c("CD 01", "CD 02", "CD 03", "CD 04", "CD 05",
                        "CD 06", "CD 07", "CD 08", "CD 09", "CD 10",
                        "CD 11", "CD 12", "CD 13", "CD 14", "CD 15",
                        "CD 16", "CD 17", "CD 18", "CD 19")),
    SEX = case_when(
      grepl("Male", label, fixed = TRUE) == TRUE ~ 1,
      grepl("Female", label, fixed = TRUE) == TRUE ~ 2) %>%
      factor(levels = c(1,2), labels = c("Male", "Female")),
    "RACE" = case_when(
      grepl("WHITE", concept, fixed = TRUE) == TRUE ~ 1,
      grepl("BLACK", concept, fixed = TRUE) == TRUE ~ 2,
      grepl("AMERICAN INDIAN", concept, fixed = TRUE) == TRUE ~ 3,
      grepl("ASIAN", concept, fixed = TRUE) == TRUE ~ 4,
      grepl("NATIVE HAWAIIAN", concept, fixed = TRUE) == TRUE ~ 4,
      grepl("OTHER RACE", concept, fixed = TRUE) == TRUE ~ 5,
      grepl("TWO OR MORE", concept, fixed = TRUE) == TRUE ~ 5) %>%
      factor(levels = c(1,2,3,4,5),
             labels = c("White", "Black", "American Indian, Alaska Native", 
                        "Asian American Pacific Islander", "Other")),
    AGE_CAT = case_when(
      grepl(25, label, fixed = TRUE) == TRUE ~ 7,
      grepl(30, label, fixed = TRUE) == TRUE ~ 8,
      grepl(35, label, fixed = TRUE) == TRUE ~ 9,
      grepl(45, label, fixed = TRUE) == TRUE ~ 10,
      grepl(55, label, fixed = TRUE) == TRUE ~ 11) %>%
      factor(levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),
             labels = c("Under 5 years", "05 to 09 years", "10 to 14 years",
                        "15 to 17 years", "18 and 19 years", "20 to 24 years",
                        "25 to 29 years", "30 to 34 years", "35 to 44 years",
                        "45 to 54 years", "55 to 64 years", "65 to 74 years",
                        "75 to 84 years", "85 years and over"))) %>%
  select(c("CD", "SEX", "RACE", "AGE_CAT", "year", "Population")) %>%
  group_by(CD, SEX, RACE, AGE_CAT, year) %>%
  summarise(Population = sum(Population)) %>%
  ungroup() %>%
  inner_join(CONGRESS) %>%
  inner_join(AGE_CATS)

population_byRaceSexAgeCD_byCongress <- population_byRaceSexAgeCD%>%
  group_by(CD, SEX, RACE, AGE_CAT_EDUC, CONGRESS) %>%
  summarise(Population = sum(Population, na.rm = TRUE))

population_byRace2SexAgeCD_byCongress <- inner_join(population_byRaceSexAgeCD_byCongress, RACE2) %>%
  group_by(CD, SEX, RACE2, AGE_CAT_EDUC, CONGRESS) %>%
  summarise(Population = sum(Population, na.rm = TRUE))

population_byRaceSexAgeCD_byCongress2 <- population_byRaceSexAgeCD %>%
  group_by(CD, SEX, RACE, AGE_CAT_EDUC, CONGRESS2) %>%
  summarise(Population = sum(Population, na.rm = TRUE))

population_byRaceSexAgeCD_byCongress2_orig <- population_byRaceSexAgeCD_orig %>%
  group_by(CD, SEX, RACE, AGE_CAT_EDUC, CONGRESS2) %>%
  summarise(Population = sum(Population, na.rm = TRUE))

population_byRace2SexAgeCD_byCongress2 <- inner_join(population_byRaceSexAgeCD_byCongress2, RACE2) %>%
  group_by(CD, SEX, RACE2, AGE_CAT_EDUC, CONGRESS2) %>%
  summarise(Population = sum(Population, na.rm = TRUE))

################################################################################

popEducSexAge_all_2010 <- get_acs(geography = "congressional district", 
                                  variables = popEducSexAge_all, 
                                  year = 2010, state = 42) %>%
  left_join(var_names) %>%
  mutate(year = 2010)
    

popEducSexAge_all_2011 <- get_acs(geography = "congressional district", 
                                  variables = popEducSexAge_all, 
                                  year = 2011, state = 42) %>%
  left_join(var_names) %>%
  mutate(year = 2011)
 
popEducSexAge_all_2012 <- get_acs(geography = "congressional district", 
                                  variables = popEducSexAge_all, 
                                  year = 2012, state = 42) %>%
  left_join(var_names) %>%
  mutate(year = 2012)
 
popEducSexAge_all_2013 <- get_acs(geography = "congressional district", 
                                  variables = popEducSexAge_all, 
                                  year = 2013, state = 42) %>%
  left_join(var_names) %>%
  mutate(year = 2013)
 
popEducSexAge_all_2014 <- get_acs(geography = "congressional district", 
                                  variables = popEducSexAge_all, 
                                  year = 2014, state = 42) %>%
  left_join(var_names) %>%
  mutate(year = 2014)

popEducSexAge_all_2015 <- get_acs(geography = "congressional district", 
                                  variables = popEducSexAge_all, 
                                  year = 2015, state = 42) %>%
  left_join(var_names) %>%
  mutate(year = 2015)

popEducSexAge_all_2016 <- get_acs(geography = "congressional district", 
                                  variables = popEducSexAge_all, 
                                  year = 2016, state = 42) %>%
  left_join(var_names) %>%
  mutate(year = 2015)
  
population_byEducSexAgeCD_orig <- bind_rows(popEducSexAge_all_2010, popEducSexAge_all_2011, 
                                       popEducSexAge_all_2012, popEducSexAge_all_2013, 
                                       popEducSexAge_all_2014, popEducSexAge_all_2015) %>%
  mutate(
    CD = as.integer(str_sub(GEOID, start=-2)) %>%
      factor(levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),
            labels = c("CD 01", "CD 02", "CD 03", "CD 04", "CD 05",
                       "CD 06", "CD 07", "CD 08", "CD 09", "CD 10",
                       "CD 11", "CD 12", "CD 13", "CD 14", "CD 15",
                       "CD 16", "CD 17", "CD 18", "CD 19")),
    SEX = case_when(
      grepl("Male", label, fixed = TRUE) == TRUE ~ 1,
      grepl("Female", label, fixed = TRUE) == TRUE ~ 2) %>%
      factor(levels = c(1,2),
             labels = c("Male", "Female")),
    "EDUC" = case_when(
      grepl("Less than 9th grade", label, fixed = TRUE) == TRUE ~ 1,
      grepl("9th to 12th grade, no diploma", label, fixed = TRUE) == TRUE ~ 1,
      grepl("High school graduate", label, fixed = TRUE) == TRUE ~ 2,
      grepl("Some college, no degree", label, fixed = TRUE) == TRUE ~ 3,
      grepl("Associate's degree", label, fixed = TRUE) == TRUE ~ 3,
      grepl("Bachelor's degree", label, fixed = TRUE) == TRUE ~ 4,
      grepl("Graduate or professional degree", label, fixed = TRUE) == TRUE ~ 4) %>%
      factor(levels = c(1,2,3,4),
             labels = c("Less than High School","High School",
                              "Some College/Associate Degree",
                              "Bachelor/Master/Doctorate/Professional Degree")),
    AGE_CAT_EDUC = case_when(
      grepl("25", label, fixed = TRUE) == TRUE ~ 3,
      grepl("35", label, fixed = TRUE) == TRUE ~ 4,
      grepl("45", label, fixed = TRUE) == TRUE ~ 5) %>%
      factor(levels = c(1,2,3,4,5,6),
             labels = c("Under 18 years", "18 to 24 years", "25 to 34 years",
                        "35 to 44 years", "45 to 64 years", "65 years and over"))) %>%
  rename(Population = estimate) %>%
  select(c("CD", "SEX", "EDUC", "AGE_CAT_EDUC", "year", "Population")) %>%
  group_by(CD, SEX, EDUC, AGE_CAT_EDUC, year) %>%
  summarise(Population = sum(Population)) %>%
  ungroup() %>%
  inner_join(CONGRESS)

population_byEducSexAgeCD <- bind_rows(popEducSexAge_all_2011, popEducSexAge_all_2016) %>%
  rename(Population = estimate) %>%
  mutate(
    Population = Population * 3,
    CD = as.integer(str_sub(GEOID, start=-2)) %>%
      factor(levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),
             labels = c("CD 01", "CD 02", "CD 03", "CD 04", "CD 05",
                        "CD 06", "CD 07", "CD 08", "CD 09", "CD 10",
                        "CD 11", "CD 12", "CD 13", "CD 14", "CD 15",
                        "CD 16", "CD 17", "CD 18", "CD 19")),
    SEX = case_when(
      grepl("Male", label, fixed = TRUE) == TRUE ~ 1,
      grepl("Female", label, fixed = TRUE) == TRUE ~ 2) %>%
      factor(levels = c(1,2),
             labels = c("Male", "Female")),
    "EDUC" = case_when(
      grepl("Less than 9th grade", label, fixed = TRUE) == TRUE ~ 1,
      grepl("9th to 12th grade, no diploma", label, fixed = TRUE) == TRUE ~ 1,
      grepl("High school graduate", label, fixed = TRUE) == TRUE ~ 2,
      grepl("Some college, no degree", label, fixed = TRUE) == TRUE ~ 3,
      grepl("Associate's degree", label, fixed = TRUE) == TRUE ~ 3,
      grepl("Bachelor's degree", label, fixed = TRUE) == TRUE ~ 4,
      grepl("Graduate or professional degree", label, fixed = TRUE) == TRUE ~ 4) %>%
      factor(levels = c(1,2,3,4),
             labels = c("Less than High School","High School",
                        "Some College/Associate Degree",
                        "Bachelor/Master/Doctorate/Professional Degree")),
    AGE_CAT_EDUC = case_when(
      grepl("25", label, fixed = TRUE) == TRUE ~ 3,
      grepl("35", label, fixed = TRUE) == TRUE ~ 4,
      grepl("45", label, fixed = TRUE) == TRUE ~ 5) %>%
      factor(levels = c(1,2,3,4,5,6),
             labels = c("Under 18 years", "18 to 24 years", "25 to 34 years",
                        "35 to 44 years", "45 to 64 years", "65 years and over"))) %>%
  select(c("CD", "SEX", "EDUC", "AGE_CAT_EDUC", "year", "Population")) %>%
  group_by(CD, SEX, EDUC, AGE_CAT_EDUC, year) %>%
  summarise(Population = sum(Population)) %>%
  ungroup() %>%
  inner_join(CONGRESS)

#Sum population over CD
population_byEducSexAgeCD_byCongress <- population_byEducSexAgeCD %>%
  group_by(CD, SEX, EDUC, AGE_CAT_EDUC, CONGRESS) %>%
  summarise(Population = sum(Population, na.rm = TRUE))

population_byEducSexAgeCD_byCongress2 <- group_by(population_byEducSexAgeCD, CD, SEX, 
                                                  EDUC, AGE_CAT_EDUC, CONGRESS2) %>%
  summarise(Population = sum(Population, na.rm = TRUE))

population_byEducSexAgeCD_byCongress2_orig <- group_by(population_byEducSexAgeCD_orig, CD, SEX, 
                                                  EDUC, AGE_CAT_EDUC, CONGRESS2) %>%
  summarise(Population = sum(Population, na.rm = TRUE))
