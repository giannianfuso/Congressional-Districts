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

#Replace with your own directory path
dir_name <- "//files.drexel.edu\\encrypted\\SOPH\\UHC\\SchnakeMahl_PAVitalStat\\Data\\"

################################################################################

#Columns we need for birth files
keep_birth <- c("FILENO", "SEX", "CHILDDOB", "MOTHEDU", "MOTHHISP0315", 
                "MOTHHISP8902", "MOTHRACE0315", "mothrace7902", "FATHEDU",
                "FATHHISP0315", "FATHHISP8902", "FATHRACE0315", "fathrace7902", 
                "GEOID10", "year")

#Import birth data
birth2010 <- fread(paste0(dir_name, "birthgeo2010.csv")) %>%
  mutate(year = "2010") %>%
  select(one_of(keep_birth))


birth2011 <- fread(paste0(dir_name, "birthgeo2011.csv")) %>%
  mutate(year = "2011") %>%
  select(one_of(keep_birth))

birth2012 <- fread(paste0(dir_name, "birthgeo2012.csv")) %>%
  mutate(year = "2012") %>%
  select(one_of(keep_birth))

birth2013 <- fread(paste0(dir_name, "birthgeo2013.csv")) %>%
  mutate(year = "2013") %>%
  select(one_of(keep_birth))

birth2014 <- fread(paste0(dir_name, "birthgeo2014.csv")) %>%
  mutate(year = "2014") %>%
  select(one_of(keep_birth))

birth2015 <- fread(paste0(dir_name, "birthgeo2015.csv")) %>%
  mutate(year = "2015") %>%
  select(one_of(keep_birth))

birth_original <- bind_rows(birth2010, birth2011, birth2012, 
                            birth2013, birth2014, birth2015)

birth_original_modified <- birth_original %>%
  mutate(
    MOMHISP = case_when(
      #(MOTHHISP8902 == 0) | only works if we have pre-2010 data
      (MOTHHISP0315 == 1) ~ 1, #Not Hispanic
      #((MOTHHISP8902 >= 1) & (MOTHHISP8902 <= 5) | 
      (MOTHHISP0315 >= 2) & (MOTHHISP0315 <= 5) ~ 2, #Hispanic
      #(MOTHHISP8902 == 9) 
      (MOTHHISP0315 == 9) ~ 3), #Unknown
    DADHISP = case_when(
      #(FATHHISP8902 == 0)  
      (FATHHISP0315 == 1) ~ 1, #Not Hispanic
      # ((FATHHISP8902 >= 1) & (FATHHISP8902 <= 5) 
      (FATHHISP0315 >= 2) & (FATHHISP0315 <= 5) ~ 2, #Hispanic
      # (FATHHISP8902 == 9) 
      (FATHHISP0315 == 9) ~ 3), #Unknown
    HISP = case_when(
      !is.na(MOMHISP) ~ MOMHISP,
      is.na(MOMHISP) ~ DADHISP) %>%
      factor(labels = c("Non-Hispanic", "Hispanic", "Unknown")),
    MOMRACE = case_when(
      # (mothrace7902 == 1) 
      (MOTHRACE0315 == 1)  ~ 1, #White
      # (mothrace7902 == 2)  
      (MOTHRACE0315 == 2)  ~ 2, #Black
      # (mothrace7902 == 3) 
      (MOTHRACE0315 == 3)  ~ 3, #American Indian Alaska Native 
      # (mothrace7902 >=4 & mothrace7902<= 6) | (mothrace7902 == 8)
      (MOTHRACE0315 >= 4 & MOTHRACE0315 <= 14) ~ 4, #AAPI
      # (mothrace7902 == 7 | mothrace7902 == 0 | is.na(mothrace7902))
      (MOTHRACE0315 >= 15 | is.na(MOTHRACE0315)) ~ 5), #Other
    DADRACE = case_when(
      # (fathrace7902 == 1)
      (FATHRACE0315 == 1) ~ 1, #White
      # (fathrace7902 == 2) 
      (FATHRACE0315 == 2) ~ 2, #Black
      # (fathrace7902 == 3)
      (FATHRACE0315 == 3) ~ 3, #American Indian Alaska Native 
      # (fathrace7902 >=4 & fathrace7902<= 6) | (fathrace7902 == 8) 
      (FATHRACE0315 >= 4 & FATHRACE0315 <= 14) ~ 4, #AAPIT
      # (fathrace7902 == 7 | fathrace7902 == 0 | is.na(fathrace7902)) 
      (FATHRACE0315 >= 15 | is.na(FATHRACE0315)) ~ 5), #Other
    RACE = case_when(
      !is.na(MOMRACE) ~ MOMRACE,
      is.na(MOMRACE) ~ DADRACE) %>%
      factor(
        levels = c(1,2,3,4,5),
        labels = c("White", "Black", "American Indian, Alaska Native", 
                   "Asian American Pacific Islander", "Other")),
    RACE2 = case_when(
      as.integer(RACE) == 1 ~ 1, #White
      as.integer(RACE) > 1 ~ 2) %>% #Non-White
      factor(
        levels = c(1,2),
        labels = c("White", "Non-White")),
    RACEHISP = case_when(
      as.integer(HISP) == 2 ~ 1, #Hispanic
      as.integer(HISP) == 1 & as.integer(RACE) == 1 ~ 2, #Non-Hispanic White
      as.integer(HISP) == 1 & as.integer(RACE) == 2 ~ 3, #Non-Hispanic Black
      as.integer(HISP) == 1 & as.integer(RACE) == 3 ~ 4, #Non-Hispanic American Indian Alaska Native
      as.integer(HISP) == 1 & as.integer(RACE) == 4 ~ 5, #Non-Hispanic AAPI
      as.integer(HISP) == 1 & as.integer(RACE) == 5 ~ 6, #Non-Hispanic Other
      as.integer(HISP) == 3 ~ 7) %>% #Unknown
      factor(
        levels = c(1,2,3,4,5,6,7),
        labels = c("Hispanic", "Non-Hispanic White", "Non-Hispanic Black", 
                   "Non-Hispanic American Indian, Alaska Native",
                   "Non-Hispanic Asian American Pacific Islander", "Non-Hispanic Other", "Unknown")),
    RACEHISP2 = case_when(
      (as.integer(HISP) == 2) ~ 1, #Hispanic
      (as.integer(HISP) == 1) & (as.integer(RACE2) == 1) ~ 2, #Non-Hispanic White
      (as.integer(HISP) == 1) & (as.integer(RACE2) == 2) ~ 3, #Non-Hispanic Non-White
      (as.integer(HISP) == 3) ~ 4) %>% #Unknown
      factor(
        levels = c(1,2,3,4),
        labels = c("Hispanic", "Non-Hispanic White", "Non-Hispanic Non-White", "Unknown")),
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

#See how unknown entries we have with respect to race-ethnicity
birth_original_modified_noracehisp <- birth_original_modified %>%
  filter(RACEHISP == "Unknown")

#What % of the data is unknown with respect to race-ethnicity?
birth_original_modified_racehisp_pct <- nrow(birth_original_modified_noracehisp)/nrow(birth_original_modified) * 100

#What % of the birth data is not georeferenced?
birth_nogeo <- nrow(filter(birth_original, is.na(GEOID10) | nchar(as.numeric(GEOID10)) < 10))/nrow(birth_original) * 100

#Columns we need for death files
keep_death <- c("SEX", "AGE6011", "AGE1215", "AGE", "Age_death", "DOB", "HISPANIC", 
                "RACE9011", "race1215", "EDUC8711", "EDUC1215", "EDUC", "CODICD10", 
                "INFMORT", "GEOID10", "year")

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

death_original <- bind_rows(death2010, death2011, death2012, 
                            death2013, death2014, death2015)

#What % of the death data is not georeferenced?
death_nogeo <- nrow(filter(death_original, is.na(GEOID10) | nchar(as.numeric(GEOID10)) < 10))/nrow(death_original) * 100

#Add despair indicator for deaths
death_original_despair <- death_original %>%
  mutate(
    EDUC = case_when(
      EDUC == 0 ~ 1, #Less than HS
      EDUC == 1 ~ 2, #HS
      EDUC == 2 ~ 3, #Some College
      EDUC == 3 ~ 4, #College+
      TRUE ~ 5) %>% #Unknown
      factor(
        levels = c(1,2,3,4,5),
        labels = c("Less than High School", "High School", "Some College/Associate Degree",
                   "Bachelor/Master/Doctorate/Professional Degree", "Unknown")),
    RACE = case_when(
      RACE9011 == 1 | 
        race1215 == 01 ~ 1, #White
      RACE9011 == 2 | 
        race1215 == 02 ~ 2, #Black
      RACE9011 == 3 | 
        race1215 == 03 ~ 3, #American Indian Alaska Native
      ((RACE9011 >= 4 & RACE9011 <= 8) | RACE9011 == 0) | 
        (race1215 >= 04 & race1215 <= 14) ~ 4, #AAPI
      RACE9011 == 9 | is.na(RACE9011) | 
        is.na(race1215) | race1215>= 15 ~ 5) %>% #Other
      factor(
        levels = c(1,2,3,4,5),
        labels = c("White", "Black", 
                   "American Indian, Alaska Native", "Asian American Pacific Islander", "Other")),
    RACE2 = case_when(
      as.integer(RACE) == 1 ~ 1, #White
      as.integer(RACE) > 1 ~ 2) %>% #Non-White
      factor(
        levels = c(1,2),
        labels = c("White", "Non-White")),
    HISP = case_when(
      #Non-Hispanic
      HISPANIC == 1 ~ 1, #Non-Hispanic
      #Hispanic
      HISPANIC >= 2 & HISPANIC <= 5 ~ 2, #Hispanic
      #Unknown
      HISPANIC == 9 ~ 3), #Unknown
    HISP = factor(HISP, labels = c("Non-Hispanic", "Hispanic", "Unknown")),
    RACEHISP = case_when(
      (as.integer(HISP) == 2) ~ 1, #Hispanic
      (as.integer(HISP) == 1) & (as.integer(RACE) == 1) ~ 2, #Non-Hispanic White
      (as.integer(HISP) == 1) & (as.integer(RACE) == 2) ~ 3, #Non-Hispanic Black
      (as.integer(HISP) == 1) & (as.integer(RACE) == 3) ~ 4, #Non-Hispanic American Indian Alaska Native
      (as.integer(HISP) == 1) & (as.integer(RACE) == 4) ~ 5, #Non-Hispanic AAPI
      (as.integer(HISP) == 1) & (as.integer(RACE) == 5) ~ 6, #Non-Hispanic Other
      (as.integer(HISP) == 3) ~ 7) %>% #Unknown
      factor(
        levels = c(1,2,3,4,5,6,7),
        labels = c("Hispanic", "Non-Hispanic White", "Non-Hispanic Black", 
                   "Non-Hispanic American Indian, Alaska Native",
                   "Non-Hispanic Asian American Pacific Islander", "Non-Hispanic Other", "Unknown")),
    RACEHISP2 = case_when(
      (as.integer(HISP) == 2) ~ 1, #Hispanic
      (as.integer(HISP) == 1) & (as.integer(RACE2) == 1) ~ 2, #Non-Hispanic White
      (as.integer(HISP) == 1) & (as.integer(RACE2) == 2) ~ 3, #Non-Hispanic Non-White
      (as.integer(HISP) == 3) ~ 4) %>% #Unknown
      factor(
        levels = c(1,2,3,4),
        labels = c("Hispanic", "Non-Hispanic White", "Non-Hispanic Non-White", "Unknown")),
    AGE = floor(Age_death),
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
      TRUE ~ 0)
  )

#How many DoD observations are missing race-ethnicity?
death_original_despair_noracehisp <- death_original_despair %>%
  filter(RACEHISP == "Unknown")

#What % of DoD observations are missing race-ethnicity?
death_original_despair_noracehisp_pct <- nrow(death_original_despair_noracehisp)/nrow(death_original_despair) * 100

#How many DoD observations are missing education data?
death_original_despair_noeduc <- death_original_despair %>%
  filter(EDUC == "Unknown" | is.na(AGE))

#hat % of DoD observations are missing education data?
death_original_despair_noeduc_pct <- nrow(death_original_despair_noeduc)/nrow(death_original_despair) * 100

#Determine what % of all infant mortalities could not be geocoded
infMort_total <- filter(death_original, INFMORT == 1) %>%
  mutate(GEOID10 = as.numeric(GEOID10), char = nchar(GEOID10))

infMort_total_byYear <- infMort_total %>% group_by(year) %>% summarise(tot=n())

infMort_nogeo <- filter(infMort_total, is.na(GEOID10) | char < 10)

infMort_nogeo_byYear <- infMort_nogeo %>% group_by(year) %>% summarise(n=n()) %>%
  inner_join(infMort_total_byYear) %>%
  mutate(pct = n/tot*100)

nrow(infMort_nogeo)/nrow(infMort_total)*100

#Determine what % of DoD were not geocoded
DOD_total <- filter(death_original_despair, DESPAIR==1)%>%
  mutate(GEOID10 = as.numeric(GEOID10), char = nchar(GEOID10))

DOD_total_byYear <- DOD_total %>% group_by(year) %>% summarise(tot = n())

DOD_nogeo <- filter(DOD_total, is.na(GEOID10) | char < 10)

DOD_nogeo_byYear <- DOD_nogeo %>% group_by(year) %>% summarise(n=n()) %>%
  inner_join(DOD_total_byYear) %>% mutate(pct = n/tot * 100)

nrow(DOD_nogeo)/nrow(DOD_total)*100

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
      MOMHISP = case_when(
        #(MOTHHISP8902 == 0) | only works if we have pre-2010 data
          (MOTHHISP0315 == 1) ~ 1, #Non-Hispanic
        #((MOTHHISP8902 >= 1) & (MOTHHISP8902 <= 5) | 
           (MOTHHISP0315 >= 2) & (MOTHHISP0315 <= 5) ~ 2, #Hispanic
        #(MOTHHISP8902 == 9) | 
          (MOTHHISP0315 == 9) ~ 3), #Unknown
      DADHISP = case_when(
        #(FATHHISP8902 == 0) | 
          (FATHHISP0315 == 1) ~ 1, #Non-Hispanic
        # ((FATHHISP8902 >= 1) & (FATHHISP8902 <= 5) | 
           (FATHHISP0315 >= 2) & (FATHHISP0315 <= 5) ~ 2, #Hispanic
        # (FATHHISP8902 == 9) | 
          (FATHHISP0315 == 9) ~ 3), #Unknown
      HISP = case_when(
        !is.na(MOMHISP) ~ MOMHISP,
        is.na(MOMHISP) ~ DADHISP) %>%
          factor(labels = c("Non-Hispanic", "Hispanic", "Unknown")),
      MOMRACE = case_when(
        # (mothrace7902 == 1) | 
          (MOTHRACE0315 == 1)  ~ 1, #White
        # (mothrace7902 == 2) | 
          (MOTHRACE0315 == 2)  ~ 2, #Black
        # (mothrace7902 == 3) | 
          (MOTHRACE0315 == 3)  ~ 3, #American Indian Alaska Native
        # (mothrace7902 >=4 & mothrace7902<= 6) | (mothrace7902 == 8)
          (MOTHRACE0315 >= 4 & MOTHRACE0315 <= 14) ~ 4, #AAPI
        # (mothrace7902 == 7 | mothrace7902 == 0 | is.na(mothrace7902))
          (MOTHRACE0315 >= 15 | is.na(MOTHRACE0315)) ~ 5), #Other
      DADRACE = case_when(
        # (fathrace7902 == 1)  |
          (FATHRACE0315 == 1) ~ 1, #White
        # (fathrace7902 == 2) | 
          (FATHRACE0315 == 2) ~ 2, #Black
        # (fathrace7902 == 3) | 
          (FATHRACE0315 == 3) ~ 3, #American Indian Alaska Native
        # (fathrace7902 >=4 & fathrace7902<= 6) | (fathrace7902 == 8) |
          (FATHRACE0315 >= 4 & FATHRACE0315 <= 14) ~ 4, #AAPI
        # (fathrace7902 == 7 | fathrace7902 == 0 | is.na(fathrace7902)) | 
          (FATHRACE0315 >= 15 | is.na(FATHRACE0315)) ~ 5), #Other
      RACE = case_when(
        !is.na(MOMRACE) ~ MOMRACE,
        is.na(MOMRACE) ~ DADRACE) %>%
          factor(
            levels = c(1,2,3,4,5),
            labels = c("White", "Black", "American Indian, Alaska Native", 
                       "Asian American Pacific Islander", "Other")),
      RACE2 = case_when(
        as.integer(RACE) == 1 ~ 1, #White
        as.integer(RACE) > 1 ~ 2) %>% #Non-White
        factor(
          levels = c(1,2),
          labels = c("White", "Non-White")),
      RACEHISP = case_when(
        as.integer(HISP) == 2 ~ 1, #Hispanic
        as.integer(HISP) == 1 & as.integer(RACE) == 1 ~ 2, #Non-Hispanic White
        as.integer(HISP) == 1 & as.integer(RACE) == 2 ~ 3, #Non-Hispanic Black
        as.integer(HISP) == 1 & as.integer(RACE) == 3 ~ 4, #Non-Hispanic American Indian Alaska Native
        as.integer(HISP) == 1 & as.integer(RACE) == 4 ~ 5, #Non-Hispanic AAPI
        as.integer(HISP) == 1 & as.integer(RACE) == 5 ~ 6, #Non-Hispanic Other
        as.integer(HISP) == 3 ~ 7) %>% #Unknown
          factor(
            levels = c(1,2,3,4,5,6,7),
            labels = c("Hispanic", "Non-Hispanic White", "Non-Hispanic Black", 
                        "Non-Hispanic American Indian, Alaska Native",
                        "Non-Hispanic Asian American Pacific Islander", "Non-Hispanic Other", "Unknown")),
      RACEHISP2 = case_when(
        (as.integer(HISP) == 2) ~ 1, #Hispanic
        (as.integer(HISP) == 1) & (as.integer(RACE2) == 1) ~ 2, #Non-Hispanic White
        (as.integer(HISP) == 1) & (as.integer(RACE2) == 2) ~ 3, #Non-Hispanic Non-White
        (as.integer(HISP) == 3) ~ 4) %>% #Unknown
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
          race1215 == 01 ~ 1, #White
        RACE9011 == 2 | 
          race1215 == 02 ~ 2, #Black
        RACE9011 == 3 | 
          race1215 == 03 ~ 3, #American Indian Alaska Native
        ((RACE9011 >= 4 & RACE9011 <= 8) | RACE9011 == 0) | 
          (race1215 >= 04 & race1215 <= 14) ~ 4, #AAPI
        RACE9011 == 9 | is.na(RACE9011) | 
          is.na(race1215) | race1215>= 15 ~ 5) %>% #Other
            factor(
              levels = c(1,2,3,4,5),
              labels = c("White", "Black", 
              "American Indian, Alaska Native", "Asian American Pacific Islander", "Other")),
      RACE2 = case_when(
        as.integer(RACE) == 1 ~ 1, #White
        as.integer(RACE) > 1 ~ 2) %>% #Non-White
          factor(
            levels = c(1,2),
            labels = c("White", "Non-White")),
      HISP = case_when(
        HISPANIC == 1 ~ 1, #Non-Hispanic
        HISPANIC >= 2 & HISPANIC <= 5 ~ 2, #Hispanic
        HISPANIC == 9 ~ 3), #Unknown
      HISP = factor(HISP, labels = c("Non-Hispanic", "Hispanic", "Unknown")),
      RACEHISP = case_when(
        (as.integer(HISP) == 2) ~ 1, #Hispanic
        (as.integer(HISP) == 1) & (as.integer(RACE) == 1) ~ 2, #Non-Hispanic White
        (as.integer(HISP) == 1) & (as.integer(RACE) == 2) ~ 3, #Non-Hispanic Black
        (as.integer(HISP) == 1) & (as.integer(RACE) == 3) ~ 4, #Non-Hispanic American Indian Alaska Native
        (as.integer(HISP) == 1) & (as.integer(RACE) == 4) ~ 5, #Non-Hispanic AAPI
        (as.integer(HISP) == 1) & (as.integer(RACE) == 5) ~ 6, #Non-Hispanic Other
        (as.integer(HISP) == 3) ~ 7) %>% #Unknown
          factor(
            levels = c(1,2,3,4,5,6,7),
            labels = c("Hispanic", "Non-Hispanic White", "Non-Hispanic Black", 
                       "Non-Hispanic American Indian, Alaska Native",
                       "Non-Hispanic Asian American Pacific Islander", "Non-Hispanic Other", "Unknown")),
      RACEHISP2 = case_when(
        (as.integer(HISP) == 2) ~ 1, #Hispanic
        (as.integer(HISP) == 1) & (as.integer(RACE2) == 1) ~ 2, #Non-Hispanic White
        (as.integer(HISP) == 1) & (as.integer(RACE2) == 2) ~ 3, #Non-Hispanic Non-White
        (as.integer(HISP) == 3) ~ 4) %>% #Unknown
          factor(
            levels = c(1,2,3,4),
            labels = c("Hispanic", "Non-Hispanic White", "Non-Hispanic Non-White", "Unknown")),
      SEX = factor(SEX, levels = c(1,2,3), labels = c("Male", "Female", "Unknown")),
      AGE = floor(Age_death),
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
        EDUC == 0 ~ 1, #Less than HS
        EDUC == 1 ~ 2, #HS
        EDUC == 2 ~ 3, #Some College
        EDUC == 3 ~ 4, #College+
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
#ACS variables
var_names <- load_variables(2015, "acs5", cache = TRUE) %>% rename(variable = name)

#Population variables - men and women ages 25-64
pop_male_25_64 <- paste0("B01001_", sprintf("%03d", 11:19))
pop_female_25_64 <- paste0("B01001_", sprintf("%03d", 35:43))
pop_25_64 <- c(pop_male_25_64, pop_female_25_64)

#Population variables - by race
pop_byRace_vars <- c("B03002_012", "B03002_003", "B03002_004",
                     "B03002_005", "B03002_006", "B03002_007", 
                     "B03002_008", "B03002_009")

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

################################################################################

pop_25_64_2011 <- get_acs(geography = "congressional district", 
                          variables = pop_25_64, 
                          year = 2011, state = 42) %>%
  left_join(var_names) %>%
  mutate(year = 2011)

pop_pop_25_64_2016 <- get_acs(geography = "congressional district", 
                          variables = pop_25_64, 
                          year = 2016, state = 42) %>%
  left_join(var_names) %>%
  mutate(year = 2015)

population_pop_25_64_byCD <- bind_rows(pop_25_64_2011, pop_pop_25_64_2016) %>%
  rename(Population = estimate) %>%
  mutate(
    Population = Population * 3,
    CD = as.integer(str_sub(GEOID, start=-2)) %>%
      factor(levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),
             labels = c("CD 01", "CD 02", "CD 03", "CD 04", "CD 05",
                        "CD 06", "CD 07", "CD 08", "CD 09", "CD 10",
                        "CD 11", "CD 12", "CD 13", "CD 14", "CD 15",
                        "CD 16", "CD 17", "CD 18", "CD 19"))) %>%
  select(c(CD, year, Population)) %>%
  group_by(CD, year) %>%
  summarise(Population = sum(Population)) %>%
  ungroup() %>%
  inner_join(CONGRESS)

popRaceSexAge_all_2011 <- get_acs(geography = "congressional district", 
                                  variables = popRaceSexAge_all, 
                                  year = 2011, state = 42) %>%
  left_join(var_names) %>%
  mutate(year = 2011)

#Purposefully pulling 2015 data for 2015 for later analysis
popRaceSexAge_all_2016 <- get_acs(geography = "congressional district", 
                                  variables = popRaceSexAge_all, 
                                  year = 2016, state = 42) %>%
  left_join(var_names) %>%
  mutate(year = 2015)

#Population by Race and Sex, by year

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
    RACE = case_when(
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
  select(c(CD, SEX, RACE, AGE_CAT, year, Population)) %>%
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

#no sex
population_byRaceAgeCD_byCongress2 <- population_byRaceSexAgeCD_byCongress2 %>%
  group_by(CD, RACE, AGE_CAT_EDUC, CONGRESS2) %>%
  summarise(Population = sum(Population, na.rm = TRUE))

population_byRace2SexAgeCD_byCongress2 <- inner_join(population_byRaceSexAgeCD_byCongress2, RACE2) %>%
  group_by(CD, SEX, RACE2, AGE_CAT_EDUC, CONGRESS2) %>%
  summarise(Population = sum(Population, na.rm = TRUE))

################################################################################
    
popEducSexAge_all_2011 <- get_acs(geography = "congressional district", 
                                  variables = popEducSexAge_all, 
                                  year = 2011, state = 42) %>%
  left_join(var_names) %>%
  mutate(year = 2011)

popEducSexAge_all_2016 <- get_acs(geography = "congressional district", 
                                  variables = popEducSexAge_all, 
                                  year = 2016, state = 42) %>%
  left_join(var_names) %>%
  mutate(year = 2015)

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
  select(c(CD, SEX, EDUC, AGE_CAT_EDUC, year, Population)) %>%
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

#no sex
population_byEducAgeCD_byCongress2 <- population_byEducSexAgeCD_byCongress2 %>%
  group_by(CD, CONGRESS2, AGE_CAT_EDUC, EDUC) %>%
  summarise(Population = sum(Population, na.rm = TRUE))
