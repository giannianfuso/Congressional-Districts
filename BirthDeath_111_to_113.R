library(classInt)

################################################################################

#Birth data from 2013-2015 with 111/112 boundaries
birth_113_data_111 <- birth_original %>%
  filter(year == 2013 | year == 2014 | year == 2015) %>%
  inner_join(tract_to_cd111) %>%
  rename(CD = cd111, ALLOCATION = afact111) %>%
  birth_mutations_cd() %>%
  mutate(CONGRESS2 = '111 - 112 (2010 - 2012)') %>%
  rbind({
    birth_cd %>%
      filter(CONGRESS2 == '113 - 114 (2013 - 2015)') 
  })

#Birth data from 2013-2015 with 111/112 boundaries
death_113_data_111 <- death_original %>%
  filter(year == 2013 | year == 2014 | year == 2015) %>%
  inner_join(tract_to_cd111) %>%
  rename(CD = cd111, ALLOCATION = afact111) %>%
  death_mutations_cd() %>%
  mutate(CONGRESS2 = '111 - 112 (2010 - 2012)') %>%
  rbind({
    death_cd %>%
      filter(CONGRESS2 == '113 - 114 (2013 - 2015)')
  })

#Tract-level popultaion data for 2010-2012 and allocate to 113 boundaries
pop_25_64_113_data_111 <- get_acs(geography = "tract", 
                           variables = pop_25_64, 
                           year = 2016, state = 42) %>%
  left_join(var_names) %>%
  rename(Population = estimate, GEOID10 = GEOID) %>%
  mutate(
    GEOID10 = as.integer64(GEOID10),
    Population = Population * 3,
    SEX = case_when(
      grepl("Male", label, fixed = TRUE) == TRUE ~ 1,
      grepl("Female", label, fixed = TRUE) == TRUE ~ 2) %>%
      factor(levels = c(1,2),
             labels = c("Male", "Female"))) %>%
  group_by(GEOID10) %>%
  summarise(Population = sum(Population)) %>%
  inner_join(tract_to_cd111) %>%
  rename(CD = cd111) %>%
  mutate(
    GEOID10 = as.numeric(GEOID10),
    pop_111 = Population*afact111,
    CD = factor(CD, 
                levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),
                labels = c("CD 01", "CD 02", "CD 03", "CD 04", "CD 05",
                           "CD 06", "CD 07", "CD 08", "CD 09", "CD 10",
                           "CD 11", "CD 12", "CD 13", "CD 14", "CD 15",
                           "CD 16", "CD 17", "CD 18", "CD 19"))) %>%
  group_by(CD) %>%
  summarise(Population = sum(pop_111)) %>%
  arrange(CD) %>%
  mutate(CONGRESS2 = '111 - 112 (2010 - 2012)') %>%
  relocate(CONGRESS2, .before = Population) %>%
  rbind({
    population_pop_25_64_byCD %>%
      filter(CONGRESS2 == '113 - 114 (2013 - 2015)') %>%
      group_by(CD, CONGRESS2) %>%
      summarise(Population = sum(Population))
  })

pop_totals <- pop_25_64_113_data_111 %>%
  group_by(CONGRESS2) %>%
  summarise(Population = sum(Population))


################################################################################
####################################IMR#########################################
################################################################################

#Infant Mortality by CD - combined Congresses
infMort_byCD_113_data_111 <- group_by(death_113_data_111, CONGRESS2, CD) %>%
  summarise(InfantMortality = sum(INFMORT_CD, na.rm = TRUE))

#Live Births by CD - combined Congresses
liveBirths_byCD_113_data_111 <- group_by(birth_113_data_111, CONGRESS2, CD) %>%
  summarise(LiveBirths = sum(ALLOCATION))

#IMR by CD - combined Congresses - 116
IMR_byCD_113_data_111 <- inner_join(infMort_byCD_113_data_111, liveBirths_byCD_113_data_111) %>%
  mutate(IMR = InfantMortality/LiveBirths * 1000) %>%
  ungroup() %>%
  mutate(jenks=cut(IMR, breaks=classIntervals(IMR,n=5,style="jenks")$brks,include.lowest=T)) %>%
  inner_join(cd_outlines_2) %>%
  select(-CONGRESS)

#Write to CSV - isolate 111/112 boundaties
write.csv(IMR_byCD_113_data_111%>%select(-c(geometry))
          %>%filter(CONGRESS2 == "111 - 112 (2010 - 2012)"), "./111 to 113/IMR_byCD_113_data_111.csv", row.names = F)

#Write to CSV - isolate 113/114 boundaties
write.csv(IMR_byCD_113_data_111%>%select(-c(geometry))
          %>%filter(CONGRESS2 == "113 - 114 (2013 - 2015)"), "./111 to 113/IMR_byCD_113_data_113.csv", row.names = F)

#Maps
IMR_Maps_113_data_111 <- ggplot(IMR_byCD_113_data_111) +
  geom_sf(aes(fill = jenks, geometry = geometry), lwd = .1) +
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
  labs(fill = "Infant Mortality Rate, \nper 1,000 Live Births")
IMR_Maps_113_data_111

ggsave("./111 to 113/IMR_Maps_113_data_111.pdf", plot = IMR_Maps_113_data_111, width = 9, height = 10)

IMR_113_data_111_summary <- IMR_byCD_113_data_111 %>%
  group_by(CONGRESS2) %>%
  summarise(mean = mean(IMR), sd = sd(IMR)) %>%
  mutate(cv = sd/mean*100, CONGRESS2 = as.character(CONGRESS2)) %>%
  pivot_longer(cols = -CONGRESS2) %>%
  pivot_wider(names_from = CONGRESS2, values_from = value)

#Write to CSV
write.csv(IMR_113_data_111_summary, "./111 to 113/IMR_113_data_111_summary.csv", row.names = F)

################################################################################
####################################DoD#########################################
################################################################################

#DoD by CD - combined Congresses
DOD_byCD_113_data_111 <- group_by(death_113_data_111, CONGRESS2, CD) %>%
  summarise(DOD = sum(DESPAIR_CD, na.rm = TRUE)) %>%
  inner_join(pop_25_64_113_data_111) %>%
  mutate(MR = ifelse(Population == 0, 0, DOD/Population*10000)) %>%
  filter(!is.na(Population)) %>%
  ungroup() %>%
  mutate(jenks=cut(MR, breaks=classIntervals(MR,n=5,style="jenks")$brks,include.lowest=T)) %>%
  inner_join(cd_outlines_2)%>%
  select(-c(CONGRESS))

#Write to CSV - isolate 111/112 boundaties
write.csv(DOD_byCD_113_data_111%>%select(-c(geometry))
          %>%filter(CONGRESS2 == "111 - 112 (2010 - 2012)"), "./111 to 113/DOD_byCD_113_data_111.csv", row.names = F)

#Write to CSV - isolate 113/114 boundaties
write.csv(DOD_byCD_113_data_111%>%select(-c(geometry))
          %>%filter(CONGRESS2 == "113 - 114 (2013 - 2015)"), "./111 to 113/DOD_byCD_113_data_113.csv", row.names = F)

#Maps
DoD_Maps_113_data_111 <- ggplot(DOD_byCD_113_data_111) +
  geom_sf(aes(fill = jenks, geometry = geometry), lwd = .1) +
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
DoD_Maps_113_data_111

ggsave("./111 to 113/DoD_Maps_113_data_111.pdf", plot = DoD_Maps_113_data_111, width = 9, height = 10)

DoD_113_data_111_summary <- DOD_byCD_113_data_111 %>%
  group_by(CONGRESS2) %>%
  summarise(mean = mean(MR), sd = sd(MR)) %>%
  mutate(cv = sd/mean*100, CONGRESS2 = as.character(CONGRESS2)) %>%
  pivot_longer(cols = -CONGRESS2) %>%
  pivot_wider(names_from = CONGRESS2, values_from = value)

#Write to CSV
write.csv(DoD_113_data_111_summary, "./111 to 113/DoD_113_data_111_summary.csv", row.names = F)
