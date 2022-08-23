{
  ## Dependencies
  library(shiny)
  library(leaflet)
  library(plotly)
  library(dplyr)
  library(tidyverse)
  library(highcharter)
  library(sf)
  library(RColorBrewer)
  
  ## Helpers (for now you can just source files but eventually refactor to modules is best for maintainability)
  source("R/helpers/plot_imr_race.R")
  source("R/helpers/plot_dod.R")
}

ui <- fluidPage(
  tags$head(tags$style(HTML(".leaflet-container {background: none;}"))),
  tags$head(includeCSS("CSS/Header.css")),
  tags$head(includeCSS("CSS/NavbarPage.css")),
  tags$head(includeCSS("CSS/Home.css")),
  tags$head(includeHTML("HTML/FontAwesomeLoader.html")),
  includeHTML("HTML/Header.html"),
  navbarPage( id = 'navbar',
              title = "Title",
              tabPanel("Project Details",includeHTML("HTML/Home.html")),
              tabPanel("Maps",
                       headerPanel(''),
                       sidebarPanel(
                         selectInput(c("Infant Mortality Rate", "Deaths of Despair Mortality Rate"), 
                                     inputId = "Measure_Maps", label = "Select a measure"),
                         selectInput(choices = c("111 - 112 (2010 - 2012)", "113 - 114 (2013 - 2015)", "116 (2019 - 2020)*"), 
                                     inputId = "Congress_Maps", label = "Select a Congress"),
                         h6("* When 116 (2019 - 2020) is selected as the congress boundary, the map shows Congress 116 boundaries
                            using data from 2013-2015."),
                         h4("Mortality Rates:"),
                         h6("A mortality rate is a measure of the number of deaths in a particular population,
                            scaled to the size of that population. Infant Mortality Rate (IMR) calculates the 
                            number of premature deaths in a given population divided by the number of live births in that same population.
                            It is then multiplied by 1,000, as it is reported per 1,000 live births. Deaths of Despair Mortality Rate
                            calculates the number of deaths of despair in a given population by the population of that same population, multiplied by
                            10,000, as it is reported per 10,000 people.")),
                       mainPanel(
                         leafletOutput('Maps'))),
              tabPanel("Charts",
                       headerPanel(''),
                       sidebarPanel(
                         selectInput(c("Infant Mortality Rate", "Deaths of Despair Mortality Rate"), 
                                     inputId = "Measure_Charts", label = "Select a measure"),
                         selectInput(choices = c("111 - 112 (2010 - 2012)", "113 - 114 (2013 - 2015)", "116 (2019 - 2020)*"), 
                                     inputId = "Congress_Charts", label = "Select a congress"),
                         selectInput(choices = NULL, inputId = "Displayed", label = "Values displayed"),
                         selectInput(choices = NULL, inputId = "Subgroup", label = "Select a subgrouping**"),
                         selectInput(choices = NULL, inputId = "AgeGroup", label = "Select an age group"),
                         h6("* When 116 (2019 - 2020) is selected as the congress boundary, the charts show the selected measures 
                         calculated using Congress 116 boundaries with data from 2013-2015."),
                         h6("** The ACS does not make data available by race-ethnicity at the census tract level, so we use race alone as a subgrouping when looking
                            at DOD mortality rates. Due to smaller group sizes, we also provide the option of looking at DOD mortaity rates in white/non-white populations.")),
                       mainPanel(
                         plotlyOutput('Lollipop', height = 650)
                       )),
              tabPanel("Redistricting",
                       headerPanel(''),
                       sidebarPanel(
                         h1("2011 PA Redistricting"),
                         h6("Following the decennial U.S. Census in 2010, Pennsylvania drew a new map that was approved at the end of 2011. 
                            This map went into effect for the 113th Congress, and the diagram to the right is meant to display how census tracts 
                            in Pennsylvania were re-allocated from their 111th-112th congressional districts (on the left) 
                            to their 113th-114th districts (on the right)."),
                         h6("Some of the congressional districts stayed very consistent between the two maps and others had many changes. 
                            For example, hovering your mouse over the thick black line for CD 14 shows that 242 census tracts contained in CD 14 
                            were still contained in CD 14 after the redistricting. Additionally, the thin black lines above and below the thick one, 
                            represent 3 tracts that moved to CD 12 and 3 tracts that moved to CD 18. Other districts, on the other hand, were completely 
                            split up after the redistricting. None of the tracts from CD 04 from the 111th and 112th congresses go to CD 04 in the 
                            113th congress; instead, we see that 45 tracts moved to CD 03, 117 tracts to CD 12, 17 tracts to CD 14, and 3 tracts to CD 18.")
                       ),
                       mainPanel(
                         highchartOutput('Sankey', width = 500, height = 650)
                       )),
  ),
  includeHTML("HTML/Footer.html")
)

server <- function(input, output, session) {
  
  #IMR map data
  load(file = "IMR_Maps_byCD_2.Rdata", envir=.GlobalEnv)
  
  #IMR map data
  load(file = "IMR_Maps_cd116.Rdata", envir=.GlobalEnv)
  
  #DOD map data
  load(file = "DOD_Maps_byCD_2.Rdata", envir=.GlobalEnv)
  
  #DOD map data
  load(file = "DOD_Maps_cd116.Rdata", envir=.GlobalEnv)
  
  #IMR plot data
  load(file = "IMR_byCD_byRace_2.Rdata", envir=.GlobalEnv)
  
  #IMR plot data - cd116
  load(file = "IMR_byCD_byRace_2_cd116.Rdata", envir=.GlobalEnv)
  
  #IMR plot data - disparities
  load(file = "IMR_byCD_byRace_absDisparity_2.Rdata", envir=.GlobalEnv)
  
  #IMR plot data - disparities - cd116
  load(file = "IMR_byCD_byRace_absDisparity_2_cd116.Rdata", envir=.GlobalEnv)
  
  #DOD plot data - by race
  load(file = "DOD_byRaceSexCD_2.Rdata", envir=.GlobalEnv)
  
  #DOD plot data - by race - cd116
  load(file = "DOD_byRaceSexCD_2_cd116.Rdata", envir=.GlobalEnv)
  
  #DOD plot data - by race - disparities
  load(file = "DOD_byRaceSexCD_absDisparity_2.Rdata", envir=.GlobalEnv)
  
  #DOD plot data - by race - disparities - cd116
  load(file = "DOD_byRaceSexCD_absDisparity_2_cd116.Rdata", envir=.GlobalEnv)
  
  #DOD plot data - by race - rolled up
  load(file = "DOD_byRace2SexCD_2.Rdata", envir=.GlobalEnv)
  
  #DOD plot data - by race - rolled up - cd116
  load(file = "DOD_byRace2SexCD_2_cd116.Rdata", envir=.GlobalEnv)
  
  #DOD plot data - by education
  load(file = "DOD_byEducSexCD_2.Rdata", envir=.GlobalEnv)
  
  #DOD plot data - by education - cd116
  load(file = "DOD_byEducSexCD_2_cd116.Rdata", envir=.GlobalEnv)
  
  #DOD plot data - by education - disparities
  load(file = "DOD_byEducSexCD_absDisparity_2.Rdata", envir=.GlobalEnv)
  
  #DOD plot data - by education - disparities - cd116
  load(file = "DOD_byEducSexCD_absDisparity_2_cd116.Rdata", envir=.GlobalEnv)
  
  #Sakney
  load(file = "sankey_111_113_full.Rdata", envir=.GlobalEnv)
  
  # Maps ------
  output$Maps <- renderLeaflet({
    imr_popup <- paste0("<strong>", filter(IMR_Maps_byCD_2, CONGRESS2 == input$Congress_Maps)$CD, 
                        "</strong>", "<br><strong>Infant Mortality Rate: </strong>", 
                        round(as.numeric(filter(IMR_Maps_byCD_2, CONGRESS2 == input$Congress_Maps)$IMR), 2))
    
    imr_colors <- colorNumeric(palette = "viridis", reverse = FALSE,
                               domain = as.numeric(filter(IMR_Maps_byCD_2, CONGRESS2 == input$Congress_Maps)$IMR))
    
    imr_colors_rev <- colorNumeric(palette = "viridis", reverse = TRUE,
                                   domain = as.numeric(filter(IMR_Maps_byCD_2, CONGRESS2 == input$Congress_Maps)$IMR))
    
    IMR_Maps_Interactive <- leaflet(filter(IMR_Maps_byCD_2, CONGRESS2 == input$Congress_Maps)) %>%
      addPolygons(
        stroke = TRUE,
        weight = 0.5,
        color = "black",
        fillColor = ~imr_colors(as.numeric(filter(IMR_Maps_byCD_2, CONGRESS2 == input$Congress_Maps)$IMR)),
        fillOpacity = 0.8,
        smoothFactor = 0.5,
        popup = imr_popup) %>%
      addLegend("topright", pal = imr_colors_rev, 
                #values = as.numeric(filter(IMR_Maps_byCD_2, CONGRESS2 == input$Congress_Maps)$IMR),
                values = IMR_Maps_byCD_2$IMR,
                title = "IMR, per 1,000 <br> Live Births", opacity = 1,
                labFormat = labelFormat(digits = 2, transform = function(x) {sort(x, decreasing = TRUE)}))
    
    imr_popup_cd116 <- paste0("<strong>", IMR_Maps_cd116$CD, 
                              "</strong>", "<br><strong>Infant Mortality Rate: </strong>", 
                              round(IMR_Maps_cd116$IMR, 2))
    
    imr_colors_cd116 <- colorNumeric(palette = "viridis", reverse = FALSE, domain = IMR_Maps_cd116$IMR)
    
    imr_colors_cd116_rev <- colorNumeric(palette = "viridis", reverse = TRUE, domain = IMR_Maps_cd116$IMR)
    
    IMR_Maps_Interactive_CD116 <- leaflet(IMR_Maps_cd116) %>%
      addPolygons(
        stroke = TRUE,
        weight = 0.5,
        color = "black",
        fillColor = ~imr_colors_cd116(IMR),
        fillOpacity = 0.8,
        smoothFactor = 0.5,
        popup = imr_popup_cd116) %>%
      addLegend("topright", pal = imr_colors_cd116_rev, values = IMR_Maps_cd116$IMR,
                title = "IMR, per 1,000 <br> Live Births",
                opacity = 1,
                labFormat = labelFormat(digits = 2, transform = function(x) sort(x, decreasing = TRUE)))
    
    dod_popup <- paste0("<strong>", filter(DOD_Maps_byCD_2, CONGRESS2 == input$Congress_Maps)$CD, 
                        "</strong>", "<br><strong>DOD Mortality Rate: </strong>", 
                        round(filter(DOD_Maps_byCD_2, CONGRESS2 == input$Congress_Maps)$MR,2))
    
    dod_colors <- colorNumeric(palette = "viridis", reverse = FALSE,
                               domain = filter(DOD_Maps_byCD_2, CONGRESS2 == input$Congress_Maps)$MR)
    
    dod_colors_rev <- colorNumeric(palette = "viridis", reverse = TRUE,
                                   domain = filter(DOD_Maps_byCD_2, CONGRESS2 == input$Congress_Maps)$MR)
    
    DOD_Maps_Interactive <- leaflet(filter(DOD_Maps_byCD_2, CONGRESS2 == input$Congress_Maps)) %>%
      addPolygons(
        stroke = TRUE,
        weight = 0.5,
        color = "black",
        fillColor = ~dod_colors(MR),
        fillOpacity = 0.8,
        smoothFactor = 0.5,
        popup = dod_popup) %>%
      addLegend("topright", pal = dod_colors_rev, values = DOD_Maps_byCD_2$MR,
                title = "DOD MR <br> per 10,000",
                opacity = 1,
                labFormat = labelFormat(digits = 2, transform = function(x) sort(x, decreasing = TRUE)))
    
    dod_popup_cd116 <- paste0("<strong>", DOD_Maps_cd116$CD, 
                              "</strong>", "<br><strong>DOD Mortality Rate: </strong>", 
                              round(DOD_Maps_cd116$MR, 2))
    
    dod_colors_cd116 <- colorNumeric(palette = "viridis", reverse = FALSE, domain = DOD_Maps_cd116$MR)
    
    dod_colors_cd116_rev <- colorNumeric(palette = "viridis", reverse = TRUE, domain = DOD_Maps_cd116$MR)
    
    DOD_Maps_Interactive_CD116 <- leaflet(DOD_Maps_cd116) %>%
      addPolygons(
        stroke = TRUE,
        weight = 0.5,
        color = "black",
        fillColor = ~ dod_colors_cd116(MR),
        fillOpacity = 0.8,
        smoothFactor = 0.5,
        popup = dod_popup_cd116) %>%
      addLegend("topright", pal = dod_colors_cd116_rev, values = DOD_Maps_cd116$MR,
                title = "DOD MR <br> per 10,000",
                opacity = 1,
                labFormat = labelFormat(digits = 2, transform = function(x) sort(x, decreasing = TRUE)))
    
    if(input$Congress_Maps != "116 (2019 - 2020)*" && input$Measure_Maps == "Infant Mortality Rate") {map <- IMR_Maps_Interactive}
    if(input$Congress_Maps != "116 (2019 - 2020)*" && input$Measure_Maps == "Deaths of Despair Mortality Rate") {map <- DOD_Maps_Interactive}
    if(input$Congress_Maps == "116 (2019 - 2020)*" && input$Measure_Maps == "Infant Mortality Rate") {map <- IMR_Maps_Interactive_CD116}
    if(input$Congress_Maps == "116 (2019 - 2020)*" && input$Measure_Maps == "Deaths of Despair Mortality Rate") {map <- DOD_Maps_Interactive_CD116}
    map
  })
  

  #Color themes -----
  
  ## For this type of data you should utilize lists! it keeps everything together and easily accessible. 
  colors = list()
  colors$AAPI <- "#50bde9"
  colors$AmericanIndian <- "#009e74"
  colors$Black <- "#d55e00"
  colors$Hispanic <- "#cc79a7"
  colors$White <- "#f0e442"
  
  colors$LessHS <- "#cc79a7"
  colors$HS <- "#009e73"
  colors$SomeCollege <- "#0072b2"
  colors$Bachelor <- "#f0e442"
  
  # Charts Input updater----
  
  toListen_subgroup <- reactive({list(input$Measure_Charts,input$Displayed) })
  observeEvent(toListen_subgroup(), {
    req(input$Measure_Charts )
    req(input$Displayed )
    
    ## Dev logs
    print("------------------")
    print("UPDATE  Subgroup!!!!!!!!")
    print(input$Measure_Charts)
    print(input$Displayed)
    print(input$Subgroup)
    print("------------------")
    
    ## Update subgroup
    choices_list_subgroup = case_when(
      input$Measure_Charts == "Infant Mortality Rate" ~ list(c("Race/Ethnicity")),
      input$Measure_Charts == "Deaths of Despair Mortality Rate" && input$Displayed == "Mortality Rates" ~ list(c("Race", "Race (Rolled-Up)", "Education")),
      input$Measure_Charts == "Deaths of Despair Mortality Rate" && input$Displayed == "Absolute Disparities" ~ list(c("Race", "Education")))
    updateSelectInput(inputId = "Subgroup", choices =  unlist(choices_list_subgroup))
    
    ## Update ageGroup
    choices_list_ageGroup = case_when(
      input$Measure_Charts == "Infant Mortality Rate" ~ list(c("All ages groups combined")),
      input$Measure_Charts == "Deaths of Despair Mortality Rate" ~ list(c("25 to 34 years", "35 to 44 years", "45 to 64 years")))
    updateSelectInput(inputId = "AgeGroup", choices =  unlist(choices_list_ageGroup))
    
  })
  
  # toListen_ageGroup <- reactive({list(input$Measure_Charts) })
  # observeEvent(toListen_ageGroup(), {
  #   print('UDPATE AgeGroup!!!!!!!!!!!!!!!!')
  #   req(input$Measure_Charts )
  #   choices_list_ageGroup = case_when(
  #     input$Measure_Charts == "Infant Mortality Rate" ~ list(c("All ages groups combined")),
  #     input$Measure_Charts == "Deaths of Despair Mortality Rate" ~ list(c("25 to 34 years", "35 to 44 years", "45 to 64 years")))
  #   updateSelectInput(inputId = "AgeGroup", choices =  unlist(choices_list_ageGroup))
  # })
  
  displayed <- reactive({
    choices <- case_when(
      input$Subgroup == "Race (Rolled-Up)" ~ c("Mortality Rates"),
      TRUE ~ c("Mortality Rates", "Absolute Disparities")
    )
  })
  
  observeEvent(displayed(), {
    print('UDPATE Displayed!!!!!!!!!!!!!!!!')
    choices <- displayed()
    updateSelectInput(inputId = "Displayed", choices = choices)
  })
  
  
  # Lollipop -----
  output$Lollipop <- renderPlotly({
    if((input$Measure_Charts == "Infant Mortality Rate") &
       (input$Displayed == "Mortality Rates") &
       (input$Congress_Charts != "116 (2019 - 2020)*")) {filename <-
         plot_imr_race(filter(IMR_byCD_byRace_2, as.integer(RACEHISP) < 6, IMR > 0), colors, input$Congress_Charts, input$Displayed)}
    if((input$Measure_Charts == "Infant Mortality Rate") &
       (input$Displayed == "Mortality Rates") &
       (input$Congress_Charts == "116 (2019 - 2020)*")) {filename <-
         plot_imr_race(filter(IMR_byCD_byRace_2_cd116, as.integer(RACEHISP) < 6, IMR > 0, as.integer(CONGRESS2) == 2), colors, input$Congress_Charts, input$Displayed)}
    if((input$Measure_Charts == "Infant Mortality Rate") &
       (input$Displayed == "Absolute Disparities") &
       (input$Congress_Charts != "116 (2019 - 2020)*")) {filename <-
         plot_imr_race(filter(IMR_byCD_byRace_absDisparity_2, as.integer(RACEHISP) != 2, as.integer(RACEHISP) < 6), colors, input$Congress_Charts, input$Displayed)}
    if((input$Measure_Charts == "Infant Mortality Rate") &
       (input$Displayed == "Absolute Disparities") &
       (input$Congress_Charts == "116 (2019 - 2020)*")) {filename <-
         plot_imr_race(filter(IMR_byCD_byRace_absDisparity_2_cd116, as.integer(RACEHISP) != 2, as.integer(RACEHISP) < 6, as.integer(CONGRESS2) == 2), colors, input$Congress_Charts, input$Displayed)}
    if((input$Measure_Charts == "Deaths of Despair Mortality Rate") &
       (input$Subgroup == "Race") &
       (input$Displayed == "Mortality Rates") &
       (input$Congress_Charts != "116 (2019 - 2020)*")) {filename <-
         plot_dod(filter(DOD_byRaceSexCD_2, MR > 0, as.integer(RACE) < 5), colors, input$Congress_Charts, input$Subgroup, input$Displayed, input$AgeGroup)}
    if((input$Measure_Charts == "Deaths of Despair Mortality Rate") &
       (input$Subgroup == "Race") &
       (input$Displayed == "Mortality Rates") &
       (input$Congress_Charts == "116 (2019 - 2020)*")) {filename <-
         plot_dod(filter(DOD_byRaceSexCD_2_cd116, MR > 0, as.integer(RACE) < 5, as.integer(CONGRESS2) == 2), colors, input$Congress_Charts, input$Subgroup, input$Displayed, input$AgeGroup)}
    if((input$Measure_Charts == "Deaths of Despair Mortality Rate") &
       (input$Subgroup == "Race") &
       (input$Displayed == "Absolute Disparities") &
       (input$Congress_Charts != "116 (2019 - 2020)*")) {filename <-
         plot_dod(filter(DOD_byRaceSexCD_absDisparity_2, as.integer(RACE) != 1, as.integer(RACE) < 5), colors, input$Congress_Charts, input$Subgroup, input$Displayed, input$AgeGroup)}
    if((input$Measure_Charts == "Deaths of Despair Mortality Rate") &
       (input$Subgroup == "Race") &
       (input$Displayed == "Absolute Disparities") &
       (input$Congress_Charts == "116 (2019 - 2020)*")) {filename <-
         plot_dod(filter(DOD_byRaceSexCD_absDisparity_2_cd116, as.integer(RACE) != 1, as.integer(RACE) < 5, as.integer(CONGRESS2) == 2), colors, input$Congress_Charts, input$Subgroup, input$Displayed, input$AgeGroup)}
    if((input$Measure_Charts == "Deaths of Despair Mortality Rate") &
       (input$Subgroup == "Race (Rolled-Up)") &
       (input$Congress_Charts != "116 (2019 - 2020)*")) {filename <-
         plot_dod(filter(DOD_byRace2SexCD_2, MR > 0), colors, input$Congress_Charts, input$Subgroup, input$Displayed, input$AgeGroup)}
    if((input$Measure_Charts == "Deaths of Despair Mortality Rate") &
       (input$Subgroup == "Race (Rolled-Up)") &
       (input$Congress_Charts == "116 (2019 - 2020)*")) {filename <-
         plot_dod(filter(DOD_byRace2SexCD_2_cd116, MR > 0, as.integer(CONGRESS2) == 2), colors, input$Congress_Charts, input$Subgroup, input$Displayed, input$AgeGroup)}
    if((input$Measure_Charts == "Deaths of Despair Mortality Rate") &
       (input$Subgroup == "Education") &
       (input$Displayed == "Mortality Rates") &
       (input$Congress_Charts != "116 (2019 - 2020)*")) {filename <- plot_dod(filter(DOD_byEducSexCD_2, MR > 0, as.integer(EDUC) < 5), colors, input$Congress_Charts, input$Subgroup, input$Displayed, input$AgeGroup)}
    if((input$Measure_Charts == "Deaths of Despair Mortality Rate") &
       (input$Subgroup == "Education") &
       (input$Displayed == "Mortality Rates") &
       (input$Congress_Charts == "116 (2019 - 2020)*")) {filename <-
         plot_dod(filter(DOD_byEducSexCD_2_cd116, MR > 0, as.integer(EDUC) < 5, as.integer(CONGRESS2) == 2), colors, input$Congress_Charts, input$Subgroup, input$Displayed, input$AgeGroup)}
    if((input$Measure_Charts == "Deaths of Despair Mortality Rate") &
       (input$Subgroup == "Education") &
       (input$Displayed == "Absolute Disparities") &
       (input$Congress_Charts != "116 (2019 - 2020)*")) {filename <-
         plot_dod(filter(DOD_byEducSexCD_absDisparity_2, as.integer(EDUC) != 4, as.integer(EDUC) < 5), colors, input$Congress_Charts, input$Subgroup, input$Displayed, input$AgeGroup)}
    if((input$Measure_Charts == "Deaths of Despair Mortality Rate") &
       (input$Subgroup == "Education") &
       (input$Displayed == "Absolute Disparities") &
       (input$Congress_Charts == "116 (2019 - 2020)*")) {filename <-
         plot_dod(filter(DOD_byEducSexCD_absDisparity_2_cd116, as.integer(EDUC) != 4, as.integer(EDUC) < 5, as.integer(CONGRESS2) == 2), colors, input$Congress_Charts, input$Subgroup, input$Displayed, input$AgeGroup)}
    filename
  })

  output$Sankey <- renderHighchart({
    hchart(sankey_111_113_full, "sankey", name = "Tract Count")
  })
}

shinyApp(ui, server)

