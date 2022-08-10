#'  This function will create ggplotly for the imr_race
#'   
#'  This is just your code from imr_race(). WIth the following changes: 1) I import a color object which was initialized
#'  in the app ( you should probably outsource this to an rdata earlier in your data pipe-line) 2) scale_color_manual() was
#'  used instead of color_scale$.... 
#'   
#'  @param colors: this is the color object which I initialized. Try to use list data structure rather as its much easier
#'  to organize and access values. 
#'  @param Congress_Charts_tmp: This just takes the input$Congress_Charts as an argument.
#'  
#'  

plot_dod = function(data, colors, Congress_Charts_tmp, Subgroup_tmp, Displayed_tmp, AgeGroup_tmp) {
  # Congress_Charts_tmp = "111 - 112 (2009 - 2012)"
  if (Congress_Charts_tmp != "116 (2019 - 2020)*") {filtered_data <- filter(data,  CONGRESS2 == Congress_Charts_tmp, AGE_CAT_EDUC == AgeGroup_tmp)}
  if (Congress_Charts_tmp == "116 (2019 - 2020)*") {filtered_data <- filter(data,  AGE_CAT_EDUC == AgeGroup_tmp)}
  if(Subgroup_tmp == "Race") {aes_color <- filtered_data$RACE}
  if(Subgroup_tmp == "Race (Rolled-Up)") {aes_color <- filtered_data$RACE2}
  if(Subgroup_tmp == "Education") {aes_color <- filtered_data$EDUC}
  if(Displayed_tmp !="Absolute Disparities") 
    {trans_tmp <- scale_y_continuous(trans = "log10", breaks = c(1,10,100,1000), limits=c(.1, NA), labels = c(1,10,100,1000))
     axis_title <- "Mortality Rate, per 10,000 People"}
  if(Displayed_tmp =="Absolute Disparities") 
    {trans_tmp <- scale_y_continuous(trans = "identity")
    axis_title <- "Absolute Disparity in Mortality Rate, per 10,000 People"}
  ggplotly(ggplot(filtered_data, aes(x = CD, y = MR, group = paired, text = paste0("CD: ", CD, "<br>MR: ", round(MR, 2),
                                                                                   "<br>", Subgroup_tmp, ": ", aes_color))) +
             geom_line() +
             geom_point(size = 3, aes(color = aes_color)) +
             scale_color_manual(values = c("Asian American Pacific Islander" = colors$AAPI,
                                           "Black" = colors$Black, "White" = colors$White,
                                           "Non-White" = colors$Black,
                                           "Less than High School" = colors$LessHS, "High School" = colors$HS, 
                                           "Some College/Associate Degree" = colors$SomeCollege, 
                                           "Bachelor/Master/Doctorate/Professional Degree" = colors$Bachelor)) +
             ggtitle(paste0("DOD Mortality Rates - ", Congress_Charts_tmp)) +
             xlab("Congressional District") +
             ylab(axis_title) +
             labs(color = Subgroup_tmp) +
             facet_wrap(~ SEX) +
             theme_bw() +
             theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 15), axis.text = element_text(size = 10),
                   axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)), 
                   legend.position = "bottom", legend.text=element_text(size=10), strip.text=element_text(size=12)) +
             guides(color = guide_legend(nrow = 2)) +
             trans_tmp +
             scale_x_discrete(expand=expansion(mult=c(0.1, 0.05)))+
             annotation_logticks(sides="b") +
             coord_flip(),
           tooltip = "text") %>%
    layout(legend = list(
      orientation = "h",
      xanchor = "center",
      x = 0.5,
      y = -0.25),
      height = 650,
      title = list(y = 0.999))
}


