#'  This function will create ggplotly for IMR
#'  @param data:   
#'  @param colors:  
#'  @param Congress_Charts_tmp: 
#'  @param Displayed_tmp:  

plot_imr_race = function(data, colors,  Congress_Charts_tmp, Displayed_tmp) {
  if (Congress_Charts_tmp != "116 (2019 - 2020)*") {data <- filter(data, CONGRESS2 == Congress_Charts_tmp)}
  if(Displayed_tmp !="Absolute Disparities") 
  {axis_title <- "Mortality Rate, per 10,000 People"}
  if(Displayed_tmp =="Absolute Disparities") 
  {axis_title <- "Absolute Disparity in Mortality Rate, per 1,000 Live Births"}
  
  ggplotly(ggplot(data, aes(x = CD, y = IMR, group = paired, 
                                   text = paste0("CD: ", substr(CD, 3, length(CD)), "<br>IMR: ", round(IMR, 2),
                                               "<br>Race/Hispanic Origin: ", RACEHISP))) +
             geom_line() +
             geom_point(size = 3, aes(color = RACEHISP)) +
             scale_color_manual(values = c("Hispanic" = colors$Hispanic,
                                           "Non-Hispanic Asian American Pacific Islander" = colors$AAPI,
                                           "Non-Hispanic Black" = colors$Black, "Non-Hispanic White" = colors$White)) +
             theme_bw() +
             theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 15), axis.text = element_text(size = 10),
                   legend.position = "bottom") +
             guides(color = guide_legend(nrow = 2)) +
             ggtitle(paste0("IMR - ", Congress_Charts_tmp)) +
             xlab("Congressional District") +
             ylab(axis_title) +
             labs(color = "Race/Hispanic Origin") +
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
