
#Figure 1 - Map of IMR rates over CD's
figure1 <- IMR_Maps_2
figure1
ggsave("./Final Results/figure1.pdf", plot = figure1, width = 8.5, height = 11)
ggsave("./Final Results/figure1.png")

#Figure 2 - Map of overall DOD over CD'2 -- by overall, do we mean MR?
figure2 <- DOD_Maps_2
figure2
ggsave("./Final Results/figure2.pdf", plot = figure2, width = 8.5, height = 11)
ggsave("./Final Results/figure2.png")

#Figure 3 - IMR by race/ethnicity
figure3 <- IMR_byCD_byRace_Plots_2
figure3
ggsave("./Final Results/figure3.pdf", plot = IMR_byCD_byRace_Plots_2, width = 11, height = 11)
ggsave("./Final Results/figure3.png", plot = figure3, width = 13, height = 13)

#Figure 4 - DOD by education - faceting by age, sex, and congress, cd111-112
figure4 <- DOD_byEducSexAge_Plot
figure4
ggsave("./Final Results/figure4.pdf", figure4, width = 15, height = 13)
ggsave("./Final Results/figure4.png", figure4, width = 30, height = 20)

appendix_figure1 <- sankey
appendix_figure1
ggsave("./Final Results/appendix_figure1.pdf", plot = appendix_figure1, width = 8.5, height = 11)
ggsave("./Final Results/appendix_figure1.png", plot = appendix_figure1, width = 8.5, height = 11)

appendix_figure2 <- IMR_DOD_Scatter
ggsave("./Final Results/appendix_figure2.pdf", plot = appendix_figure2, width = 8, height = 8)
ggsave("./Final Results/appendix_figure2.png", plot = appendix_figure2, width = 8, height = 6)
