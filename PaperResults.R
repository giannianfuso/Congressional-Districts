
#All results come from BirthDeath_Analysis.R

#Figure 1 - Map of IMR by CD
figure1 <- IMR_Maps_2
figure1
ggsave("./Final Results/figure1.pdf", plot = figure1, width = 9, height = 12.5)

#Figure 2 - IMR by race/ethnicity
figure2 <- IMR_byCD_byRace_Plots_2
figure2
ggsave("./Final Results/figure2.pdf", plot = figure2, width = 7, height = 9)

#Figure 2a - IMR by race/ethnicity
figure2a <- IMR_byCD_byRace_Plots_2_panels[[1]]
figure2a
ggsave("./Final Results/figure2a.pdf", plot = figure2a, width = 7, height = 5)

#Figure 2b - IMR by race/ethnicity
figure2b <- IMR_byCD_byRace_Plots_2_panels[[2]]
figure2b
ggsave("./Final Results/figure2b.pdf", plot = figure2b, width = 7, height = 5)

#Figure 3 - Map of DoD MR by CD
figure3 <- DoD_Maps_2
figure3
ggsave("./Final Results/figure3.pdf", plot = figure3, width = 9, height = 12.5)

#Figure 4 - DoD by education
figure4a <- DoD_byEducAge_Plot_panels[[1]]
figure4a
ggsave("./Final Results/figure4a.pdf", figure4a, width = 7, height = 5)

figure4b <- DoD_byEducAge_Plot_panels[[2]]
figure4b
ggsave("./Final Results/figure4b.pdf", figure4b, width = 7, height = 5)

figure4 <- DoD_byEducAge_Plot
figure4
ggsave("./Final Results/figure4.pdf", figure4, width = 7, height = 9)

appendix_figure1 <- sankey
appendix_figure1
ggsave("./Final Results/appendix_figure1.pdf", plot = appendix_figure1, width = 8.5, height = 11)

appendix_figure3 <- IMR_DoD_Scatter
appendix_figure3
ggsave("./Final Results/appendix_figure3.pdf", plot = appendix_figure3, width = 8, height = 8)

