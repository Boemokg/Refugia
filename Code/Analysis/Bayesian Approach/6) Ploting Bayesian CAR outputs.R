

#loading files saved from Script 1
Total <- readRDS(file = "Data/TotalCARBAYES")
Nre <- readRDS(file = "Data/NRECARBAYES")
Wet <- readRDS(file = "Data/WetCARBAYES")

bayesM = rbind(Total,Nre, Wet)


#reordering the levels 

bayesM$Model_Type = factor(bayesM$Model_Type, levels = c("Total species richness", 
                                                "Narrow-range endemics", 
                                                "Wetland-dependent NRE"))

#plotting

bayesmodel =ggplot(bayesM, aes(x = Covariates, y = Mean))+
  geom_pointrange(aes(ymin = `2.5%`, ymax = `97.5%`),data = bayesM, size =2)+
  geom_hline(yintercept = 0, linetype = 2, size = 2)+
  coord_flip()+
  facet_grid(. ~ Model_Type)+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 35),
        axis.title = element_text(size = 50, face = "bold", vjust = 1),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))+
  geom_point(colour = 'black', size = 5)


ggsave("Output/Bayesian models/Fig.4.png", bayesmodel, width = 80, height = 60, units = "cm")
