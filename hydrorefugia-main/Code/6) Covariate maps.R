#Plot the maps of reponses and covariates 


library(tidyverse)
library(readxl)
library(readr)
library(sf)
library(units)
library(terra)
library(mapview)
library(viridis)
library(tidyr)
library(ggpubr)
library(dplyr)
library(corrplot)
library(stargazer)


#read data set
CFR_data <- readRDS("Full_CFR")




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------Species distributions------------------------

##############################################################################

#Joining the 3 datasets 
#Spatial_heat_data = CFR_data %>% 
  #dplyr:: select(Total_species_richness, Narrow_Range_endemics, Facultative_species, Obligate_species)


#pivot longs 
# Using pivot_longer to convert separate columns back to a single categorical column
#Spatial_heat_data.1 <- tidyr::pivot_longer(Spatial_heat_data, cols = c(Total_species_richness, Narrow_Range_endemics), 
                                           #names_to = "group", values_to = "Total & Narrow range spp count")

# Using pivot_longer to convert separate columns back to a single categorical column
#Spatial_heat_data.2 <- tidyr::pivot_longer(Spatial_heat_data, cols = c(Facultative_species, Obligate_species), 
                                           #names_to = "group", values_to = " Wetland spp count")



#Plot heatmaps using facetwrap
#Wetland <- ggplot() + geom_sf(data = Spatial_heat_data.2, aes(fill =` Wetland spp count`, colour = ` Wetland spp count`), lwd = 0) + 
  #facet_wrap(~group, shrink = TRUE)+
  #scale_fill_viridis_c()+
  #scale_colour_viridis_c()+
  #geom_polygon()+
  #theme(panel.background = element_rect(fill = "white"))+
  #labs(title="",
       #colour="Wetland spp count",
       #fill="Wetland spp count")

#Wetland

#Endemic  <- ggplot() + geom_sf(data = Spatial_heat_data.1, aes(fill = `Total & Narrow range spp count`, colour = `Total & Narrow range spp count`), lwd = 0) + 
  #facet_wrap(~group, shrink = TRUE)+
  #scale_fill_viridis_c()+
  #scale_colour_viridis_c()+
  #geom_polygon()+
  #theme(panel.background = element_rect(fill = "white"))+
  #labs(title="",
       #colour="Total & endemic count",
       #fill="Total & endemic count")

#Endemic

#This using facet_wrap(), to Create maps of different scales 
#library(purrr)

# Assuming p is a list
#p <- list()

# Unique groups in your data
#groups <- unique(Spatial_heat_data$group)

# Create plots for each group
#Richness.plot <- map(groups, ~ {
  #Spatial_heat_data %>% 
   # filter(group == .x) %>% 
    #ggplot() +
    #geom_sf(aes(fill = `Species richness`, colour = `Species richness`), lwd = 0) +
    #scale_fill_viridis_c() +
    #scale_colour_viridis_c() +
    #theme(panel.background = element_rect(fill = "white")) +
    #labs(
     # title = "",
      #colour = "Endemic spp\nrichness",
      #fill = "Endemic spp\nrichness"
    #) +
    #ggtitle(NULL)
#})

# Now p is a list of ggplot objects for each group

# Create labels for the plots
#plot_labels <- c(" All Endemics", " Facultative", " Obligate")

# Use ggarrange to arrange plots and add labels
#Richness.plot <- ggpubr::ggarrange(plotlist = Richness.plot, labels = plot_labels, label.x = 0.4, label.y = 0.9,  hjust = 0, nrow = 3)

#ggsave("Output/heatmaps.png", Richness.plot, width = 14, height = 18.7, units = "cm")

#save species_heat_map_2
#saveRDS(Spatial_heat_data_2, file = "Data/Spatial_heat_data_2")

#Remove all objects we wont need for the analysis

#rm(full_endemic, Richness.plot, Spatial_heat_data, groups, plot_labels, p)


############ Full species distribution

Total_species_Richness <- ggplot()+
  geom_sf(data =  CFR_data ,aes(fill = Total_species_richness, colour = Total_species_richness), lwd = 0)+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="",
       colour="Total spp\nRichness",
       fill="Total spp\nRichness")

#Narrow range endemics 


Narrow_range_endemics = ggplot()+
  geom_sf(data =  CFR_data  ,aes(fill = Narrow_Range_endemics, colour = Narrow_Range_endemics), lwd = 0)+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="",
       colour="Endemic spp\nrichness",
       fill="Endemic spp\nrichness")+
  scale_x_continuous(limits = c(16, 28))+
  scale_y_continuous(limits = c(-29, -36))


# Now p is a list of ggplot objects for each group

# Create labels for the plots
plot_labels <- c(" 1a) All Species", "1b) Narrow-range endemic")

#Richness.plot <- c(Total_species_Richness, Narrow_range_endemics)

# Use ggarrange to arrange plots and add labels
Richness.plot <- ggpubr::ggarrange(Total_species_Richness, Narrow_range_endemics , labels = plot_labels, label.x = 0.4, label.y = 0.9,  hjust = 0, nrow = 2)

ggsave("Output/heatmaps1.png", Richness.plot, width = 14, height = 18.7, units = "cm")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------wetland plots ------------------------

##############################################################################

facultative <- ggplot() + geom_sf(data = CFR_data, aes(fill = Facultative_species, colour = Facultative_species), lwd = 0) + 
  scale_fill_viridis_c()+
  scale_colour_viridis_c()+
  geom_polygon()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="",
       colour="facultative spp\nrichness",
       fill="facultative spp\nrichness")

Obligate <-ggplot() + geom_sf(data = CFR_data, aes(fill = Obligate_species, colour = Obligate_species), lwd = 0) + 
  scale_fill_viridis_c()+
  scale_colour_viridis_c()+
  geom_polygon()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="",
       colour="Obligate spp\nrichness",
       fill="Obligate spp\nrichness")


# Create labels for the plots
plot_labels.1 <- c(" 1c) Facultative", "1d) Obligate")

#Richness.plot <- c(Total_species_Richness, Narrow_range_endemics)

# Use ggarrange to arrange plots and add labels
Richness.plot.wet <- ggpubr::ggarrange(facultative, Obligate , labels = plot_labels.1, label.x = 0.4, label.y = 0.9,  hjust = 0, nrow = 2)

plot_labels.2 <- c("", "")

#Merge all 4 plots into one 

Plot1.Manscript <- ggpubr::ggarrange(Richness.plot, Richness.plot.wet , labels = plot_labels.2 , label.x = 0.4, label.y = 0.9,  hjust = 0, nrow = 2)


ggsave("Output/heatmaps1.png", Plot1.Manscript, width = 18, height = 32, units = "cm")




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------Climatic stability hypothesis------------------------

##############################################################################


Wetland_Coverage = ggplot()+
  geom_sf(data =  CFR_data ,aes(fill = percent_wetland, colour = percent_wetland), lwd = 0)+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()+
  labs(title="2c) % wetland",
       colour="Cover\n(%)",
       fill="Cover\n(%)")+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))
  
  



Mean_Cloud = ggplot()+
  geom_sf(data =  CFR_data ,aes(fill = Median_CloudFQ, colour = Median_CloudFQ), lwd = 0)+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="2b) Cloud FQ ",
       colour="Frequency\n(%) ",
       fill="Frequency\n(%) ")+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

CSI = ggplot()+
  geom_sf(data =  CFR_data ,aes(fill = Mean_CSI, colour = Mean_CSI), lwd = 0)+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="2a) CSI",
       colour="CSI",
       fill="CSI")+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))


# Create labels for the plots
plot_labels <- c("CSI", "Wetland Coverage", "Mean Cloud")

# Use ggarrange to arrange plots and add labels

Climatic_stability <- ggpubr::ggarrange(CSI, Wetland_Coverage, Mean_Cloud, labels = plot_labels, label.x = 0.4, label.y = 0.9,  hjust = 0, nrow = 3)


ggsave("Climatic_stability.png", Climatic_stability, width = 14, height = 18.7, units = "cm")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------productivity ------------------------

##############################################################################


Temperature = ggplot()+
  geom_sf(data =  CFR_data ,aes(fill = Mean_annual_Temperature, colour = Mean_annual_Temperature), lwd = 0)+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="3a) MAT",
       colour="°C",
       fill="°C")+
  scale_x_continuous(limits = c(16, 28))+
  scale_y_continuous(limits = c(-29, -36))+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

#ggsave("Output/Temperature.png", Temperature, width = 14, height = 18.7, units = "cm")


Rainfall= ggplot()+
  geom_sf(data =  CFR_data ,aes(fill = mean_annual_Rainfall, colour = mean_annual_Rainfall), lwd = 0)+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="3b) MAP",
       colour=expression(paste(kg, .m^-2, .year^-1)),
       fill=expression(paste(kg, .m^-2, .year^-1)))+
  scale_x_continuous(limits = c(16, 28))+
  scale_y_continuous(limits = c(-29, -36))+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

#ggsave("Output/Rainfall.png", Rainfall, width = 14, height = 18.7, units = "cm")



Dry_Rainfall = ggplot()+
  geom_sf(data =  CFR_data ,aes(fill = Mean_Dry_Rainfall, colour = Mean_Dry_Rainfall), lwd = 0)+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="",
       colour=expression(paste(kg, .m^-2, .year^-1)),
       fill=expression(paste(kg, .m^-2, .year^-1)))

#ggsave("Output/PET.png", PET, width = 14, height = 18.7, units = "cm")


# Create labels for the plots
plot_labels_p1 <- c(" MAT", " MAP", "MDQ")

# Use ggarrange to arrange plots and add labels

Productivity_1 <- ggpubr::ggarrange(Temperature, Rainfall, Dry_Rainfall, labels = plot_labels_p1, label.x = 0.4, label.y = 0.9,  hjust = 0, nrow = 3)


ggsave("Productivity_1.png", Productivity_1, width = 14, height = 18.7, units = "cm")

###############################################


PET <- ggplot()+
  geom_sf(data =  CFR_data ,aes(fill = Mean_PET, colour = Mean_PET), lwd = 0)+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="3c) PET",
       colour=expression(paste(kg, .m^-2, .year^-1)),
       fill=expression(paste(kg, .m^-2, .year^-1)))+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

#ggsave("Output/Dry_Rainfall.png", Dry_Rainfall, width = 14, height = 18.7, units = "cm")

  
Soil_ph <- ggplot() +
  geom_sf(data = CFR_data, aes(fill = Mean_Soil_ph, colour = Mean_Soil_ph), lwd = 0) +
  scale_fill_viridis_c() +
  scale_colour_viridis_c() +
  theme(panel.background = element_rect(fill = "white")) +
  labs(
    title = "3d) pH",
    fill = "pH",  # Label for the fill legend
    colour = "pH"  # Label for the colour legend
  )+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

#ggsave("Output/Soil_ph.png", Soil_ph, width = 14, height = 18.7, units = "cm")


Mean_Ext_P <-ggplot() +
  geom_sf(data = CFR_data, aes(fill = Mean_Ext_P, colour = Mean_Ext_P), lwd = 0) +
  scale_fill_viridis_c() +
  scale_colour_viridis_c() +
  theme(panel.background = element_rect(fill = "white")) +
  labs(
    title = "3e) P",
    fill = "mg/kg",  # Label for the fill legend
    colour = "mg/kg"  # Label for the colour legend
  )+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

#ggsave("Output/Mean_Ext_P.png", Mean_Ext_P, width = 14, height = 18.7, units = "cm")


# Create labels for the plots
plot_labels_p2 <- c("PET ", "pH ", "P")

# Use ggarrange to arrange plots and add labels

Productivity_2 <- ggpubr::ggarrange(Dry_Rainfall, Soil_ph, Mean_Ext_P, labels = plot_labels_p2, label.x = 0.4, label.y = 0.9,  hjust = 0, nrow = 3)

ggsave("Output/Productivity_2.png", Productivity_2, width = 14, height = 18.7, units = "cm")

#Productivity <- ggpubr::ggarrange(Productivity_1, Productivity_2, label.x = 0.4, label.y = 0.9,  hjust = 0, nrow = 1)

#ggsave("Output/Productivity.png", Productivity, width = 40, height = 21, units = "cm")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------Heterogeneity  ------------------------

##############################################################################




Range_Temperature <- ggplot() +
  geom_sf(data = CFR_data, aes(fill = Range_Temperature, colour = Range_Temperature), lwd = 0) +
  scale_fill_viridis_c() +
  scale_colour_viridis_c() +
  theme(panel.background = element_rect(fill = "white")) +
  labs(
    title = "2e) Range in MAT",
    fill = "°C",  # Label for the fill legend
    colour = "°C"  # Label for the colour legend
  )+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

#ggsave("Output/Range_Temperature.png", Range_Temperature, width = 14, height = 18.7, units = "cm")

Range_Rainfall <- ggplot() +
  geom_sf(data = CFR_data, aes(fill = Range_Rainfall, colour = Range_Rainfall), lwd = 0) +
  scale_fill_viridis_c() +
  scale_colour_viridis_c() +
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="",
       colour=expression(paste(kg, .m^-2, .year^-1)),
       fill=expression(paste(kg, .m^-2, .year^-1)))

#ggsave("Output/Range_Rainfall.png", Range_Rainfall, width = 14, height = 18.7, units = "cm")

Range_Dry_rainfall <- ggplot() +
  geom_sf(data = CFR_data, aes(fill = Range_Dry_rainfall, colour = Range_Dry_rainfall), lwd = 0) +
  scale_fill_viridis_c() +
  scale_colour_viridis_c() +
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="",
       colour=expression(paste(kg, .m^-2, .year^-1)),
       fill=expression(paste(kg, .m^-2, .year^-1)))+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 40),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

ggsave("Output/Range_Dry_rainfall.png", Range_Dry_rainfall, width = 14, height = 18.7, units = "cm")


# Create labels for the plots
plot_labels_p3 <- c("Range in MAT", "Range in MAP", "Range in MDQ")

# Use ggarrange to arrange plots and add labels

heterogeneity_1 <- ggpubr::ggarrange(Range_Temperature, Range_Rainfall, Range_Dry_rainfall, labels = plot_labels_p3, label.x = 0.4, label.y = 0.9,  hjust = 0, nrow = 3)


ggsave("Output/heterogeneity_1.png", heterogeneity_1, width = 14, height = 18.7, units = "cm")



################################################################


Roughness_Temperature <- ggplot() +
  geom_sf(data = CFR_data, aes(fill = Roughness_Dry_Rainfall, colour = Roughness_Dry_Rainfall), lwd = 0) +
  scale_fill_viridis_c() +
  scale_colour_viridis_c() +
  theme(panel.background = element_rect(fill = "white")) +
  labs(
    title = "",
    fill = "°C",  # Label for the fill legend
    colour = "°C"  # Label for the colour legend
  )

#ggsave("Output/Roughness_Temperature.png", Roughness_Temperature, width = 14, height = 18.7, units = "cm")



Roughness_Rainfall <- ggplot() +
  geom_sf(data = CFR_data, aes(fill = Roughness_Rainfall, colour = Roughness_Rainfall), lwd = 0) +
  scale_fill_viridis_c() +
  scale_colour_viridis_c() +
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="",
       colour=expression(paste(kg, .m^-2, .year^-1)),
       fill=expression(paste(kg, .m^-2, .year^-1)))

#ggsave("Output/Roughness_Rainfall.png", Roughness_Rainfall, width = 14, height = 18.7, units = "cm")



Roughness_Dry_Rainfall <- ggplot() +
  geom_sf(data = CFR_data, aes(fill = Roughness_Dry_Rainfall, colour = Roughness_Dry_Rainfall), lwd = 0) +
  scale_fill_viridis_c() +
  scale_colour_viridis_c() +
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="2d) Roughness MDQ",
       colour=expression(paste(kg, .m^-2, .year^-1)),
       fill=expression(paste(kg, .m^-2, .year^-1)))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

#ggsave("Output/Roughness_Dry_Rainfall.png", Roughness_Dry_Rainfall, width = 14, height = 18.7, units = "cm")



# Create labels for the plots
plot_labels_p4 <- c("Roughness in MAT ", "Roughness in MAP ", "Roughness in MAQ")

# Use ggarrange to arrange plots and add labels

heterogeneity_2 <- ggpubr::ggarrange(Roughness_Dry_Rainfall, Roughness_Rainfall, Roughness_Dry_Rainfall, labels = plot_labels_p4, label.x = 0.4, label.y = 0.9,  hjust = 0, nrow = 3)

ggsave("Output/heterogeneity_2.png", heterogeneity_2, width = 14, height = 18.7, units = "cm")


#plot_labels_p5 <- c("", "")

#heterogeneity <- ggpubr::ggarrange(heterogeneity_1, heterogeneity_2,  label.x = 0.4, label.y = 0.9,  hjust = 0, nrow = 1)

ggsave("Output/heterogeneity.png", heterogeneity, width = 70, height = 30, units = "cm")


#############################################################
####PET 

Range_PET <- ggplot() +
  geom_sf(data = CFR_data, aes(fill = PET_Range, colour = PET_Range), lwd = 0) +
  scale_fill_viridis_c() +
  scale_colour_viridis_c() +
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="",
       colour=expression(paste(kg, .m^-2, .year^-1)),
       fill=expression(paste(kg, .m^-2, .year^-1)))
  

Roughness_PET <- ggplot() +
  geom_sf(data = CFR_data, aes(fill = PET_Roughness, colour = PET_Roughness), lwd = 0) +
  scale_fill_viridis_c() +
  scale_colour_viridis_c() +
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="2f) Rougness in PET",
       colour=expression(paste(kg, .m^-2, .year^-1)),
       fill=expression(paste(kg, .m^-2, .year^-1)))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

plot_labels_p6 <- c("Range in PET ", "Roughness in PET ")

heterogeneity_3 <- ggpubr::ggarrange(Range_PET, Roughness_PET, labels = plot_labels_p6, label.x = 0.4, label.y = 0.9,  hjust = 0, nrow = 2)

ggsave("Output/heterogeneity_3.png", heterogeneity_3, width = 14, height = 18.7, units = "cm")




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------ploting all covariates------------------------

##############################################################################
###Climatic stability and heterogeneity 

#plot_labels_Covariates_CG <- c("2a) CSI", "2b) Cloud FQ ", "2c) % wetland", "2d) Roughness MDQ"," 2e) Range in MAT",
                               #"")

Covariates_CG <- ggpubr::ggarrange(CSI, Mean_Cloud, Wetland_Coverage, Roughness_Dry_Rainfall, Range_Temperature,Roughness_PET,
                                  label.x = 0.4, label.y = 0.9,  
                                  hjust = 0, ncol = 3, nrow = 3)

ggsave("Covariates.png", Covariates_CG, width = 126, height = 100, units = "cm")

##############################################################################
###productivity  


#plot_labels_Covariates <- c("2a) CSI", "2b) Cloud FQ ", "2c) % wetland", "2d) Roughness MDQ"," 2e) Range in MAT",
                            #"2f) MAP", "2g) Roughness MDQ", "2h) PET", "2i) Roughness in PET", "2j) pH", "2k) P")

Covariates_p <- ggpubr::ggarrange(Temperature,Rainfall, PET, Soil_ph ,
                                Mean_Ext_P, label.x = 0.4, label.y = 0.9,  
                                hjust = 0, ncol = 3, nrow = 3)


ggsave("Covariates_P.png", Covariates_p, width = 126, height = 100, units = "cm")

