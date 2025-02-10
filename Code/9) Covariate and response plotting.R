#Plot the maps of response and covariates 


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
library(visreg)
#library(ggplotify)


#read data set
CFR_data <- readRDS("Data/CFR_data")

# add obligate and facultative spp to have the all wetland spp

CFR_data$Wetland_spp <- CFR_data$Facultative_species + CFR_data$Obligate_species

CFR_data$widespread_spp <- CFR_data$Total_species_richness- CFR_data$Narrow_Range_endemics


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


# Full species distribution

Total_species_Richness <- ggplot()+
  geom_sf(data =  CFR_data ,aes(fill = Total_species_richness, colour = Total_species_richness), lwd = 0)+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="",
       colour="total spp\nRichness",
       fill="total spp\nRichness")

# Narrow range endemics 

Narrow_range_endemics <- ggplot()+
  geom_sf(data =  CFR_data  ,aes(fill = Narrow_Range_endemics, colour = Narrow_Range_endemics), lwd = 0)+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title = " a) NRE Spatial heatmap " ,
       colour="endemic spp\nrichness",
       fill="endemic spp\nrichness")+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

# Wetland map

Wetland_dp <- ggplot()+
  geom_sf(data =  CFR_data  ,aes(fill = Wetland_spp, colour = Wetland_spp), lwd = 0)+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title = " " ,
       colour="wetland spp\nrichness",
       fill="wetland spp\nrichness")+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

# Facultative map

Facultative_nre <- ggplot()+
  geom_sf(data =  CFR_data  ,aes(fill = Facultative_species, colour = Facultative_species), lwd = 0)+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title = " " ,
       colour="facultative spp\nrichness",
       fill="facultative spp\nrichness")+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

# Obligate map

Obligate_nre <- ggplot()+
  geom_sf(data =  CFR_data  ,aes(fill = Obligate_species, colour = Obligate_species), lwd = 0)+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title = " " ,
       colour="obligate spp\nrichness",
       fill="obligate spp\nrichness")+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

#Save the total species spatial heatmap

#ggsave("Output/Fig.1.png", Total_species_Richness,  width = 14, height = 18.7, units = "cm")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------- Total scatter plots --------------------------------

##############################################################################

# linear relationship between species richness and narrow range endemics,
# wetland, facultative and obligate 

spp_ws <- lm(widespread_spp ~ Total_species_richness, data = CFR_data)
#model outputs 
summary(spp_ws)

#correlation
cor.test(CFR_data$widespread_spp, CFR_data$Total_species_richness)

# plot the relationship 
my_label_ws <- c(paste0("R^2 == 0.53"),
                  paste("ρ == 0.99"))

cex.lab <- 1.8; cex.axis <- 1.5
ws_Scatter <- visreg(spp_ws, gg = T)+
  geom_point(colour = 'black', size = 3)+
  labs(title = "",x = "", y = "Number of widespread spp")+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        axis.title = element_text(size = 50, face = "bold", vjust = 1),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))+
  annotate("text", x = 2300, y = c(450,200), 
           label = my_label_ws, parse = TRUE, size = 25)

## NRE

spp_nre <- lm(Narrow_Range_endemics ~ Total_species_richness, data = CFR_data)
#model outputs 
summary(spp_nre)

#correlation
cor.test(CFR_data$Narrow_Range_endemics, CFR_data$Total_species_richness)

# plot the relationship 
my_label_nre <- c(paste0("R^2 == 0.53"),
              paste("ρ == 0.73"))

cex.lab <- 1.8; cex.axis <- 1.5
NRE_Scatter <- visreg(spp_nre, gg = T)+
  geom_point(colour = 'black', size = 3)+
  labs(title = "",x = "", y = "Number of NREs")+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        axis.title = element_text(size = 50, face = "bold", vjust = 1),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))+
  annotate("text", x = 2300, y = c(200,90), 
           label = my_label_nre, parse = TRUE, size = 25)

## wetland dependent NRES


# linear relationship between species richness and narrow range endemics 
Wetland_spp_nre <- lm(Wetland_spp ~ Total_species_richness, data = CFR_data)

summary(Wetland_spp_nre)
# plot the relationship 

#correlation
cor.test(CFR_data$Wetland_spp, CFR_data$Total_species_richness)

# create object label
my_label_wetland <- c(paste0("R^2 == 0.57"),
                  paste("ρ  == 0.75"))

Wetland_Scatter <- visreg(Wetland_spp_nre, gg = T)+
  geom_point(colour = 'black', size = 3)+
  labs(title = "",x = "", y = "No. of Wetland NREs")+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        axis.title = element_text(size = 50, face = "bold", vjust = 1),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))+
  annotate("text", x = 2300, y = c(100,90), 
           label = my_label_wetland, parse = TRUE, size = 25)


# facultative wetland dependent nre

# linear relationship between species richness and facultative narrow range endemics 
facultative_spp_nre <- lm(Facultative_species ~ Total_species_richness, data = CFR_data)

summary(facultative_spp_nre)
# plot the relationship 

#correlation
cor.test(CFR_data$Facultative_species, CFR_data$Total_species_richness)

# create object label
my_label_facultive <- c(paste0("R^2 == 0.57"),
                      paste("r == 0.76"))

facultative_Scatter <- visreg(facultative_spp_nre, gg = T)+
  geom_point(colour = 'black', size = 3)+
  labs(title = "",x = "", y = "No. of facultative NREs")+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        axis.title = element_text(size = 50, face = "bold", vjust = 1),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))+
  annotate("text", x = 400, y = c(350,280), 
           label = my_label_facultive, parse = TRUE, size = 25)

#facultative <- ggplot() + geom_sf(data = CFR_data, aes(fill = Facultative_species, colour = Facultative_species), lwd = 0) + 
#  scale_fill_viridis_c()+
#  scale_colour_viridis_c()+
#  geom_polygon()+
#  theme(panel.background = element_rect(fill = "white"))+
#  labs(title="c) FWD spatial heatmap",
#       colour="facultative spp\nrichness",
#       fill="facultative spp\nrichness")+
#  theme(panel.background = element_rect(fill = "white"),
#        text = element_text(size = 50),
#        legend.key.height = unit(2, 'cm'), 
#        legend.key.width = unit(2, 'cm'),
#        axis.title = element_text(size = 50, face = "bold"),
#        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

# linear relationship between species richness and narrow range endemics 

#spp_fct <- lm(Facultative_species ~ Total_species_richness, data = CFR_data)

#summary(spp_fct)
# plot the relationship 
#my_label_fct <- c(paste0("R^2 == 0.57"),
 #             paste("r == 0.01"))

#cex.lab <- 1.8; cex.axis <- 1.5
#FCT_Scatter <- visreg(spp_fct, gg = T)+
 # geom_point(colour = 'black', size = 3)+
#  labs(title="d) FWD Scatter plot", x = "", y = "Number of FWD")+
#  theme(panel.background = element_rect(fill = "white"),
#        text = element_text(size = 50),
#        axis.title = element_text(size = 50, face = "bold", vjust = 1),
#        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))+
#  annotate("text", x = 400, y = c(30,25), 
#           label = my_label_fct, parse = TRUE, size = 25)


# obligate wetland dependent narrow range endemics 

#Obligate <-ggplot() + geom_sf(data = CFR_data, aes(fill = Obligate_species, colour = Obligate_species), lwd = 0) + 
#  scale_fill_viridis_c()+
#  scale_colour_viridis_c()+
#  geom_polygon()+
#  theme(panel.background = element_rect(fill = "white"))+
#  labs(title="e) OWD Spatial heatmap",
#       colour="Obligate spp\nrichness",
#       fill="Obligate spp\nrichness")+
#  theme(panel.background = element_rect(fill = "white"),
#        text = element_text(size = 50),
#        legend.key.height = unit(2, 'cm'), 
#        legend.key.width = unit(2, 'cm'),
#        axis.title = element_text(size = 50, face = "bold"),
#        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

# linear relationship between species richness and narrow range endemics 

#spp_obg <- lm(Obligate_species ~ Total_species_richness, data = CFR_data)

#summary(spp_obg)
# plot the relationship 
#my_label_OBG <- c(paste0("R^2 ==  0.46"),
#                  paste("r == 0.006"))

#cex.lab <- 1.8; cex.axis <- 1.5
#OBG_Scatter <- visreg(spp_fct, gg = T)+
# geom_point(colour = 'black', size = 3)+
 # labs(title="d) OWD Scatter plot", x = "Total Species Richness", y = "Number of OWD")+
  #theme(panel.background = element_rect(fill = "white"),
   #     text = element_text(size = 50),
    #    axis.title = element_text(size = 50, face = "bold", vjust = 1),
     #   plot.title = element_text(size = 50, face = "bold", hjust = 0.5))+
#  annotate("text", x = 400, y = c(30,25), 
 #          label = my_label_OBG, parse = TRUE, size = 25)


#Merge all 4 plots into one 

Plot1.Manscript <- ggpubr::ggarrange(ws_Scatter, 
                                     NRE_Scatter, 
                                     Wetland_Scatter,
                                     labels = "" , 
                                     label.x = 0.4, label.y = 0.9,  
                                     hjust = 0, nrow = 3, ncol = 1)


ggsave("Output/Fig.a.png", Plot1.Manscript, width = 60, height = 125, units = "cm")




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------Climatic stability hypothesis------------------------

##############################################################################


Wetland_Coverage = ggplot()+
  geom_sf(data =  CFR_data ,aes(fill = percent_wetland, colour = percent_wetland), lwd = 0)+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()+
  labs(title="c) % wetland",
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
  labs(title="b) Cloud FQ ",
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
  labs(title="a) CSI",
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
#plot_labels <- c("CSI", "Wetland Coverage", "Mean Cloud")

# Use ggarrange to arrange plots and add labels

# Climatic_stability <- ggpubr::ggarrange(CSI, Wetland_Coverage, Mean_Cloud, labels = plot_labels, label.x = 0.4, label.y = 0.9,  hjust = 0, nrow = 3)


# ggsave("Climatic_stability.png", Climatic_stability, width = 14, height = 18.7, units = "cm")


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
  labs(title="d) MAT",
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
  labs(title="e) MAP",
       colour="mm",
       fill="mm")+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

#ggsave("Output/Rainfall.png", Rainfall, width = 14, height = 18.7, units = "cm")


PET <- ggplot()+
  geom_sf(data =  CFR_data ,aes(fill = Mean_PET, colour = Mean_PET), lwd = 0)+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="f) PET",
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
    title = "g) pH",
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

#CFR_data$Mean_Ext_P = log10(CFR_data$Mean_Ext_P)

#Plot mean phosphorus

Mean_Ext_P <-ggplot() +
  geom_sf(data = CFR_data, aes(fill = Mean_Ext_P, colour = Mean_Ext_P), lwd = 0) +
  scale_fill_viridis_c() +
  scale_colour_viridis_c() +
  theme(panel.background = element_rect(fill = "white")) +
  labs(
    title = "h) log P",
    fill = "log (mg/kg)",  # Label for the fill legend
    colour = "log (mg/kg)"  # Label for the colour legend
  )+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

#ggsave("Output/Mean_Ext_P.png", Mean_Ext_P, width = 14, height = 18.7, units = "cm")


# Create labels for the plots
#plot_labels_p2 <- c("PET ", "pH ", "P")

# Use ggarrange to arrange plots and add labels

#Productivity_2 <- ggpubr::ggarrange(Dry_Rainfall, Soil_ph, Mean_Ext_P, labels = plot_labels_p2, label.x = 0.4, label.y = 0.9,  hjust = 0, nrow = 3)

#ggsave("Output/Productivity_2.png", Productivity_2, width = 14, height = 18.7, units = "cm")

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
    title = "i) Range in MAT",
    fill = "°C",  # Label for the fill legend
    colour = "°C"  # Label for the colour legend
  )+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))


Roughness_Dry_Rainfall <- ggplot() +
  geom_sf(data = CFR_data, aes(fill = Roughness_Dry_Rainfall, colour = Roughness_Dry_Rainfall), lwd = 0) +
  scale_fill_viridis_c() +
  scale_colour_viridis_c() +
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="J) Roughness in MDQ",
       colour="mm",
       fill="mm")+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))


Roughness_PET <- ggplot() +
  geom_sf(data = CFR_data, aes(fill = PET_Roughness, colour = PET_Roughness), lwd = 0) +
  scale_fill_viridis_c() +
  scale_colour_viridis_c() +
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="k) Rougness in PET",
       colour=expression(paste(kg, .m^-2, .year^-1)),
       fill=expression(paste(kg, .m^-2, .year^-1)))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------ploting all covariates------------------------

##############################################################################
###Climatic stability and heterogeneity 

#plot_labels_Covariates_CG <- c("2a) CSI", "2b) Cloud FQ ", "2c) % wetland", "2d) Roughness MDQ"," 2e) Range in MAT",
#"")

Covariates_CG <- ggpubr::ggarrange(CSI, Mean_Cloud, Wetland_Coverage, 
                                   Temperature, Rainfall,
                                   PET, Soil_ph ,Mean_Ext_P, 
                                   Range_Temperature, Roughness_Dry_Rainfall ,Roughness_PET,
                                   label.x = 0.4, label.y = 0.9,  
                                   hjust = 0, ncol = 3, nrow = 4)

ggsave("Output/Fig.3.png", Covariates_CG, width = 126, height = 100, units = "cm")


##############################################################################
######proceed to  Analysis folder : 1) Total species modeling



p1 <- ggplot() + theme_void()
multiplot(p,p1,p1,p,p,p1,p,p,p,cols=3) 
