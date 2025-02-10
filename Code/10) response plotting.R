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

# widespread

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
       colour="CFR total\nspp count",
       fill="CFR total\nspp count", tag = "a)")+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

# widespread species 

WS = ggplot()+
  geom_sf(data =  CFR_data ,aes(fill = widespread_spp, colour = widespread_spp), lwd = 0)+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="",
       colour="CFR widespread\nspp count",
       fill="CFR widespread\nspp count", tag = "b)")+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))


# Narrow range endemics 

Narrow_range_endemics <- ggplot()+
  geom_sf(data =  CFR_data  ,aes(fill = Narrow_Range_endemics, colour = Narrow_Range_endemics), lwd = 0)+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title = "" ,
       colour="CFR endemic\nspp count",
       fill="CFR endemic\nspp count", tag = "c)")+
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
  labs(title = "" ,
       colour="CFR wetland\nspp count",
       fill="CFR wetland\nspp count", tag = "d)")+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

# Facultative map

#Facultative_nre <- ggplot()+
  #geom_sf(data =  CFR_data  ,aes(fill = Facultative_species, colour = Facultative_species), lwd = 0)+
  #scale_fill_viridis_c()+
  #scale_colour_viridis_c()+
  #theme(panel.background = element_rect(fill = "white"))+
  #labs(title = "d)" ,
       #colour="facultative spp\nrichness",
      # fill="facultative spp\nrichness")+
  #theme(panel.background = element_rect(fill = "white"),
        #text = element_text(size = 50),
        #legend.key.height = unit(2, 'cm'), 
        #legend.key.width = unit(2, 'cm'),
       # axis.title = element_text(size = 50, face = "bold"),
      #  plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

# Obligate map

#Obligate_nre <- ggplot()+
#  geom_sf(data =  CFR_data  ,aes(fill = Obligate_species, colour = Obligate_species), lwd = 0)+
#  scale_fill_viridis_c()+
 # scale_colour_viridis_c()+
  #theme(panel.background = element_rect(fill = "white"))+
  #labs(title = "e)" ,
   #    colour="obligate spp\nrichness",
    #   fill="obligate spp\nrichness")+
#  theme(panel.background = element_rect(fill = "white"),
 #       text = element_text(size = 50),
  #      legend.key.height = unit(2, 'cm'), 
   #     legend.key.width = unit(2, 'cm'),
    #    axis.title = element_text(size = 50, face = "bold"),
     #   plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

#Save the total species spatial heatmap

# Plot the heatmaps 

Plot2.Manscript <- ggpubr::ggarrange(Total_species_Richness,
                                     WS,
                                     Narrow_range_endemics,
                                     Wetland_dp,
                                     labels = "" , 
                                     label.x = 0.4, label.y = 0.9,  hjust = 0, nrow = 2, ncol = 2)


ggsave("Output/Fig.1.png", Plot2.Manscript, width = 80, height = 110, units = "cm")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------- Total scatter plots --------------------------------

##############################################################################

# linear relationship between species richness and narrow range endemics,
# wetland, facultative and obligate 

## NRE

spp_nre <- lm(Narrow_Range_endemics ~ Total_species_richness, data = CFR_data)
#model outputs 
summary(spp_nre)

#correlation
cor.test(CFR_data$Narrow_Range_endemics, CFR_data$Total_species_richness)

# plot the relationship 
my_label_nre <- c(paste0("R^2 == 0.53"),
              paste("p ≤ 0.001"))

cex.lab <- 1.8; cex.axis <- 1.5
NRE_Scatter <- visreg(spp_nre, gg = T)+
  geom_point(colour = 'black', size = 3)+
  labs(title = "",x = "", y = "Narrow range endemics", tag = "a)")+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        axis.title = element_text(size = 50, face = "bold", vjust = 1),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))+
  annotate("text", x = 480, y = c(450,350), 
           label = my_label_nre, parse = TRUE, size = 20)

## wetland dependent NRES


# linear relationship between species richness and narrow range endemics 
Wetland_spp_nre <- lm(Wetland_spp ~ Total_species_richness, data = CFR_data)

summary(Wetland_spp_nre)
# plot the relationship 

#correlation
cor.test(CFR_data$Wetland_spp, CFR_data$Total_species_richness)

# create object label
my_label_wetland <- c(paste0("R^2 == 0.57"),
                  paste(expression("p =≤ 0.001")))

Wetland_Scatter <- visreg(Wetland_spp_nre, gg = T)+
  geom_point(colour = 'black', size = 3)+
  labs(title = "",x = "Total species richness", y = "Wetland NREs", tag = "c)")+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        axis.title = element_text(size = 50, face = "bold", vjust = 1),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))+
  annotate("text", x = 500, y = c(50,35), 
           label = my_label_wetland, parse = TRUE, size = 20)


## facultative wetland dependent nre

# linear relationship between species richness and facultative narrow range endemics 
#facultative_spp_nre <- lm(Facultative_species ~ Total_species_richness, data = CFR_data)

#summary(facultative_spp_nre)
# plot the relationship 

#correlation
#cor.test(CFR_data$Facultative_species, CFR_data$Total_species_richness)

# create object label
#my_label_facultive <- c(paste0("R^2 == 0.57"),
 #                     paste("r == 0.76"))

#facultative_Scatter <- visreg(facultative_spp_nre, gg = T)+
  #geom_point(colour = 'black', size = 3)+
  #labs(title = "",x = "", y = "Facultative NREs")+
  #theme(panel.background = element_rect(fill = "white"),
     #   text = element_text(size = 50),
    #    axis.title = element_text(size = 50, face = "bold", vjust = 1),
   #     plot.title = element_text(size = 50, face = "bold", hjust = 0.5))+
  #annotate("text", x = 500, y = c(35,28), 
   #        label = my_label_facultive, parse = TRUE, size = 20)

## Obligate spp

# linear relationship between species richness and narrow range endemics 

#spp_obg <- lm(Obligate_species ~ Total_species_richness, data = CFR_data)

#summary(spp_obg)
# plot the relationship 
#my_label_OBG <- c(paste0("R^2 ==  0.46"),
 #                paste("r == 0.68"))

#correlation
#cor.test(CFR_data$Obligate_species, CFR_data$Total_species_richness)
#OBG_Scatter <- visreg(spp_obg, gg = T)+
# geom_point(colour = 'black', size = 3)+
#  labs(title="", x = "Total Species Richness", y = "Obligate NREs", tag = "b)")+
 # theme(panel.background = element_rect(fill = "white"),
  # text = element_text(size = 50),
   #axis.title = element_text(size = 50, face = "bold", vjust = 1),
   #plot.title = element_text(size = 50, face = "bold", hjust = 0.5))+
   #annotate("text", x = 500, y = c(24,18), 
  #label = my_label_OBG, parse = TRUE, size = 20)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------Narrow range plots ------------------------

##############################################################################

## Wetland
nre_wetland <- lm(Wetland_spp ~ Narrow_Range_endemics, data = CFR_data)

#model outputs 
summary(nre_wetland)

#correlation
cor.test(CFR_data$Narrow_Range_endemics, CFR_data$Wetland_spp)

# plot the relationship 
nre_wetland_label <- c(paste0("R^2 == 0.83"),
                  paste("p ≤ 0.001"))

NRE_v_WET <- visreg(nre_wetland, gg = T)+
  geom_point(colour = 'black', size = 3)+
  labs(title = "",x = "", y = "Narrow range endemics", tag = "c)")+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        axis.title = element_text(size = 50, face = "bold", vjust = 1),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))+
  annotate("text", x = 170, y = c(48,39), 
           label = nre_wetland_label, parse = TRUE, size = 20)

## Facultative spp

#nre_facutative <- lm(Facultative_species ~ Narrow_Range_endemics, data = CFR_data)
#model outputs 
#summary(nre_facutative)

#correlation
#cor.test(CFR_data$Narrow_Range_endemics, CFR_data$Facultative_species)

# plot the relationship 
#nre_facultative_label <- c(paste0("R^2 == 0.82"),
 #                      paste("r == 0.90"))

#NRE_v_FCT <- visreg(nre_facutative, gg = T)+
 # geom_point(colour = 'black', size = 3)+
  #labs(title = "",x = "", y = "")+
#  theme(panel.background = element_rect(fill = "white"),
 #       text = element_text(size = 50),
  #      axis.title = element_text(size = 50, face = "bold", vjust = 1),
   #     plot.title = element_text(size = 50, face = "bold", hjust = 0.5))+
#  annotate("text", x = 200, y = c(40,32), 
 #          label = nre_facultative_label, parse = TRUE, size = 20)

## Obligate spp

#nre_obligate <- lm(Obligate_species ~ Narrow_Range_endemics, data = CFR_data)
#model outputs 
#summary(nre_obligate)

#correlation
#cor.test(CFR_data$Narrow_Range_endemics, CFR_data$Obligate_species)

# plot the relationship 
#nre_obligate_label <- c(paste0("R^2 == 0.71"),
      #                     paste("r == 0.84"))

#NRE_v_OLG <- visreg(nre_obligate, gg = T)+
 # geom_point(colour = 'black', size = 3)+
  #labs(title = "",x = "Narrow range endemics", y = "")+
#  theme(panel.background = element_rect(fill = "white"),
 #       text = element_text(size = 50),
  #      axis.title = element_text(size = 50, face = "bold", vjust = 1),
   #     plot.title = element_text(size = 50, face = "bold", hjust = 0.5))+
#  annotate("text", x = 200, y = c(28,21), 
 #          label = nre_obligate_label, parse = TRUE, size = 20)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------Wetland plots ------------------------

##############################################################################

#wet_fct <- lm(Facultative_species ~ Wetland_spp, data = CFR_data)
#model outputs 
#summary(wet_fct)

#correlation
#cor.test(CFR_data$Facultative_species, CFR_data$Wetland_spp)

# plot the relationship 
#wet_facultative_label <- c(paste0("R^2 == 0.96"),
 #                          paste("r == 0.98"))
#
#wet_v_FCT <- visreg(wet_fct, gg = T)+
 # geom_point(colour = 'black', size = 3)+
  #labs(title = "",x = "", y = "")+
  #theme(panel.background = element_rect(fill = "white"),
   #     text = element_text(size = 50),
    #    axis.title = element_text(size = 50, face = "bold", vjust = 1),
     #   plot.title = element_text(size = 50, face = "bold", hjust = 0.5))+
  #annotate("text", x = 20, y = c(49,40), 
   #        label = wet_facultative_label, parse = TRUE, size = 20)

##############################################################################

# Obligate 

#wet_olg <- lm(Obligate_species ~ Wetland_spp, data = CFR_data)
#model outputs 
#summary(wet_olg)

#correlation
#cor.test(CFR_data$Obligate_species, CFR_data$Wetland_spp)

# plot the relationship 
#wet_obligate_label <- c(paste0("R^2 == 0.89"),
 #                          paste("r == 0.94"))

#wet_v_obg <- visreg(wet_fct, gg = T)+
 # geom_point(colour = 'black', size = 3)+
  #labs(title = "",x = "Wetland NREs", y = "")+
  #theme(panel.background = element_rect(fill = "white"),
   #     text = element_text(size = 50),
    #    axis.title = element_text(size = 50, face = "bold", vjust = 1),
     #   plot.title = element_text(size = 50, face = "bold", hjust = 0.5))+
  #annotate("text", x = 20, y = c(39,30), 
   #        label = wet_obligate_label, parse = TRUE, size = 20)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------facultative plots ------------------------

##############################################################################

## Obligate
#fct_obg <- lm(Obligate_species ~ Facultative_species, data = CFR_data)

#model outputs 
#summary(fct_obg)

#correlation
#cor.test(CFR_data$Obligate_species, CFR_data$Facultative_species)

# plot the relationship 
#fct_obg_label <- c(paste0("R^2 == 0.72"),
 #                      paste("r == 0.85"))

#fct_v_obg <- visreg(fct_obg, gg = T)+
#  geom_point(colour = 'black', size = 3)+
#  labs(title = "",x = "Facultative NREs", y = "")+
#  theme(panel.background = element_rect(fill = "white"),
#        text = element_text(size = 50),
 #       axis.title = element_text(size = 50, face = "bold", vjust = 1),
 #       plot.title = element_text(size = 50, face = "bold", hjust = 0.5))+
#  annotate("text", x = 10, y = c(32,25), 
 #          label = fct_obg_label, parse = TRUE, size = 20)

#Blank plots 

position1 <- ggplot() +
  labs(title = "Narrow range endemics",x = "", y = "")+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        axis.title = element_text(size = 50, face = "bold", vjust = 1),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

#P2

#position2 <- ggplot() +
 # labs(title = "Wetland NREs",x = "", y = "")+
  #theme(panel.background = element_rect(fill = "white"),
   #     text = element_text(size = 50),
    #    axis.title = element_text(size = 50, face = "bold", vjust = 1),
     #   plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

#p3

#position3 <- ggplot() +
 # labs(title = "Facultative NREs",x = "", y = "")+
  #theme(panel.background = element_rect(fill = "white"),
   #     text = element_text(size = 50),
    #    axis.title = element_text(size = 50, face = "bold", vjust = 1),
     #   plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

#p4 

#position4 <- ggplot() +
 # labs(title = "",x = "", y = "")+
  #theme(panel.background = element_rect(fill = "white"),
   #     text = element_text(size = 50),
    #    axis.title = element_text(size = 50, face = "bold", vjust = 1),
     #   plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

#p5

#position5 <- ggplot() +
 # labs(title = "",x = "", y = "")+
  #theme(panel.background = element_rect(fill = "white"),
   #     text = element_text(size = 50),
    #    axis.title = element_text(size = 50, face = "bold", vjust = 1),
     #   plot.title = element_text(size = 50, face = "bold", hjust = 0.5))

#p6

#position6 <- ggplot() +
 # labs(title = "",x = "", y = "")+
  #theme(panel.background = element_rect(fill = "white"),
   #     text = element_text(size = 50),
    #    axis.title = element_text(size = 50, face = "bold", vjust = 1),
     #   plot.title = element_text(size = 50, face = "bold", hjust = 0.5))



#Merge all 4 plots into one 

Plot1.Manscript <- ggpubr::ggarrange(NRE_Scatter, position1,
                                     Wetland_Scatter, NRE_v_WET,
                                     labels = "" , 
                                     label.x = 0.4, label.y = 0.9,  hjust = 0, nrow = 2, ncol = 2)


ggsave("Output/Fig.2.png", Plot1.Manscript, width = 60, height = 80, units = "cm")


##############################################################################
######proceed to  Analysis folder : 1) Total species modeling



