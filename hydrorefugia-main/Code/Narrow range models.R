##### Creating a map of the cape using the QDS grids###

#1)get libraries
#2) create objects 
#3) plot all maps in two panels 

#load libraries

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
library(MuMIn)
library(AICcmodavg)
library(spdep)
library(spatialreg)


#Cleaning the data


#Make all NAs =0 for fct
CFR_data <- CFR_data %>% 
  mutate(Facultative_species = coalesce(Facultative_species, 0))

#Make all NAs =0 for oblig
CFR_data <- CFR_data %>% 
  mutate(Obligate_species = coalesce(Obligate_species, 0))

#Make all NAs =0 for wetland
CFR_data <- CFR_data %>% 
  mutate(percent_wetland = coalesce(percent_wetland, 0))


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------Analysis ---------------------------------------------


##############################################################################
###############################################################################
##########################cORRELATION 

#Cleaning up the data
Mod.1_cor <- CFR_data %>% dplyr:: select(- Narrow_Range_endemics, -qdgc, -Facultative_species,-Obligate_species, 
                                         -Total_species_richness, -geometry)

Mod.1_cor = Mod.1_cor %>% st_set_geometry(NULL)
#Saved numbers 
M<-cor(Mod.1_cor)

head(round(M,1))
# as number
#corrplot(M, method="number")

# Plot the correlation matrix with larger squares and visible numbers
 plot_cor <- corrplot(
  M,
  method = "number",  # You can choose a different method based on your preference
  #type = "upper",     # Show upper triangular part of the matrix
  tl.cex = 0.7,       # Adjust the size of text labels
  tl.col = "black",   # Set text label color
  #tl.srt = 45,        # Rotate text labels for better visibility
  addCoef.col = "black",  # Set color for correlation coefficients
  number.cex = 0.7,       # Adjust the size of correlation coefficients
  col = colorRampPalette(c("#FFFFFF", "#67a9cf", "#ef8a62"))(100),
  addrect = 4, mar = c(0, 0, 0.0, 0)# Set color palette
)
 
#Z standardize the covariates  
 CFR_data_anlys <- CFR_data %>%
   mutate(across(6:30, scale))
 
##############################################################################
###############################################################################
##########################Narrow range endemics  
##############################################################################

#Climatic stability nested model
Clim.nre <- glm(Narrow_Range_endemics ~  Median_CloudFQ + percent_wetland + Mean_CSI
              , family = "quasipoisson" , data = CFR_data_anlys)
#Model summary 
summary(Clim.nre)

#water and energy hypothesis model
Prod.nre <- glm(Narrow_Range_endemics~ Mean_annual_Temperature + mean_annual_Rainfall + 
                Mean_PET + Mean_Soil_ph + Mean_Ext_P, family = "quasipoisson" , data = CFR_data_anlys)
#Model summary
summary(Prod.nre)

#Environmental heterogeneity Model
Het.nre <- glm(Narrow_Range_endemics ~
               Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
             , family = "quasipoisson" , data = CFR_data_anlys)
#Model summary
summary(Het.nre)

#full model
full.nre <- glm(Narrow_Range_endemics ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                  Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                  Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                , family = "quasipoisson" , data = CFR_data_anlys)

#Model summary
summary(full.nre)

                
# Data
# Narrow range endemics 
data <- data.frame(
  Estimates = c( 0.21848, 0.15789,  -0.38056, 
                 0.01901, 0.29968, 0.18392, -0.68180, -0.05345, 
                 0.30781, 0.33477, 0.02714, 
                 0.34790, 0.11088, -0.36793, 
                 0.04524,0.12238 ,0.17581,-0.28806,  
                 -0.03111, 0.37953, 0.18173, -0.11848),
  Covariates = c( 'Median CloudFQ', 'Wetland%', 'Mean CSI', 
                 'MAT', 'MAP', 'PET', 'pH', 'P',
                 'Range in MAT', 'Roughness in MDQ','Roughness in PET', 
                 'Median CloudFQ', 'Wetland%', 'Mean CSI',
                 'MAT', 'MAP', 'PET', 'pH', 'P',
                 'Range in MAT', 'Roughness in MDQ','Roughness in PET'),
  Model_Type = c( 'Climate stability', 'Climate stability', 'Climate stability',
                 'Productivity', 'Productivity', 'Productivity', 'Productivity','Productivity', 
                 'Environmental heterogeneity','Environmental heterogeneity','Environmental heterogeneity', 
                 'Full','Full','Full','Full','Full','Full','Full','Full','Full','Full','Full'), 
  Pvalue = c(0.03640, 0.02676, 0.00151,
                  0.862671, 0.000216, 0.032456, 0.000000326, 0.716262,
                  0.000603, 0.000176, 0.798093,0.029881,0.114972,0.003297,
                  0.703072,0.200296, 0.200231,0.037958,0.818461,0.000298, 0.063660, 0.306491))
# Calculate upper and lower bounds for error bars
#data$error_upper <- data$Estimates + sd_values
#data$error_lower <- data$Estimates - sd_values

#plot 
# Filter out rows with zero estimates
#non_zero_data <- reshaped_data %>%
  #filter(Estimates != 0)

add_asterisks <- function(pvalue) {
  if (pvalue <= 0.001) {
    return('***')
  } else if (pvalue <= 0.01) {
    return('**')
  } else if (pvalue <= 0.05) {
    return('*')
  } else {
    return('')
  }
}
#Add a new column for asterisks
data$asterisks <- sapply(data$Pvalue, add_asterisks)


# Set custom order for Model_Type
custom_order <- c("Climate stability","Productivity", "Environmental heterogeneity", "Full")
data$Model_Type <- factor(data$Model_Type, levels = custom_order)

# plot 
Narrow_range_endemics <- ggplot(data, aes(x = Model_Type, y = Estimates, fill = Covariates)) +
  geom_bar(position = position_dodge(preserve = "single"), width = 0.8, stat = 'identity',show.legend = FALSE) +
  geom_text(data = data, aes(label = sapply(Pvalue, add_asterisks)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, 
            size = 2, 
            fontface = "bold",
            angle = 90) +  
  labs(title = 'Quasi GLM',
       x = 'Model Type',
       y = 'Estimates', show.legend = FALSE) +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        text = element_text(size = 40),
        axis.title = element_text(size = 50, face = "bold"),
        axis.text = element_text(size = 50, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        plot.title = element_text(size = 100, face = "bold"))+
  coord_flip()


#ggsave("Output/Narrow Range model outputs.png", Narrow_range_endemics, width = 40, height = 18.7, units = "cm")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------Spatial autocorrelation  ------------------------

##############################################################################


#1) plot narrow range endemic map 
#2) Look at resisuals 
#3) Creat queen's neigbours 
#4) plot to check 
#5) then create W matrix of weights for each area
#6) Calculate Moran’s I
#7) If significant add errorsarlm and listw = w-matrix, method = "eigen", 
##    zero.policy = T or lagsarlm

#plot narrow range endemics 

#look at the model residuals 
plot(Full_mod.1$residuals)
plot()

#creating queen's neigbours 
Neighbours <- poly2nb(CFR_data_anlys)

#matrics of weights
listed_w <- spdep :: nb2listw(Neighbours, zero.policy = TRUE)

# Perform Moran's test
MoranI_Total <- moran.test(CFR_data_anlys$Narrow_Range_endemics, listed_w)

print(MoranI_Total)


#Checking for NAs
#any(is.na(CFR_data_fin$Narrow_range_spp.x))
#any(is.na(CFR_data_fin))
#any(is.na(listed_w))
#Fit the model

Neighbours <- poly2nb(CFR_data_anlys)

#matrics of weights
listed_w <- spdep :: nb2listw(Neighbours, zero.policy = TRUE)

narrow_clim.sp <- lagsarlm(Clim.nre, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

narrow_prod.sp <- lagsarlm(Prod.nre, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

narrow_het.sp <- lagsarlm(Het.nre, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

narrow_full.sp <- lagsarlm(full.nre, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# Data
# Narrow range endemics 
data2 <- data.frame(
  Estimates = c( 9.6298, 18.1556,  -11.4668, 
                 -9.1998, 28.4287, 18.5474, -35.7927, 10.3023, 
                 21.5857, 30.8551, -2.9428, 
                 18.6226, 17.1454, -20.2496, 
                 -4.6674,27.3585 ,25.7800,-5.2198,  
                 12.6301, 36.6768, 16.2389, -1.0623),
  Covariates = c( 'Median CloudFQ', 'Wetland%', 'Mean CSI', 
                  'MAT', 'MAP', 'PET', 'pH', 'P',
                  'Range in MAT', 'Roughness in MDQ','Roughness in PET', 
                  'Median CloudFQ', 'Wetland%', 'Mean CSI',
                  'MAT', 'MAP', 'PET', 'pH', 'P',
                  'Range in MAT', 'Roughness in MDQ','Roughness in PET'),
  Model_Type = c( 'Climate stability', 'Climate stability', 'Climate stability',
                  'Productivity', 'Productivity', 'Productivity', 'Productivity','Productivity', 
                  'Environmental heterogeneity','Environmental heterogeneity','Environmental heterogeneity', 
                  'Full','Full','Full','Full','Full','Full','Full','Full','Full','Full','Full'))


#Add a new column for asterisks
data2$asterisks <- sapply(data2$Pvalue, add_asterisks)

Y = ggplot(data2, aes(x = Model_Type, y = Estimates, fill = Covariates)) +
  geom_bar(position = position_dodge(preserve = "single"), width = 0.8, stat = 'identity') +
  labs(title = 'SAR',
       x = 'Model Type',
       y = 'Estimates', 
       show.legend = FALSE,
       plot.title = element_text(size = 100, face = "bold")) +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.y = element_blank(), 
        text = element_text(size = 40),
        axis.title = element_text(size = 50, face = "bold"),
        axis.text = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 100, face = "bold")) +
  coord_flip()


#plot_labels_p2 <- c(" ", "")

# Use ggarrange to arrange plots and add labels

Productivity_2 <- ggpubr::ggarrange(X, Y, labels = plot_labels_p2, label.x = 0.4, label.y = 0.9,  hjust = 0, nrow = 1)

ggsave("Output/Productivity_2.png", Productivity_2, width = 120, height = 90, units = "cm")


##############################################################################
###############################################################################
########################## Proceed to facultative script 