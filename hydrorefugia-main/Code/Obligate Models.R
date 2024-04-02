

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


CFR_data_fin <- readRDS("CFR_data_fin")


#Z standardize the covariates  
CFR_data_anlys <- CFR_data %>%
  mutate(across(6:30, scale))



##############################################################################
###############################################################################
##########################Facultative 
##############################################################################

#Climatic stability nested model
Clim.fct <- glm(Facultative_species ~  Median_CloudFQ + percent_wetland + Mean_CSI
                , family = "quasipoisson" , data = CFR_data_anlys)
#Model summary 
summary(Clim.fct)

#water and energy hypothesis model
prod.fct <- glm(Facultative_species~ Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P, family = "quasipoisson" , data = CFR_data_anlys)
#Model summary
summary(prod.fct)

#Environmental heterogeneity Model
Het.fct <- glm(Facultative_species ~
                 Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
               , family = "quasipoisson" , data = CFR_data_anlys)
#Model summary
summary(Het.fct)

#full model
full.fct <- glm(Facultative_species ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                  Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                  Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                , family = "quasipoisson" , data = CFR_data_anlys)

#Model summary
summary(full.fct)

###################creating a plot
# Data
# Narrow range endemics 
data_f <- data.frame(
  Estimates = c( 0.42109, 0.08547,  -0.63312, 
                 0.002382, 0.479219, 0.255141, -0.729576, -0.175151, 
                 0.20797, 0.41899, 0.15063, 
                 0.365855, -0.009933, -0.563951, 
                 0.050995,0.289679 ,0.111986,-0.332491,  
                 -0.127026, 0.350554, 0.180530, -0.064208),
  Covariates = c( 'Median CloudFQ', 'Wetland%', 'Mean CSI', 
                  'MAT', 'MAP', 'PET', 'pH', 'P',
                  'Range in MAT', 'Roughness in MDQ','Roughness in PET', 
                  'Median CloudFQ', 'Wetland%', 'Mean CSI',
                  'MAT', 'MAP', 'PET', 'pH', 'P',
                  'Range in MAT', 'Roughness in MDQ','Roughness in PET'),
  Model_Type = c( 'Climate stability', 'Climate stability', 'Climate stability',
                  'Productivity', 'Productivity', 'Productivity', 'Productivity','Productivity', 
                  'Environmental heterogeneity','Environmental heterogeneity','Environmental heterogeneity', 
                  'Full','Full','Full','Full','Full','Full','Full','Full','Full','Full','Full')
)

#sd_values <- c(0.02731, 0.02675, 0.02324, 
#-0.01934, 0.02401, 0.02381, 0.02717, -0.02717,
#0.04730, 0.02326, -0.01925,
#0.03079, 0.02683, 0.02913, 0.01984,
#-0.02166, -0.04435,-0.04435, 0.01984,
#-0.05116, 0.03306, 0.03042 
# )

# Calculate upper and lower bounds for error bars
#data$error_upper <- data$Estimates + sd_values
#data$error_lower <- data$Estimates - sd_values

#plot 
# Filter out rows with zero estimates
#non_zero_data <- reshaped_data %>%
#filter(Estimates != 0)

# Set custom order for Model_Type
custom_order <- c("Climate stability","Productivity", "Environmental heterogeneity", "Full")
data_f$Model_Type <- factor(data$Model_Type, levels = custom_order)

# plot 
Factdep_nre <- ggplot(data_f, aes(x = Model_Type, y = Estimates, fill = Covariates)) +
  geom_bar(position = position_dodge(preserve = "single"), width = 0.8, stat = 'identity') +
  labs(title = '',
       x = 'Model Type',
       y = 'Estimates') +
  theme(panel.background = element_rect(fill = "white")) +
  theme(
    panel.background = element_rect(fill = "white"),
    text = element_text(size = 16),  # Adjust the overall text size
    axis.title = element_text(size = 18),  # Adjust axis title size
    axis.text = element_text(size = 16),  # Adjust axis text size
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),  # Optional margin adjustment for y-axis title
    plot.title = element_text(size = 20, face = "bold")  # Adjust plot title size and style
  ) +
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

#creating queen's neigbours 
Neighbours <- poly2nb(CFR_data_anlys)

#matrics of weights
listed_w <- spdep :: nb2listw(Neighbours, zero.policy = TRUE)

# Perform Moran's test
MoranI_Total_fct <- moran.test(CFR_data_anlys$Facultative_species, listed_w)

print(MoranI_Total_fct)


#Checking for NAs
#any(is.na(CFR_data_fin$Narrow_range_spp.x))
#any(is.na(CFR_data_fin))
#any(is.na(listed_w))
#Fit the model

#Neighbours <- poly2nb(CFR_data_anlys)

#matrics of weights
listed_w <- spdep :: nb2listw(Neighbours, zero.policy = TRUE)

fct_clim.sp <- lagsarlm(Clim.fct, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

#model outputs
summary(fct_clim.sp)

fct_prod.sp <- lagsarlm(prod.fct, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

#model outputs
summary(fct_prod.sp)

fct_het.sp <- lagsarlm(Het.fct, data= CFR_data_anlys, listed_w, zero.policy=TRUE)
#model outputs
summary(fct_het.sp)

fct_full.sp <- lagsarlm(full.fct, data= CFR_data_anlys, listed_w, zero.policy=TRUE)
summary(fct_full.sp)

# Data
# Narrow range endemics 
data2_f <- data.frame(
  Estimates = c( 0.85497, 0.71911,  -0.87104, 
                 -0.67371,2.40213, 1.34460, -1.92011, 0.83297, 
                 0.7644031  , 2.1890362  , -0.0086955  , 
                 1.097422   , 0.750700, -1.054043, 
                 -0.191020 ,2.065981    ,1.551955   ,-0.060946,  
                 1.022375   , 1.783716   , 1.473445   , 0.056084   ),
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


# Set custom order for Model_Type
newdata_f <- rbind(data_f, data2_f)

# Add a new column to indicate the source of the data (for coloring purposes)
newdata_f$Source <- rep(c("Quasi Model", "Spatial lag Model"), each = nrow(data))

# Plot using facet_wrap
fct.models <- ggplot(newdata_f, aes(x = Estimates, y = Model_Type, fill = Covariates)) +
  geom_bar(position = position_dodge(preserve = "single"), width = 0.8, stat = 'identity') +
  facet_wrap(~ Source, scales = "free", ncol = 2) +  # Facet by the 'Source' column
  labs(title = '',
       x = 'Estimates',
       y = 'Model Type') +
  theme(panel.background = element_rect(fill = "white")) +
  theme(
    panel.background = element_rect(fill = "white"),
    text = element_text(size = 40),
    axis.title = element_text(size = 50, face = "bold"),
    axis.text = element_text(size = 50, face = "bold"),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    plot.title = element_text(size = 100, face = "bold")
  )

ggsave("fct.models.png", fct.models, width = 120, height = 90, units = "cm")

##############################################################################
###############################################################################
########################## Proceed to facultative script 