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
library(stargazer)


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
                 'Full','Full','Full','Full','Full','Full','Full','Full','Full','Full','Full')
)

sd_values <- c(0.02731, 0.02675, 0.02324, 
               -0.01934, 0.02401, 0.02381, 0.02717, -0.02717,
               0.04730, 0.02326, -0.01925,
               0.03079, 0.02683, 0.02913, 0.01984,
               -0.02166, -0.04435,-0.04435, 0.01984,
               -0.05116, 0.03306, 0.03042 
              )

# Calculate upper and lower bounds for error bars
data$error_upper <- data$Estimates + sd_values
data$error_lower <- data$Estimates - sd_values

#plot 
# Filter out rows with zero estimates
non_zero_data <- reshaped_data %>%
  filter(Estimates != 0)

# Set custom order for Model_Type
custom_order <- c("Climate stability","Productivity", "Environmental heterogeneity", "Full")
data$Model_Type <- factor(data$Model_Type, levels = custom_order)

# plot 
Narrow_range_endemics <- ggplot(data, aes(x = Model_Type, y = Estimates, fill = Covariates)) +
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


ggsave("Output/Narrow Range model outputs.png", clim.model, width = 40, height = 18.7, units = "cm")

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


# Set custom order for Model_Type
newdata <- rbind(data, data2)

# Add a new column to indicate the source of the data (for coloring purposes)
newdata$Source <- rep(c("Narrow range endemics", "Narrow range endemics spatial lag"), each = nrow(data))

# Plot using facet_wrap
nre.models <- ggplot(newdata, aes(x = Estimates, y = Model_Type, fill = Covariates)) +
  geom_bar(position = position_dodge(preserve = "single"), width = 0.8, stat = 'identity') +
  facet_wrap(~ Source, scales = "free", ncol = 2) +  # Facet by the 'Source' column
  labs(title = '',
       x = 'Estimates',
       y = 'Model Type') +
  theme(panel.background = element_rect(fill = "white")) +
  theme(
    panel.background = element_rect(fill = "white"),
    text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    plot.title = element_text(size = 20, face = "bold")
  )


ggsave("nre.models.png", nre.models, width = 60, height = 30, units = "cm")

##############################################################################
###############################################################################
##########################Facultative 
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
Het.m <- glm(Narrow_Range_endemics ~
               Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
             , family = "quasipoisson" , data = CFR_data_anlys)
#Model summary
summary(Het.m)

#full model
full.nre <- glm(Narrow_Range_endemics ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                  Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                  Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                , family = "quasipoisson" , data = CFR_data_anlys)

#Model summary
summary(full.nre)

###################creating a plot

# Data
# facultative
data_f <- data.frame(
  Estimates = c(-0.34092, 0.27853, 0.11727, -0.065683, 
                0.341122, 0.186342 , 0.005223, -0.360207, 0.29113 , 0.17304,
               -0.14198, 0.31542, 0.12613, 0.12196, -0.18773,
                0.07201, -0.12625, 0.11320, 0.06156, 0.10270),
  Covariates = c( 'CSI', 'CloudFQ', 'Wetland%', 
                 'MAT', 'MAP', 'PET', 'pH', 'P', 'Range in MAT', 'Roughness in MAP', 
                  'MAT', 'MAP', 'Range in MAT','Roughness in MAT', 'P', 'CloudFQ', 
                 'CSI', 'pH','Wetland%', 'PET'),
  Model_Type = c( 'Climate stability', 'Climate stability', 'Climate stability',
                  'Productivity', 'Productivity', 'Productivity', 'Productivity',
                 'Productivity', 'Environmental heterogeneity', 
                 'Environmental heterogeneity', 'Full','Full','Full','Full','Full','Full'
                 ,'Full','Full','Full','Full')
  
)
#plot 
Prod.model <- ggplot(data_f, aes(x = Estimates, y = Model_Type, fill = Covariates)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.8, lwd = 0) +
  labs(title = 'b)',
       x = 'Estimates',
       y = 'Model Type')+
  coord_flip()+
  theme(panel.background = element_rect(fill = "white"))+
  theme(
    panel.background = element_rect(fill = "white"),
    text = element_text(size = 16),  # Adjust the overall text size
    axis.title = element_text(size = 18),  # Adjust axis title size
    axis.text = element_text(size = 16),  # Adjust axis text size
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),  # Optional margin adjustment for y-axis title
    plot.title = element_text(size = 20, face = "bold")   # Adjust plot title size and style
  )

ggsave("Output/Facultative model outputs.png", Prod.model, width = 40, height = 18.7, units = "cm")


###########################################
##############################################################################
###############################################################################
##########################obligate 
##############################################################################


#obligate

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
Het.m <- glm(Narrow_Range_endemics ~
               Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
             , family = "quasipoisson" , data = CFR_data_anlys)
#Model summary
summary(Het.m)

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
                  'Full','Full','Full','Full','Full','Full','Full','Full','Full','Full','Full')
)

sd_values <- c(0.02731, 0.02675, 0.02324, 
               -0.01934, 0.02401, 0.02381, 0.02717, -0.02717,
               0.04730, 0.02326, -0.01925,
               0.03079, 0.02683, 0.02913, 0.01984,
               -0.02166, -0.04435,-0.04435, 0.01984,
               -0.05116, 0.03306, 0.03042 
)

# Calculate upper and lower bounds for error bars
data$error_upper <- data$Estimates + sd_values
data$error_lower <- data$Estimates - sd_values

#plot 
# Filter out rows with zero estimates
non_zero_data <- reshaped_data %>%
  filter(Estimates != 0)

# Set custom order for Model_Type
custom_order <- c("Climate stability","Productivity", "Environmental heterogeneity", "Full")
data$Model_Type <- factor(data$Model_Type, levels = custom_order)

# plot 
Narrow_range_endemics <- ggplot(data, aes(x = Model_Type, y = Estimates, fill = Covariates)) +
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
###################creating a plot

# Data
# facultative
data_f <- data.frame(
  Estimates = c( -0.43527, 0.23277, 0.08888, 0.08469, 
                0.17545, 0.28564 , -0.21964, -0.35852, 0.28410 ,  0.11781,
                 -0.09230, 0.13107, 0.10497, 0.10465, -0.13856,
                0.29655, -0.24691, 0.14754, 0.06582, 0.32797),
  Covariates = c( 'CSI', 'CloudFQ', 'Wetland%', 
                  'MAT', 'MAP', 'PET', 'pH', 'P', 'Range in MAT', 'Roughness in MAP', 
                  'MAT', 'MAP', 'Range in MAT','Roughness in MAT', 'P', 'CloudFQ', 
                  'CSI', 'pH','Wetland%', 'PET'),
  Model_Type = c( 'Climate stability', 'Climate stability', 'Climate stability',
                  'Productivity', 'Productivity', 'Productivity', 'Productivity',
                  'Productivity', 'Environmental heterogeneity', 
                  'Environmental heterogeneity', 'Full','Full','Full','Full','Full','Full'
                  ,'Full','Full','Full','Full')
  
)
#plot 
het.model <- ggplot(data_f, aes(x = Estimates, y = Model_Type, fill = Covariates)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.8, lwd = 0) +
  labs(title = 'c)',
       x = 'Estimates',
       y = 'Model Type')+
  coord_flip()+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 16),  # Adjust the overall text size
        axis.title = element_text(size = 18),  # Adjust axis title size
        axis.text = element_text(size = 16),  # Adjust axis text size
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),  # Optional margin adjustment for y-axis title
        plot.title = element_text(size = 20, face = "bold")   # Adjust plot title size and style
  )

ggsave("Output/Obligate model outputs.png", het.model, width = 40, height = 18.7, units = "cm")

