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


# Cleaning the data
CFR_data <- readRDS("Data/CFR_data")

CFR_data$Mean_Ext_P = log(CFR_data$Mean_Ext_P)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------Analysis ---------------------------------------------


#Z standardize the covariates  
CFR_data_anlys <- CFR_data %>%
  mutate(across(6:30, scale))


##############################################################################
###############################################################################
##########################Total richness models 
##############################################################################

#Climatic stability submodel
Clim.tot <- glm(Total_species_richness ~  Median_CloudFQ + percent_wetland + Mean_CSI
                , family = "quasipoisson" , data = CFR_data_anlys)
#Model summary 
summary(Clim.tot)

#water and energy hypothesis submodel
Prod.tot <- glm(Total_species_richness~ Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P, family = "quasipoisson" , data = CFR_data_anlys)
#Model summary
summary(Prod.tot)

#Environmental heterogeneity Model
Het.tot <- glm(Total_species_richness ~
                 Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
               , family = "quasipoisson" , data = CFR_data_anlys)
#Model summary
summary(Het.tot)

#full model
full.tot <- glm(Total_species_richness ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                  Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                  Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                , family = "quasipoisson" , data = CFR_data_anlys)

#Model summary
summary(full.tot)


# Now we plot the output:
# ***************************************************************************
# the following piece of code extracts estimates and coefficients
# ************************************************************************

# Data
# total spp 
climate= summary(Clim.tot)$coefficients %>% data.frame()
climate = climate[-1,]
climate$Covariates = c('Median CloudFQ', 'Wetland%', 'Mean CSI')


productivity= summary(Prod.tot)$coefficients %>% data.frame()
productivity = productivity[-1,]
productivity$Covariates = c('MAT', 'MAP', 'PET', 'pH', 'P')

Heterogeneity = summary(Het.tot)$coefficients %>% data.frame()
Heterogeneity = Heterogeneity[-1,]
Heterogeneity$Covariates = c('Range in MAT', 'Roughness in MDQ','Roughness in PET')

Full = summary(full.tot)$coefficients %>% data.frame()
Full = Full[-1,]
Full$Covariates = c('Median CloudFQ', 'Wetland%', 'Mean CSI',
                 'MAT', 'MAP', 'PET', 'pH', 'P',
                 'Range in MAT', 'Roughness in MDQ','Roughness in PET')

Totalspp = rbind(climate,productivity,Heterogeneity,Full)

Totalspp$Model_Type = c( 'Climate stability', 'Climate stability', 'Climate stability',
                  'Productivity', 'Productivity', 'Productivity', 'Productivity','Productivity', 
                  'Environmental heterogeneity','Environmental heterogeneity','Environmental heterogeneity', 
                  'Full','Full','Full','Full','Full','Full','Full','Full','Full','Full','Full') 

# Calculate upper and lower bounds for error bars
#data$error_upper <- data$Estimates + sd_values
#data$error_lower <- data$Estimates - sd_values

# we then create a function to represent significance through "*"

add_asterisks <- function(Pr...t..) {
  if (Pr...t.. <= 0.001) {
    return('***')
  } else if (Pr...t.. <= 0.01) {
    return('**')
  } else if (Pr...t.. <= 0.05) {
    return('*')
  } else {
    return('')
  }
}
#Add a new column for asterisks
Totalspp$asterisks <- sapply(Totalspp$Pr...t.., add_asterisks)


# Set custom order for Model_Type
custom_order <- c("Climate stability","Productivity", "Environmental heterogeneity", "Full")
Totalspp$Model_Type <- factor(Totalspp$Model_Type, levels = custom_order)

# plot 
 
total_spp_model = ggplot(Totalspp, aes(x = Model_Type, y = Estimate, fill = Covariates)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single"), width = 1) +
  geom_text(aes(label = asterisks, vjust = ifelse(Totalspp$Estimate >= 0, 1, -0.2)),
            position = position_dodge2(width = 1, preserve = "single"),
            size = 20, angle = 90) +
  labs(title = 'Quasi GLM',
       x = 'Model Type',
       y = 'Estimates', show.legend = FALSE) +
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))+
  coord_flip()+
  theme(legend.position = "none")  # Remove the legend



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

#creating queen's neigbours 
Neighbours <- poly2nb(CFR_data_anlys)

#matrics of weights
listed_w <- spdep :: nb2listw(Neighbours, zero.policy = TRUE)

# Perform Moran's test
MoranI_Total <- moran.test(CFR_data_anlys$Total_species_richness, listed_w)

print(MoranI_Total)

#Contact a Lagrange Multiplier test, to test spatial dependence in linear models

#CHECKING spatial autocorrelation (fOR EACH MODEL AND EACH RESPONSE) : 
# 1) residuals 
# 2) response 
# 3) The adjusted residuals 
# 4) The adjusted response 
# 5) The adjusted residuals and response (SARMA) 

# Fit Rao's score for the climatic stability - narrow range 
Clim.lagrange.tot <- lm.RStests(Clim.tot, listed_w, 
                                test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))

# Only adjusted the resp and adj show SPAT COR. 
summary(Clim.lagrange.tot)

# Fit Rao's score for the productivity - narrow range 
Prod.lagrange.tot <- lm.RStests(Prod.tot, listed_w, 
                                test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))

# No SPAT COR on all models but SAMAR is more unlikely 
summary(Prod.lagrange.tot)

# Fit Rao's score for the Heterogeneity - narrow range 
HET.lagrange.tot <- lm.RStests(Het.tot, listed_w, 
                               test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))

# No SPAT COR on across
summary(HET.lagrange.tot)

# Fit Rao's score for the Full - narrow range 
full.lagrange.tot <- lm.RStests(full.tot, listed_w, 
                                test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))

# only SPAT COR on :errors and slightly on adj errors 
summary(full.lagrange.tot)



# now fit a Spatial simultaneous 
# autoregressive model estimation by maximum likelihood :
# where : ρ and λ are found by nlminb or optim() first, and 
# β and other parameters by generalized least squares subsequently.

# Climatic stability 
tot_clim.sp <- sacsarlm(Clim.tot, data= CFR_data_anlys, listed_w, zero.policy=TRUE)
# outputs
summary(tot_clim.sp)

# Productivity  
tot_prod.sp <- sacsarlm(Prod.tot, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# outputs
summary(tot_prod.sp)

# Heterogeneity 
tot_het.sp <- sacsarlm(Het.tot, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# outputs
summary(tot_het.sp)

# Full model 
tot_full.sp <- sacsarlm(full.tot, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# outputs
summary(tot_full.sp)

# Now we plot the residuals:
# ***************************************************************************
# the following piece of code extracts log Likelihoods and AIC, and calculates
# the other things needed for a AIC based model selection.
# In the end, it produces a model selection table
# ************************************************************************

# Data
# Narrow range endemics 
climate.spt= summary(tot_clim.sp)$Coef %>% data.frame()
climate.spt = climate.spt[-1,]
climate.spt$Covariates = c('Median CloudFQ', 'Wetland%', 'Mean CSI')

productivity.spt= summary(tot_prod.sp)$Coef %>% data.frame()
productivity.spt = productivity.spt[-1,]
productivity.spt$Covariates = c('MAT', 'MAP', 'PET', 'pH', 'P')

Heterogeneity.spt = summary(tot_het.sp)$Coef %>% data.frame()
Heterogeneity.spt = Heterogeneity.spt[-1,]
Heterogeneity.spt$Covariates = c('Range in MAT', 'Roughness in MDQ','Roughness in PET')

Full.spt = summary(tot_full.sp)$Coef %>% data.frame()
Full.spt = Full.spt[-1,]
Full.spt$Covariates = c('Median CloudFQ', 'Wetland%', 'Mean CSI',
                    'MAT', 'MAP', 'PET', 'pH', 'P',
                    'Range in MAT', 'Roughness in MDQ','Roughness in PET')

Totalspp.spt = rbind(climate.spt,productivity.spt,Heterogeneity.spt,Full.spt)

Totalspp.spt$Model_Type = c( 'Climate stability', 'Climate stability', 'Climate stability',
                         'Productivity', 'Productivity', 'Productivity', 'Productivity','Productivity', 
                         'Environmental heterogeneity','Environmental heterogeneity','Environmental heterogeneity', 
                         'Full','Full','Full','Full','Full','Full','Full','Full','Full','Full','Full') 


#Add a new column for asterisks
Totalspp.spt$asterisks <- sapply(Totalspp.spt$Pr...z.., add_asterisks)


# Set custom order for Model_Type
custom_order <- c("Climate stability","Productivity", "Environmental heterogeneity", "Full")
Totalspp.spt$Model_Type <- factor(Totalspp.spt$Model_Type, levels = custom_order)

# plot 

Totalspp_Model_sp = ggplot(Totalspp.spt, aes(x = Model_Type, y = Estimate, fill = Covariates)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single"), width = 1) +
  geom_text(aes(label = asterisks, vjust = ifelse(Totalspp.spt$Estimate >= 0, 1, -0.2)),
            position = position_dodge2(width = 1, preserve = "single"),
            size = 20, angle = 90)  +
  labs(title = 'SARMA Model',
       x = '',
       y = 'Estimates', show.legend = FALSE) +
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5)) +
  coord_flip()


#plot_labels_p2 <- c(" ", "")

# Use ggarrange to arrange plots and add labels

TOTAL_MODEL <- ggpubr::ggarrange(total_spp_model, Totalspp_Model_sp, label.x = 0.4, label.y = 0.9,  hjust = 0, nrow = 1)

ggsave("Output/total_model_outputs.png", TOTAL_MODEL, width = 120, height = 90, units = "cm")


##############################################################################
###############################################################################
########################## Proceed to : 2 narrow range endemics modeling


