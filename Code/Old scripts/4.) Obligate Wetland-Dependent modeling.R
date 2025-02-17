

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
library(lmtest)


# Cleaning the data
CFR_data <- readRDS("Data/CFR_data")
CFR_data$Mean_Ext_P = log(CFR_data$Mean_Ext_P)


#Z standardize the covariates  
CFR_data_anlys <- CFR_data %>%
  mutate(across(6:30, scale))



##############################################################################
###############################################################################
##########################Obligate models 
##############################################################################

#Climatic stability submodel
Clim.ob <- glm(Obligate_species ~  Median_CloudFQ + percent_wetland + Mean_CSI
                , family = "quasipoisson" , data = CFR_data_anlys)
#Model summary 
summary(Clim.ob)

#water and energy hypothesis submodel
Prod.ob <- glm(Obligate_species~ Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P, family = "quasipoisson" , data = CFR_data_anlys)
#Model summary
summary(Prod.ob)

#Environmental heterogeneity Model
Het.ob <- glm(Obligate_species ~
                 Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
               , family = "quasipoisson" , data = CFR_data_anlys)
#Model summary
summary(Het.ob)

#full model
full.ob <- glm(Obligate_species ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                  Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                  Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                , family = "quasipoisson" , data = CFR_data_anlys)

#Model summary
summary(full.ob)



# Now we plot the residuals:
# ***************************************************************************
# the following piece of code extracts log Likelihoods and AIC, and calculates
# the other things needed for a AIC based model selection.
# In the end, it produces a model selection table
# ************************************************************************

# Data
# Narrow range spp 
climate= summary(Clim.ob)$coefficients %>% data.frame()
climate = climate[-1,]
climate$Covariates = c('Median CloudFQ', 'Wetland%', 'Mean CSI')

productivity= summary(Prod.ob)$coefficients %>% data.frame()
productivity = productivity[-1,]
productivity$Covariates = c('MAT', 'MAP', 'PET', 'pH', 'P')

Heterogeneity = summary(Het.ob)$coefficients %>% data.frame()
Heterogeneity = Heterogeneity[-1,]
Heterogeneity$Covariates = c('Range in MAT', 'Roughness in MDQ','Roughness in PET')

Full = summary(full.ob)$coefficients %>% data.frame()
Full = Full[-1,]
Full$Covariates = c('Median CloudFQ', 'Wetland%', 'Mean CSI',
                    'MAT', 'MAP', 'PET', 'pH', 'P',
                    'Range in MAT', 'Roughness in MDQ','Roughness in PET')

obg = rbind(climate,productivity,Heterogeneity,Full)

obg$Model_Type = c( 'Climate stability', 'Climate stability', 'Climate stability',
                         'Productivity', 'Productivity', 'Productivity', 'Productivity','Productivity', 
                         'Environmental heterogeneity','Environmental heterogeneity','Environmental heterogeneity', 
                         'Full','Full','Full','Full','Full','Full','Full','Full','Full','Full','Full') 

# Calculate upper and lower bounds for error bars
#data$error_upper <- data$Estimates + sd_values
#data$error_lower <- data$Estimates - sd_values

#plot 
# Filter out rows with zero estimates
#non_zero_data <- reshaped_data %>%
#filter(Estimates != 0)

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
obg$asterisks <- sapply(obg$Pr...t.., add_asterisks)


# Set custom order for Model_Type
custom_order <- c("Climate stability","Productivity", "Environmental heterogeneity", "Full")
obg$Model_Type <- factor(obg$Model_Type, levels = custom_order)

# plot 

obg_glm = ggplot(obg, aes(x = Model_Type, y = Estimate, fill = Covariates)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single"), width = 1) +
  geom_text(aes(label = asterisks, vjust = ifelse(obg$Estimate >= 0, 1, -0.2)),
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
  theme(legend.position = "none") # Remove the legend # Remove the legend


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
MoranI_Total <- moran.test(CFR_data_anlys$Obligate_species, listed_w)

print(MoranI_Total)

#Contact a Lagrange Multiplier test, to test spatial dependence in linear models

#CHECKING spatial autocorrelation (fOR EACH MODEL AND EACH RESPONSE) : 
# 1) residuals 
# 2) response 
# 3) The adjusted residuals 
# 4) The adjusted response 
# 5) The adjusted residuals and response (SARMA) 

# Fit Rao's score for the climatic stability - narrow range 
Clim.lagrange.ob <- lm.LMtests(Clim.ob, listed_w, 
                                test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))

# Only adjusted error and response show SPAT COR. 
summary(Clim.lagrange.ob)

# Fit Rao's score for the productivity - narrow range 
Prod.lagrange.ob <- lm.LMtests(Prod.ob, listed_w, 
                                test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))

# No SPAT COR on all models but SAMAR is more unlikely 
summary(Prod.lagrange.ob)

# Fit Rao's score for the Heterogeneity - narrow range 
HET.lagrange.ob <- lm.LMtests(Het.ob, listed_w, 
                               test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))

# No SPAT COR on : SAMAR, SPLAG & SPERR
summary(HET.lagrange.ob)

# Fit Rao's score for the Full - narrow range 
full.lagrange.ob <- lm.LMtests(full.ob, listed_w, 
                                test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))

# only SPAT COR on :resp
summary(full.lagrange.ob)



# now fit a Spatial simultaneous 
# autoregressive model estimation by maximum likelihood :
# where : ρ and λ are found by nlminb or optim() first, and 
# β and other parameters by generalized least squares subsequently.

# Climatic stability 
clim.ob_sp <- sacsarlm(Clim.ob, data= CFR_data_anlys, listed_w, zero.policy=TRUE)
# outputs
summary(clim.ob_sp)

# Productivity  
prod.ob_sp <- sacsarlm(Prod.ob, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# outputs
summary(prod.ob_sp)

# Heterogeneity 
het.ob_sp<- sacsarlm(Het.ob, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# outputs
summary(het.ob_sp)

# Full model 
full.ob_sp <- sacsarlm(full.ob, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# outputs
summary(full.ob_sp)

# Now we plot the residuals:
# ***************************************************************************
# the following piece of code extracts log Likelihoods and AIC, and calculates
# the other things needed for a AIC based model selection.
# In the end, it produces a model selection table
# ************************************************************************

# Data
# Narrow range endemics 
climate.spt= summary(clim.ob_sp)$Coef %>% data.frame()
climate.spt = climate.spt[-1,]
climate.spt$Covariates = c('Median CloudFQ', 'Wetland%', 'Mean CSI')

productivity.spt= summary(prod.ob_sp)$Coef %>% data.frame()
productivity.spt = productivity.spt[-1,]
productivity.spt$Covariates = c('MAT', 'MAP', 'PET', 'pH', 'P')

Heterogeneity.spt = summary(het.ob_sp)$Coef %>% data.frame()
Heterogeneity.spt = Heterogeneity.spt[-1,]
Heterogeneity.spt$Covariates = c('Range in MAT', 'Roughness in MDQ','Roughness in PET')

Full.spt = summary(full.ob_sp)$Coef %>% data.frame()
Full.spt = Full.spt[-1,]
Full.spt$Covariates = c('Median CloudFQ', 'Wetland%', 'Mean CSI',
                        'MAT', 'MAP', 'PET', 'pH', 'P',
                        'Range in MAT', 'Roughness in MDQ','Roughness in PET')

obg.spt = rbind(climate.spt,productivity.spt,Heterogeneity.spt,Full.spt)

obg.spt$Model_Type = c( 'Climate stability', 'Climate stability', 'Climate stability',
                             'Productivity', 'Productivity', 'Productivity', 'Productivity','Productivity', 
                             'Environmental heterogeneity','Environmental heterogeneity','Environmental heterogeneity', 
                             'Full','Full','Full','Full','Full','Full','Full','Full','Full','Full','Full') 

add_asterisks_z <- function(Pr...z..) {
  if (Pr...z.. <= 0.001) {
    return('***')
  } else if (Pr...z.. <= 0.01) {
    return('**')
  } else if (Pr...z.. <= 0.05) {
    return('*')
  } else {
    return('')
  }
}
#Add a new column for asterisks
obg.spt$asterisks <- sapply(obg.spt$Pr...z.., add_asterisks_z)

# Set custom order for Model_Type
custom_order <- c("Climate stability","Productivity", "Environmental heterogeneity", "Full")
obg.spt$Model_Type <- factor(obg.spt$Model_Type, levels = custom_order)


# plot 

OBG_SARMA = ggplot(obg.spt, aes(x = Model_Type, y = Estimate, fill = Covariates)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single"), width = 1) +
  geom_text(aes(label = asterisks, vjust = ifelse(obg.spt$Estimate >= 0, 1, -0.2)),
            position = position_dodge2(width = 1, preserve = "single"),
            size = 20, angle = 90) +
  labs(title = 'Quasi GLM',
       x = '',
       y = 'Estimates', show.legend = FALSE) +
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        axis.title = element_text(size = 50, face = "bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))+
  coord_flip()


#plot_labels_p2 <- c(" ", "")

# Use ggarrange to arrange plots and add labels

OBG_MODELS <- ggpubr::ggarrange(obg_glm, OBG_SARMA, label.x = 0.4, label.y = 0.9,  hjust = 0, nrow = 1)

ggsave("Output/Obligate.png", OBG_MODELS, width = 120, height = 90, units = "cm")


##############################################################################
###############################################################################
########################## Proceed to :5.) Model selection  


