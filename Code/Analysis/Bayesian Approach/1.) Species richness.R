##### analysis of CFR Data

# Fit a spatial generalised linear mixed model to areal unit data, 
# where the response variable is Poisson.
# The linear predictor is modelled by known covariates and a vector of random effects. 
# The modelling is by the conditional autoregressive prior proposed by 
# Leroux et al. (2000).
# Independent random effects can be obtained by setting rho=0, while
# the intrinsic CAR model can be obtained by setting rho=1. 
#I nference is conducted in a Bayesian setting using Markov chain Monte Carlo (MCMC) simulation.

# 1) Get libraries
# 2) Read in CFR data 
# 3) model according to the best model according to AIC 
# 4) Spatial autocorrelation
# 4a) create neighbors 
# 4b) create a list 
# 4c) create weights 
# 4d) create a matrix
# 5) fit the model using the package CARbayes 

#load libraries

library(tidyverse)
#library(readxl)
#library(readr)
library(sf)
#library(units)
#library(terra)
#library(mapview)
#library(viridis)
#library(tidyr)
library(ggpubr)
library(dplyr)
#library(corrplot)
#library(MuMIn)
#library(AICcmodavg)
library(spdep)
library(spatialreg)
library(CARBayes)


#Z standardize CFR data 
CFR_data_anlys <- readRDS("Data/CFR_data_anlys")

# Removing high value data (cut off 1300 ?)

CFR_data_anlys <-  CFR_data_anlys %>%
  filter(Total_species_richness < 1300)

##############################################################################
###############################################################################
##########################The best Total richness model 
##############################################################################


# full model
clim.tot <- glm(Total_species_richness ~ Median_CloudFQ + percent_wetland + Mean_CSI
                , family = "quasipoisson" , data = CFR_data_anlys)

# Model summary
summary(clim.tot)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------Spatial autocorrelation  ------------------------

##############################################################################

#creating queen's neigbours 
Neighbours <- poly2nb(CFR_data_anlys)

# Create weights list object
listed_w <- nb2listw(Neighbours, zero.policy = TRUE)

# Convert to spatial weights matrix
W <-nb2mat(Neighbours, style = "B", zero.policy = TRUE)

# Check the structure of the weights matrix
str(W)

# Fit the Leroux CAR model
SR <- S.CARleroux(formula = CFR_data_anlys$Total_species_richness ~ 
                     CFR_data_anlys$Median_CloudFQ + 
                     CFR_data_anlys$percent_wetland + 
                     CFR_data_anlys$Mean_CSI+
                     CFR_data_anlys$Mean_annual_Temperature + 
                     CFR_data_anlys$mean_annual_Rainfall + 
                     CFR_data_anlys$Mean_PET + 
                     CFR_data_anlys$Mean_Soil_ph + 
                     CFR_data_anlys$Mean_Ext_P +
                     CFR_data_anlys$Range_Temperature + 
                     CFR_data_anlys$Roughness_Dry_Rainfall + 
                     CFR_data_anlys$PET_Roughness,
                   family = "poisson",
                   W = W,
                   burnin = 500000,
                   n.sample = 1000000,
                   thin = 500,
                   n.cores = 3,
                   n.chains = 3)


# Summarize the model
print(SR)

# Check burnin and chain stability
plot(SR$samples$beta)

#saving the plot 


# Species richness data cleaning 

#extract values from model 

Full <- SR$summary.results %>% as.data.frame()
Full = Full[-1,]
Full = Full[-12,]
Full = Full[-12,]
Full$Covariates = c('Median CloudFQ', 'Wetland%', 'Mean CSI',
                        'MAT', 'MAP', 'PET', 'pH', 'P',
                        'Range in MAT', 'Roughness in MDQ','Roughness in PET')

#Totalspp = rbind(climate,productivity,Heterogeneity,Full)

Full$Model_Type = rep('Total species richness', 11)

# Set custom order for Model_Type
custom_order <- c('Median CloudFQ', 'Wetland%', 'Mean CSI',
                  'MAT', 'MAP', 'PET', 'pH', 'P',
                  'Range in MAT', 'Roughness in MDQ','Roughness in PET')

# Reoder levels 
Full$Covariates <- factor(Full$Covariates, levels = custom_order)

# change data frame name
Totalspp_full_Carbeys = Full

# Save data frame 
saveRDS(Totalspp_full_Carbeys, file = "Data/TotalCARBAYES")


Full_spp = ggplot(Full, aes(x = Covariates, y = Mean, ymin = `2.5%`, ymax = `97.5%`))+
 geom_pointrange(size = 2)+
 geom_hline(yintercept = 0, linetype = 2, size = 2)+
 theme(panel.background = element_rect(fill = "white"))+
  scale_y_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, by = 0.1))+
  labs(title = "a)")+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 50),
        axis.title = element_text(size = 50, face = "bold", vjust = 1),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))+
  geom_point(colour = 'black', size = 5)+
  coord_flip()
  

# Save 
ggsave("Output/Bayesian models/Fig.1.png", Full_spp, width = 80, height = 50, units = "cm")


##############################################################################
###############################################################################
########################## Proceed to : 2 narrow range endemics modeling

  

  
