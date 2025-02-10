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
# 3) model according to the best most (AIC) 
# 4) SA
# 4a) create neigbhours 
# 4b) creat a list 
# 4c) create weights 
# 4d) create a matrix
# 5) fit the model using CARbayes 

#load libraries

#library(tidyverse)
#library(readxl)
#library(readr)
#library(sf)
#library(units)
#library(terra)
#library(mapview)
#library(viridis)
#library(tidyr)
#library(ggpubr)
#library(dplyr)
#library(corrplot)
#library(MuMIn)
#library(AICcmodavg)
#library(spdep)
#library(spatialreg)
#library(CARBayes)


#Z standardize CFR data 
#CFR_data_anlys <- readRDS("Data/CFR_data_anlys")


##############################################################################
###############################################################################
##########################The Obligate_species NRE model 
##############################################################################


# full model
#full.obg <- glm(Obligate_species ~ Median_CloudFQ + percent_wetland + Mean_CSI+
               #   Mean_annual_Temperature + mean_annual_Rainfall + 
               #   Mean_PET + Mean_Soil_ph + Mean_Ext_P +
               #   Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
               # , family = "quasipoisson" , data = CFR_data_anlys)

# Model summary
#summary(full.obg)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------Spatial autocorrelation  ------------------------

##############################################################################

#creating queen's neigbours 
#Neighbours <- poly2nb(CFR_data_anlys)

#summary
#Neighbours <- summary(Neighbours)

# Create weights list object
#listed_w <- nb2listw(Neighbours, zero.policy = TRUE)

# Convert to spatial weights matrix
#W <-nb2mat(Neighbours, style = "B", zero.policy = TRUE)

# Check the structure of the weights matrix
#str(W)

# Perform Moran's test
#MoranI_Total <- moran.test(CFR_data_anlys$Obligate_species, listed_w)

#print(MoranI_Total)


# Fit the Leroux CAR model
Obg_mod <- S.CARleroux(formula = CFR_data_anlys$Obligate_species ~ 
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
print(Obg_mod)


plot(Obg_mod$samples$beta)

# Species richness data cleaning 

#extract values from model 

Full.OBG <- Obg_mod$summary.results %>% as.data.frame()
Full.OBG = Full.OBG[-1,]
Full.OBG = Full.OBG[-12,]
Full.OBG = Full.OBG[-12,]
Full.OBG$Covariates = c('Median CloudFQ', 'Wetland%', 'Mean CSI',
                        'MAT', 'MAP', 'PET', 'pH', 'P',
                        'Range in MAT', 'Roughness in MDQ','Roughness in PET')

#Totalspp = rbind(climate,productivity,Heterogeneity,Full)

Full.OBG$Model_Type = c('Full','Full','Full','Full','Full','Full','Full','Full','Full','Full','Full') 

# Set custom order for Model_Type
custom_order <- c('Median CloudFQ', 'Wetland%', 'Mean CSI',
                  'MAT', 'MAP', 'PET', 'pH', 'P',
                  'Range in MAT', 'Roughness in MDQ','Roughness in PET')
Full.OBG$Covariates <- factor(Full.OBG$Covariates, levels = custom_order)


Obligate_spp <- ggplot(Full.OBG, aes(x = Covariates, y = Mean))+
  geom_pointrange(aes(ymin = `2.5%`, ymax = `97.5%`),data = Full.OBG, size = 2)+
  geom_hline(yintercept = 0, linetype = 2, size = 2)+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title = "e)")+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 35),
        axis.title = element_text(size = 50, face = "bold", vjust = 1),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5))+
  geom_point(colour = 'black', size = 5)



ggsave("Output/Bayesian models/Fig.5.png", Obligate_spp, width = 80, height = 50, units = "cm")

  


#saveRDS(Obligate_plot, file = "Data/Obligate_model_output")

#Merge all 4 plots into one 

Plot1.Manscript <- ggpubr::ggarrange(Full_spp, 
                                     NRE_SPP,
                                     Wet_spp,
                                     FCT_spp,
                                     Obligate_spp,
                                     labels = "" , 
                                     label.x = 0.4, label.y = 0.9,  
                                     hjust = 0, nrow = 1, ncol = 5)


##############################################################################
###############################################################################
########################## END


