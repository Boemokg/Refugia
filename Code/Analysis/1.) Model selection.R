#Computing “quasi-AIC” (QAIC)
#For this i will require e QAIC: AICcmodavg and MuMIn
#Use the dredge function and model.av function to get the best model 
#fit the model twice, once with a regular likelihood model (family=binomial,
#poisson, etc.) and once with the quasi- variant — extract the loglikelihood 
#from the former and the dispersion parameter from the latter

#1) fit a normal poisson model on the full model 
#2) the fit the quasi-poisson model 
#3) create a function to get the loglikehood from the general possion model 
#4) The use the dredge mode

library(MuMIn)
library(AICcmodavg)
library(spdep)
library(spatialreg)
library(MASS)
library(sf)
library(tidyverse)
library(writexl)
library(spdep)
library(spatialreg)
library(CARBayes)



# Load thesis data 
# Z standardize CFR data 
CFR_data_anlys <- readRDS("Data/CFR_data_anlys")



#creating queen's neigbours 
Neighbours <- poly2nb(CFR_data_anlys)

# Create weights list object
listed_w <- nb2listw(Neighbours, zero.policy = TRUE)

# Convert to spatial weights matrix
W <-nb2mat(Neighbours, style = "B", zero.policy = TRUE)

# Check the structure of the weights matrix
str(W)


##############################################################################
###############################################################################
########################## TOTAL SPECIES MODEL SELECTION  
##############################################################################

#################### SARMA GLM 

Clim.tot <- glm(Total_species_richness ~  Median_CloudFQ + percent_wetland + Mean_CSI
                , family = "quasipoisson" , data = CFR_data_anlys)

# CARBAYES CLIM data
Clim.tot <- S.CARleroux(formula = CFR_data_anlys$Total_species_richness ~ 
                    CFR_data_anlys$Median_CloudFQ + 
                    CFR_data_anlys$percent_wetland + 
                    CFR_data_anlys$Mean_CSI,
                      family = "poisson",
                    W = W,
                    burnin = 500000,
                    n.sample = 1000000,
                    thin = 500,
                    n.cores = 3,
                    n.chains = 3)

#print model 
print(Clim.tot) #DIC =  1900.247       p.d =  189.5339       LMPL =  -1011.46 

#water and energy hypothesis submodel
Prod.tot <- glm(Total_species_richness~ Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P, family = "quasipoisson" , data = CFR_data_anlys)

# Fit the Leroux CAR model
Prod.tot <- S.CARleroux(formula = CFR_data_anlys$Total_species_richness ~ 
                    CFR_data_anlys$Mean_annual_Temperature + 
                    CFR_data_anlys$mean_annual_Rainfall + 
                    CFR_data_anlys$Mean_PET + 
                    CFR_data_anlys$Mean_Soil_ph + 
                    CFR_data_anlys$Mean_Ext_P,
                  family = "poisson",
                  W = W,
                  burnin = 500000,
                  n.sample = 1000000,
                  thin = 500,
                  n.cores = 3,
                  n.chains = 3)

# print model 
print(Prod.tot) #DIC =  1902.689       p.d =  189.865       LMPL =  -1018.56 


#Environmental heterogeneity Model
Het.tot <- glm(Total_species_richness ~
                 Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
               , family = "quasipoisson" , data = CFR_data_anlys)

# Fit the Leroux CAR model
Het.tot <- S.CARleroux(formula = CFR_data_anlys$Total_species_richness ~ 
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

# print model 
print(Het.tot) #DIC =  1901.027       p.d =  189.4458       LMPL =  -1008.36 

#Best model

best.tot <- glm(Total_species_richness ~ percent_wetland + Mean_CSI+
                             Mean_annual_Temperature + mean_annual_Rainfall + 
                             Mean_Ext_P + Range_Temperature + Roughness_Dry_Rainfall
                           , family = "quasipoisson" , data = CFR_data_anlys)

# Fit the Leroux CAR model
best.tot <- S.CARleroux(formula = CFR_data_anlys$Total_species_richness ~ 
                    CFR_data_anlys$percent_wetland + 
                    CFR_data_anlys$Mean_CSI+
                    CFR_data_anlys$Mean_annual_Temperature + 
                    CFR_data_anlys$mean_annual_Rainfall + 
                    CFR_data_anlys$Mean_Ext_P +
                    CFR_data_anlys$Range_Temperature + 
                    CFR_data_anlys$Roughness_Dry_Rainfall,
                  family = "poisson",
                  W = W,
                  burnin = 500000,
                  n.sample = 1000000,
                  thin = 500,
                  n.cores = 3,
                  n.chains = 3)

# print model 
print(best.tot)


#full model
full.tot <- glm(Total_species_richness ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                  Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                  Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                , family = "quasipoisson" , data = CFR_data_anlys)

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
print(SR) #DIC =  1901.969       p.d =  189.2283       LMPL =  -1025.85 
#loglikelihood -761.7561 



##############################################################################
###############################################################################
########################## NRE DIC 
##############################################################################

#################### SARMA GLM 


Clim.nre <- glm(Narrow_Range_endemics ~  Median_CloudFQ + percent_wetland + Mean_CSI
                , family = "quasipoisson" , data = CFR_data_anlys)

# CARBAYES CLIM data
Clim.nre <- S.CARleroux(formula = CFR_data_anlys$Narrow_Range_endemics ~ 
                          CFR_data_anlys$Median_CloudFQ + 
                          CFR_data_anlys$percent_wetland + 
                          CFR_data_anlys$Mean_CSI,
                        family = "poisson",
                        W = W,
                        burnin = 500000,
                        n.sample = 1000000,
                        thin = 500,
                        n.cores = 3,
                        n.chains = 3)

print(Clim.nre) #DIC =  1390.531       p.d =  187.3698       LMPL =  -765.38 


#water and energy hypothesis submodel
Prod.nre <- glm(Narrow_Range_endemics~ Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P, family = "quasipoisson" , data = CFR_data_anlys)

# Fit the Leroux CAR model
Prod.nre <- S.CARleroux(formula = CFR_data_anlys$Narrow_Range_endemics ~ 
                          CFR_data_anlys$Mean_annual_Temperature + 
                          CFR_data_anlys$mean_annual_Rainfall + 
                          CFR_data_anlys$Mean_PET + 
                          CFR_data_anlys$Mean_Soil_ph + 
                          CFR_data_anlys$Mean_Ext_P,
                        family = "poisson",
                        W = W,
                        burnin = 500000,
                        n.sample = 1000000,
                        thin = 500,
                        n.cores = 3,
                        n.chains = 3)

print(Prod.nre) #DIC =  1390.531       p.d =  187.3698       LMPL =  -765.38 


#Environmental heterogeneity Model
Het.nre <- glm(Narrow_Range_endemics ~
                 Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
               , family = "quasipoisson" , data = CFR_data_anlys)

# Fit the Leroux CAR model
Het.nre <- S.CARleroux(formula = CFR_data_anlys$Narrow_Range_endemics ~ 
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
print(Het.nre) #DIC =  1387.976       p.d =  185.1115       LMPL =  -762.94 

# Best model 
Best.nre <- glm(Narrow_Range_endemics ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                        Mean_Soil_ph + Range_Temperature + Roughness_Dry_Rainfall
                      ,family = "quasipoisson" , data = CFR_data_anlys)

# Fit the Leroux car model 

Best.nre <- S.CARleroux(formula = CFR_data_anlys$Narrow_Range_endemics ~ 
              CFR_data_anlys$Median_CloudFQ + 
              CFR_data_anlys$percent_wetland + 
              CFR_data_anlys$Mean_CSI+
              CFR_data_anlys$Mean_Soil_ph + 
              CFR_data_anlys$Range_Temperature + 
              CFR_data_anlys$Roughness_Dry_Rainfall,
              family = "poisson",
            W = W,
            burnin = 500000,
            n.sample = 1000000,
            thin = 500,
            n.cores = 3,
            n.chains = 3)

# print output
print(Best.nre) #DIC =  1384.904       p.d =  182.7361       LMPL =  -762.3 

#full model
full.nre <- glm(Narrow_Range_endemics ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                  Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                  Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                , family = "quasipoisson" , data = CFR_data_anlys)
# Fit the Leroux CAR model
SR_nre <- S.CARleroux(formula = CFR_data_anlys$Narrow_Range_endemics ~ 
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

# print the outputs 
print(SR_nre) #


# delta DIC and weights 

DIC_Total <- DIC(hacked_clim.wet,hacked_prod.wet,hacked_het.wet,hacked_full.wet,hacked_best.wet)                                 # AIC
qdelta.aics <- qaics$AIC - min(qaics$AIC)  # Delta AIC
qwi <- exp(-0.5*qdelta.aics)/sum(exp(-0.5*qdelta.aics))# Akaike weights

qlogliks <- c(logLik(hacked_clim.wet),logLik(hacked_prod.wet),logLik(hacked_het.wet), logLik(hacked_full.wet), logLik(hacked_best.wet))        # log Likelihoods
models <- c("Climatic Stability", "Productivity","Heterogeneity", "Full", "Best")                          # model names
WTD = data.frame(models, -2*qlogliks, numpar=qaics$df, AIC=qaics$AIC, qdelta.aics,qwi)
write_xlsx(WTD, "Data/modtable_WET_SARMA.xlsx")

##############################################################################
###############################################################################
########################## Wetland spp DIC 
##############################################################################

#wetland data 
CFR_data_anlys$wetland_nre <- CFR_data_anlys$Facultative_species+
  CFR_data_anlys$Obligate_species

# CARBAYES CLIM data
Clim.wet <- S.CARleroux(formula = CFR_data_anlys$wetland_nre ~ 
                          CFR_data_anlys$Median_CloudFQ + 
                          CFR_data_anlys$percent_wetland + 
                          CFR_data_anlys$Mean_CSI,
                        family = "poisson",
                        W = W,
                        burnin = 500000,
                        n.sample = 1000000,
                        thin = 500,
                        n.cores = 3,
                        n.chains = 3)
print(Clim.wet)

# Fit the Leroux CAR model
Prod.wet <- S.CARleroux(formula = CFR_data_anlys$wetland_nre ~ 
                          CFR_data_anlys$Mean_annual_Temperature + 
                          CFR_data_anlys$mean_annual_Rainfall + 
                          CFR_data_anlys$Mean_PET + 
                          CFR_data_anlys$Mean_Soil_ph + 
                          CFR_data_anlys$Mean_Ext_P,
                        family = "poisson",
                        W = W,
                        burnin = 500000,
                        n.sample = 1000000,
                        thin = 500,
                        n.cores = 3,
                        n.chains = 3)

print(Prod.wet) 


# Fit the Leroux CAR model
Het.wet <- S.CARleroux(formula = CFR_data_anlys$wetland_nre ~ 
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

print(Het.wet) 

# Best model 

best.wet <- S.CARleroux(formula = CFR_data_anlys$wetland_nre ~ 
                    CFR_data_anlys$Median_CloudFQ + 
                    CFR_data_anlys$percent_wetland + 
                    CFR_data_anlys$Mean_CSI+
                    CFR_data_anlys$Mean_Soil_ph + 
                    CFR_data_anlys$Range_Temperature + 
                    CFR_data_anlys$Roughness_Dry_Rainfall, 
                  family = "poisson",
                  W = W,
                  burnin = 500000,
                  n.sample = 1000000,
                  thin = 500,
                  n.cores = 3,
                  n.chains = 3)

print(best.wet)

SR_wet <- S.CARleroux(formula = CFR_data_anlys$wetland_nre ~ 
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

print(SR_wet)

##############################################################################
###############################################################################
########################## facultative dIC 
##############################################################################

#################### SARMA GLM 

# facultative
#Clim.fct <- glm(Facultative_species ~  Median_CloudFQ + percent_wetland + Mean_CSI
#                , family = "quasipoisson" , data = CFR_data_anlys)

#water and energy hypothesis submodel
#Prod.fct <- glm(Facultative_species~ Mean_annual_Temperature + mean_annual_Rainfall + 
 #                 Mean_PET + Mean_Soil_ph + Mean_Ext_P, family = "quasipoisson" , data = CFR_data_anlys)

#Environmental heterogeneity Model
#Het.fct <- glm(Facultative_species ~
#                 Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
#               , family = "quasipoisson" , data = CFR_data_anlys)

#full model
#full.fct <- glm(Facultative_species ~ Median_CloudFQ + percent_wetland + Mean_CSI+
 #                 Mean_annual_Temperature + mean_annual_Rainfall + 
  #                Mean_PET + Mean_Soil_ph + Mean_Ext_P +
   #               Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
    #            , family = "quasipoisson" , data = CFR_data_anlys)

# now fit a Spatial simultaneous 
# autoregressive model estimation by maximum likelihood :
# where : ρ and λ are found by nlminb or optim() first, and 
# β and other parameters by generalized least squares subsequently.

# Climatic stability 
#fct_clim.sp <- sacsarlm(Clim.fct, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# Productivity  
#fct_prod.sp <- sacsarlm(Prod.fct, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# Heterogeneity 
#fct_het.sp <- sacsarlm(Het.fct, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# Full model 
#fct_full.sp <- sacsarlm(full.fct, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# *************************************************************************
# the following piece of code extracts log Likelihoods and AIC, and calculates
# the other things needed for a AIC based model selection.
# In the end, it produces a model selection table
# ************************************************************************

#aics_fct <- AIC(fct_clim.sp,fct_prod.sp,fct_het.sp,fct_full.sp)                                 # AIC
#delta.aics_fct <- aics_fct$AIC - min(aics_fct$AIC)  # Delta AIC
#wi_fct <- exp(-0.5*delta.aics_fct)/sum(exp(-0.5*delta.aics_fct))# Akaike weights

#logliks_fct <- c(logLik(fct_clim.sp),logLik(fct_prod.sp),logLik(fct_het.sp), logLik(fct_full.sp))        # log Likelihoods
#models <- c("Climatic Stability", "Productivity","Heterogeneity", "Full")                          # model names

#(modtable_fct <- data.frame(models, -2*logliks_fct, numpar=aics_fct$df, AIC=aics_fct$AIC, delta.aics_fct,wi_fct))
#write_xlsx(modtable_fct, "Data/modtable_nre.xlsx")

# CARBAYES CLIM data
Clim.fct <- S.CARleroux(formula = CFR_data_anlys$Facultative_species ~ 
                          CFR_data_anlys$Median_CloudFQ + 
                          CFR_data_anlys$percent_wetland + 
                          CFR_data_anlys$Mean_CSI,
                        family = "poisson",
                        W = W,
                        burnin = 500000,
                        n.sample = 1000000,
                        thin = 500,
                        n.cores = 3,
                        n.chains = 3)
print(Clim.fct)

# Fit the Leroux CAR model
Prod.fct <- S.CARleroux(formula = CFR_data_anlys$Facultative_species ~ 
                          CFR_data_anlys$Mean_annual_Temperature + 
                          CFR_data_anlys$mean_annual_Rainfall + 
                          CFR_data_anlys$Mean_PET + 
                          CFR_data_anlys$Mean_Soil_ph + 
                          CFR_data_anlys$Mean_Ext_P,
                        family = "poisson",
                        W = W,
                        burnin = 50000000,
                        n.sample = 100000000,
                        thin = 50000,
                        n.cores = 3,
                        n.chains = 3)

print(Prod.fct) 


# Fit the Leroux CAR model
Het.fct <- S.CARleroux(formula = CFR_data_anlys$Facultative_species ~ 
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

print(Het.fct) 

# Best model 

best.wet <- S.CARleroux(formula = CFR_data_anlys$wetland_nre ~ 
                          CFR_data_anlys$Median_CloudFQ + 
                          CFR_data_anlys$percent_wetland + 
                          CFR_data_anlys$Mean_CSI+
                          CFR_data_anlys$Mean_Soil_ph + 
                          CFR_data_anlys$Range_Temperature + 
                          CFR_data_anlys$Roughness_Dry_Rainfall, 
                        family = "poisson",
                        W = W,
                        burnin = 500000,
                        n.sample = 1000000,
                        thin = 500,
                        n.cores = 3,
                        n.chains = 3)

print(best.wet)

SR_fct <- S.CARleroux(formula = CFR_data_anlys$Facultative_species ~ 
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

print(SR_fct)


##############################################################################
###############################################################################
########################## Obligate AIC 
##############################################################################

#################### SARMA GLM 

#Climatic stability submodel
#Clim.ob <- glm(Obligate_species ~  Median_CloudFQ + percent_wetland + Mean_CSI
 #              , family = "quasipoisson" , data = CFR_data_anlys)

#water and energy hypothesis submodel
#Prod.ob <- glm(Obligate_species~ Mean_annual_Temperature + mean_annual_Rainfall + 
 #                Mean_PET + Mean_Soil_ph + Mean_Ext_P, family = "quasipoisson" , data = CFR_data_anlys)
#Environmental heterogeneity Model
#Het.ob <- glm(Obligate_species ~
 #               Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
  #            , family = "quasipoisson" , data = CFR_data_anlys)

#full model
#full.ob <- glm(Obligate_species ~ Median_CloudFQ + percent_wetland + Mean_CSI+
 #                Mean_annual_Temperature + mean_annual_Rainfall + 
  #               Mean_PET + Mean_Soil_ph + Mean_Ext_P +
   #              Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
    #           , family = "quasipoisson" , data = CFR_data_anlys)

# now fit a Spatial simultaneous 
# autoregressive model estimation by maximum likelihood :
# where : ρ and λ are found by nlminb or optim() first, and 
# β and other parameters by generalized least squares subsequently.

# Climatic stability 
#clim.ob_sp <- sacsarlm(Clim.ob, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# Productivity  
#prod.ob_sp <- sacsarlm(Prod.ob, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# Heterogeneity 
#het.ob_sp<- sacsarlm(Het.ob, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# Full model 
#full.ob_sp <- sacsarlm(full.ob, data= CFR_data_anlys, listed_w, zero.policy=TRUE)
# *************************************************************************
# the following piece of code extracts log Likelihoods and AIC, and calculates
# the other things needed for a AIC based model selection.
# In the end, it produces a model selection table
# ************************************************************************

#aics_ob <- AIC(clim.ob_sp,prod.ob_sp,het.ob_sp,full.ob_sp)                                 # AIC
#delta.aics_ob <- aics_ob$AIC - min(aics_ob$AIC)  # Delta AIC
#wi_ob <- exp(-0.5*delta.aics_ob)/sum(exp(-0.5*delta.aics_ob))# Akaike weights

#logliks_ob <- c(logLik(clim.ob_sp),logLik(prod.ob_sp),logLik(het.ob_sp), logLik(full.ob_sp))        # log Likelihoods
#models <- c("Climatic Stability", "Productivity","Heterogeneity", "Full")                          # model names

#(modtable_ob <- data.frame(models, -2*logliks_ob, numpar=aics_ob$df, AIC=aics_ob$AIC, delta.aics_ob,wi_ob))
#write_xlsx(modtable_ob, "Data/modtable_nre.xlsx")


# *************************************************************************
# the following piece of code extracts log Likelihoods and AIC, and calculates
# the other things needed for a AIC based model selection.
# In the end, it produces a model selection table
# ************************************************************************
# *************************************************************************
# the following piece of code extracts log Likelihoods and AIC, and calculates
# the other things needed for a AIC based model selection.
# In the end, it produces a model selection table
# ************************************************************************


##############################################################################
###############################################################################
######################## NRE qAIC 
##############################################################################

#################### Quasi GLM 




  # A 'hacked' constructor for quasibinomial family object that allows for
  # ML estimation
  hacked.quasipoisson <- function(...) {
    res <- quasipoisson(...)
    res$aic <- poisson(...)$aic
    res
  }
  
  
hacked_clim.nre <- update(Clim.nre, family = hacked.quasipoisson)
hacked_het.nre <- update(Het.nre, family = hacked.quasipoisson)
hacked_prod.nre <- update(Prod.nre, family = hacked.quasipoisson)
hacked_best.nre <- update(Best.nre, family = hacked.quasipoisson)
hacked_full.nre <- update(full.nre, family = hacked.quasipoisson)

AIC(hacked_full.nre, k = length(coef(full.nre))) - 2 *logLik(hacked_full.nre)
AIC(hacked_het.nre, k = length(coef(Het.nre))) - 2 * logLik(hacked_het.nre)
AIC(hacked_prod.nre, k = length(coef(Prod.nre))) - 2 * logLik(hacked_prod.nre)
AIC(hacked_best.nre, k = length(coef(Best.nre))) - 2 * logLik(hacked_best.nre)
AIC(hacked_clim.nre, k = length(coef(Clim.nre))) - 2 * logLik(hacked_clim.nre)



qaics <- AIC(hacked_clim.nre,hacked_het.nre,hacked_prod.nre,hacked_full.nre, hacked_best.nre)                                 # AIC
qdelta.aics <- qaics$AIC - min(qaics$AIC)  # Delta AIC
qwi <- exp(-0.5*qdelta.aics)/sum(exp(-0.5*qdelta.aics))# Akaike weights

qlogliks <- c(logLik(hacked_clim.nre),logLik(hacked_prod.nre),logLik(hacked_het.nre), logLik(hacked_best.nre), logLik(hacked_full.nre))        # log Likelihoods
models <- c("Climatic Stability", "Productivity","Heterogeneity", "Full", "Best")                          # model names
NRE <-data.frame(models, -2*qlogliks, numpar=qaics$df, AIC=qaics$AIC, qdelta.aics,qwi)
write_xlsx(NRE, "Data/modtable_NRE_SARMA.xlsx")


##############################################################################
###############################################################################
######################## total qAIC 
##############################################################################

#################### Quasi GLM 


hacked_clim.tot <- update(Clim.tot, family = hacked.quasipoisson)
hacked_het.tot <- update(Het.tot, family = hacked.quasipoisson)
hacked_prod.tot <- update(Prod.tot, family = hacked.quasipoisson)
hacked_best.tot <- update(best.tot, family = hacked.quasipoisson)
hacked_full.tot <- update(full.tot, family = hacked.quasipoisson)

AIC(hacked_clim.tot, k = length(coef(Clim.tot))) - 2 *logLik(hacked_clim.tot)
AIC(hacked_het.tot, k = length(coef(Het.tot))) - 2 * logLik(hacked_het.tot)
AIC(hacked_prod.tot, k = length(coef(Prod.tot))) - 2 * logLik(hacked_prod.tot)
AIC(hacked_best.tot, k = length(coef(best.tot))) - 2 * logLik(hacked_best.tot)
AIC(hacked_full.tot, k = length(coef(full.tot))) - 2 * logLik(hacked_full.tot)



qaics_tot <- AIC(hacked_clim.tot,hacked_het.tot,hacked_prod.tot,hacked_full.tot, hacked_best.tot)                                 # AIC
qdelta.aics_tot <- qaics_tot$AIC - min(qaics_tot$AIC)  # Delta AIC
qwi_tot <- exp(-0.5*qdelta.aics_tot)/sum(exp(-0.5*qdelta.aics_tot))# Akaike weights

qlogliks_tot <- c(logLik(hacked_clim.tot),logLik(hacked_het.tot),logLik(hacked_prod.tot), logLik(hacked_full.tot), logLik(hacked_best.tot))        # log Likelihoods
models <- c("Climatic Stability", "Productivity","Heterogeneity", "Full", "best")                          # model names
TOT <- data.frame(models, -2*qlogliks_tot, numpar=qaics_tot$df, AIC=qaics_tot$AIC, qdelta.aics_tot,qwi_tot)
write_xlsx(TOT, "Data/modtable_TOT_SARMA.xlsx")

##############################################################################
###############################################################################
######################## wetland qAIC 
##############################################################################

#################### SARMA GLM 


hacked_clim.wet <- update(Clim.wet, family = hacked.quasipoisson)
hacked_het.wet <- update(Het.wet, family = hacked.quasipoisson)
hacked_prod.wet <- update(Prod.wet, family = hacked.quasipoisson)
hacked_best.wet <- update(best.wet, family = hacked.quasipoisson)
hacked_full.wet <- update(full.wet, family = hacked.quasipoisson)

AIC(hacked_clim.wet, k = length(coef(Clim.wet))) - 2 *logLik(hacked_clim.wet)
AIC(hacked_het.wet, k = length(coef(Het.wet))) - 2 * logLik(hacked_het.wet)
AIC(hacked_prod.wet, k = length(coef(Prod.nre))) - 2 * logLik(hacked_prod.wet)
AIC(hacked_best.wet, k = length(coef(best.wet))) - 2 * logLik(hacked_best.wet)
AIC(hacked_full.wet, k = length(coef(full.wet))) - 2 * logLik(hacked_full.wet)



qaics <- AIC(hacked_clim.wet,hacked_prod.wet,hacked_het.wet,hacked_full.wet,hacked_best.wet)                                 # AIC
qdelta.aics <- qaics$AIC - min(qaics$AIC)  # Delta AIC
qwi <- exp(-0.5*qdelta.aics)/sum(exp(-0.5*qdelta.aics))# Akaike weights

qlogliks <- c(logLik(hacked_clim.wet),logLik(hacked_prod.wet),logLik(hacked_het.wet), logLik(hacked_full.wet), logLik(hacked_best.wet))        # log Likelihoods
models <- c("Climatic Stability", "Productivity","Heterogeneity", "Full", "Best")                          # model names
WTD = data.frame(models, -2*qlogliks, numpar=qaics$df, AIC=qaics$AIC, qdelta.aics,qwi)
write_xlsx(WTD, "Data/modtable_WET_SARMA.xlsx")


##############################################################################
###############################################################################
######################## Full  qAIC 
##############################################################################

#################### Quasi GLM 


#hacked_clim.nre <- update(Clim.nre, family = hacked.quasipoisson)
#hacked_het.nre <- update(Het.nre, family = hacked.quasipoisson)
#hacked_prod.nre <- update(Prod.nre, family = hacked.quasipoisson)
#hacked_full.nre <- update(full.nre, family = hacked.quasipoisson)

#AIC(hacked_full.nre, k = length(coef(full.nre))) - 2 *logLik(hacked_full.nre)
#AIC(hacked_het.nre, k = length(coef(Het.nre))) - 2 * logLik(hacked_het.nre)
#AIC(hacked_prod.nre, k = length(coef(Prod.nre))) - 2 * logLik(hacked_prod.nre)
#AIC(hacked_clim.nre, k = length(coef(Clim.nre))) - 2 * logLik(hacked_clim.nre)



#qaics <- AIC(hacked_clim.nre,hacked_het.nre,hacked_prod.nre,hacked_full.nre)                                 # AIC
#qdelta.aics <- qaics$AIC - min(qaics$AIC)  # Delta AIC
#qwi <- exp(-0.5*qdelta.aics)/sum(exp(-0.5*qdelta.aics))# Akaike weights

#qlogliks <- c(logLik(hacked_clim.nre),logLik(hacked_prod.nre),logLik(hacked_het.nre), logLik(hacked_full.nre))        # log Likelihoods
#models <- c("Climatic Stability", "Productivity","Heterogeneity", "Full")                          # model names
#data.frame(models, -2*qlogliks, numpar=qaics$df, AIC=qaics$AIC, qdelta.aics,qwi)

    
