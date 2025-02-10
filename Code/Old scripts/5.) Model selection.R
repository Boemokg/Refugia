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



#Load thesis data 
# Cleaning the data
CFR_data <- readRDS("Data/CFR_data")


#Z standardize the covariates  
CFR_data_anlys <- CFR_data %>%
  mutate(across(6:30, scale))

##############################################################################
###############################################################################
########################## TOTAL AIC 
##############################################################################

#################### SARMA GLM 

Clim.tot <- glm(Total_species_richness ~  Median_CloudFQ + percent_wetland + Mean_CSI
                , family = "quasipoisson" , data = CFR_data_anlys)


#water and energy hypothesis submodel
Prod.tot <- glm(Total_species_richness~ Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P, family = "quasipoisson" , data = CFR_data_anlys)

#Environmental heterogeneity Model
Het.tot <- glm(Total_species_richness ~
                 Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
               , family = "quasipoisson" , data = CFR_data_anlys)

#full model
full.tot <- glm(Total_species_richness ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                  Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                  Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                , family = "quasipoisson" , data = CFR_data_anlys)


#creating queen's neigbours 
Neighbours <- poly2nb(CFR_data_anlys)

#matrics of weights
listed_w <- spdep :: nb2listw(Neighbours, zero.policy = TRUE)

# Climatic stability 
tot_clim.sp <- sacsarlm(Clim.tot, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# Productivity  
tot_prod.sp <- sacsarlm(Prod.tot, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# Heterogeneity 
tot_het.sp <- sacsarlm(Het.tot, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# Full model 
tot_full.sp <- sacsarlm(full.tot, data= CFR_data_anlys, listed_w, zero.policy=TRUE)


  # *************************************************************************
  # the following piece of code extracts log Likelihoods and AIC, and calculates
  # the other things needed for a AIC based model selection.
  # In the end, it produces a model selection table
  # ************************************************************************
  
aics_total <- AIC(tot_clim.sp,tot_prod.sp,tot_het.sp,tot_full.sp)                                 # AIC
delta.aics_t <- aics_total$AIC - min(aics_total$AIC)  # Delta AIC
wi_t <- exp(-0.5*delta.aics_t)/sum(exp(-0.5*delta.aics_t))# Akaike weights

logliks <- c(logLik(tot_clim.sp),logLik(tot_prod.sp),logLik(tot_het.sp), logLik(tot_full.sp))        # log Likelihoods
models <- c("Climatic Stability", "Productivity","Heterogeneity", "Full")                          # model names

(modtable_total <- data.frame(models, -2*logliks, numpar=aics_total$df, AIC=aics_total$AIC, delta.aics_t,wi_t))
write_xlsx(modtable_total, "Data/modtable_total.xlsx")


##############################################################################
###############################################################################
########################## NRE AIC 
##############################################################################

#################### SARMA GLM 


Clim.nre <- glm(Narrow_Range_endemics ~  Median_CloudFQ + percent_wetland + Mean_CSI
                , family = "quasipoisson" , data = CFR_data_anlys)

#water and energy hypothesis submodel
Prod.nre <- glm(Narrow_Range_endemics~ Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P, family = "quasipoisson" , data = CFR_data_anlys)


#Environmental heterogeneity Model
Het.nre <- glm(Narrow_Range_endemics ~
                 Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
               , family = "quasipoisson" , data = CFR_data_anlys)

#full model
full.nre <- glm(Narrow_Range_endemics ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                  Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                  Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                , family = "quasipoisson" , data = CFR_data_anlys)



# Climatic stability spatial model 
narrow_clim.sp <- sacsarlm(Clim.nre, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# Productivityspatial model
narrow_prod.sp <- sacsarlm(Prod.nre, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# Heterogeneity spatial model
narrow_het.sp <- sacsarlm(Het.nre, data= CFR_data_anlys, listed_w, zero.policy=TRUE)
# Full model spatial model
narrow_full.sp <- sacsarlm(full.nre, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# *************************************************************************
# the following piece of code extracts log Likelihoods and AIC, and calculates
# the other things needed for a AIC based model selection.
# In the end, it produces a model selection table
# ************************************************************************

aics_nre <- AIC(narrow_clim.sp,narrow_prod.sp,narrow_het.sp,narrow_full.sp)                                 # AIC
delta.aics_nre <- aics_nre$AIC - min(aics_nre$AIC)  # Delta AIC
wi_nre <- exp(-0.5*delta.aics_nre)/sum(exp(-0.5*delta.aics_nre))# Akaike weights

logliks <- c(logLik(narrow_clim.sp),logLik(narrow_prod.sp),logLik(narrow_het.sp), logLik(narrow_full.sp))        # log Likelihoods
models <- c("Climatic Stability", "Productivity","Heterogeneity", "Full")                          # model names

(modtable_nre <- data.frame(models, -2*logliks, numpar=aics_nre$df, AIC=aics_nre$AIC, delta.aics_nre,wi_nre))
write_xlsx(modtable_nre, "Data/modtable_nre.xlsx")


##############################################################################
###############################################################################
########################## facultative AIC 
##############################################################################

#################### SARMA GLM 

# facultative
Clim.fct <- glm(Facultative_species ~  Median_CloudFQ + percent_wetland + Mean_CSI
                , family = "quasipoisson" , data = CFR_data_anlys)

#water and energy hypothesis submodel
Prod.fct <- glm(Facultative_species~ Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P, family = "quasipoisson" , data = CFR_data_anlys)

#Environmental heterogeneity Model
Het.fct <- glm(Facultative_species ~
                 Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
               , family = "quasipoisson" , data = CFR_data_anlys)

#full model
full.fct <- glm(Facultative_species ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                  Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                  Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                , family = "quasipoisson" , data = CFR_data_anlys)

# now fit a Spatial simultaneous 
# autoregressive model estimation by maximum likelihood :
# where : ρ and λ are found by nlminb or optim() first, and 
# β and other parameters by generalized least squares subsequently.

# Climatic stability 
fct_clim.sp <- sacsarlm(Clim.fct, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# Productivity  
fct_prod.sp <- sacsarlm(Prod.fct, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# Heterogeneity 
fct_het.sp <- sacsarlm(Het.fct, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# Full model 
fct_full.sp <- sacsarlm(full.fct, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# *************************************************************************
# the following piece of code extracts log Likelihoods and AIC, and calculates
# the other things needed for a AIC based model selection.
# In the end, it produces a model selection table
# ************************************************************************

aics_fct <- AIC(fct_clim.sp,fct_prod.sp,fct_het.sp,fct_full.sp)                                 # AIC
delta.aics_fct <- aics_fct$AIC - min(aics_fct$AIC)  # Delta AIC
wi_fct <- exp(-0.5*delta.aics_fct)/sum(exp(-0.5*delta.aics_fct))# Akaike weights

logliks_fct <- c(logLik(fct_clim.sp),logLik(fct_prod.sp),logLik(fct_het.sp), logLik(fct_full.sp))        # log Likelihoods
models <- c("Climatic Stability", "Productivity","Heterogeneity", "Full")                          # model names

(modtable_fct <- data.frame(models, -2*logliks_fct, numpar=aics_fct$df, AIC=aics_fct$AIC, delta.aics_fct,wi_fct))
write_xlsx(modtable_fct, "Data/modtable_nre.xlsx")


##############################################################################
###############################################################################
########################## Obligate AIC 
##############################################################################

#################### SARMA GLM 

#Climatic stability submodel
Clim.ob <- glm(Obligate_species ~  Median_CloudFQ + percent_wetland + Mean_CSI
               , family = "quasipoisson" , data = CFR_data_anlys)

#water and energy hypothesis submodel
Prod.ob <- glm(Obligate_species~ Mean_annual_Temperature + mean_annual_Rainfall + 
                 Mean_PET + Mean_Soil_ph + Mean_Ext_P, family = "quasipoisson" , data = CFR_data_anlys)
#Environmental heterogeneity Model
Het.ob <- glm(Obligate_species ~
                Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
              , family = "quasipoisson" , data = CFR_data_anlys)

#full model
full.ob <- glm(Obligate_species ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                 Mean_annual_Temperature + mean_annual_Rainfall + 
                 Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                 Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
               , family = "quasipoisson" , data = CFR_data_anlys)

# now fit a Spatial simultaneous 
# autoregressive model estimation by maximum likelihood :
# where : ρ and λ are found by nlminb or optim() first, and 
# β and other parameters by generalized least squares subsequently.

# Climatic stability 
clim.ob_sp <- sacsarlm(Clim.ob, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# Productivity  
prod.ob_sp <- sacsarlm(Prod.ob, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# Heterogeneity 
het.ob_sp<- sacsarlm(Het.ob, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

# Full model 
full.ob_sp <- sacsarlm(full.ob, data= CFR_data_anlys, listed_w, zero.policy=TRUE)
# *************************************************************************
# the following piece of code extracts log Likelihoods and AIC, and calculates
# the other things needed for a AIC based model selection.
# In the end, it produces a model selection table
# ************************************************************************

aics_ob <- AIC(clim.ob_sp,prod.ob_sp,het.ob_sp,full.ob_sp)                                 # AIC
delta.aics_ob <- aics_ob$AIC - min(aics_ob$AIC)  # Delta AIC
wi_ob <- exp(-0.5*delta.aics_ob)/sum(exp(-0.5*delta.aics_ob))# Akaike weights

logliks_ob <- c(logLik(clim.ob_sp),logLik(prod.ob_sp),logLik(het.ob_sp), logLik(full.ob_sp))        # log Likelihoods
models <- c("Climatic Stability", "Productivity","Heterogeneity", "Full")                          # model names

(modtable_ob <- data.frame(models, -2*logliks_ob, numpar=aics_ob$df, AIC=aics_ob$AIC, delta.aics_ob,wi_ob))
write_xlsx(modtable_ob, "Data/modtable_nre.xlsx")


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
######################## Climatic qAIC 
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
hacked_full.nre <- update(full.nre, family = hacked.quasipoisson)

AIC(hacked_full.nre, k = length(coef(full.nre))) - 2 *logLik(hacked_full.nre)
AIC(hacked_het.nre, k = length(coef(Het.nre))) - 2 * logLik(hacked_het.nre)
AIC(hacked_prod.nre, k = length(coef(Prod.nre))) - 2 * logLik(hacked_prod.nre)
AIC(hacked_clim.nre, k = length(coef(Clim.nre))) - 2 * logLik(hacked_clim.nre)



qaics <- AIC(hacked_clim.nre,hacked_het.nre,hacked_prod.nre,hacked_full.nre)                                 # AIC
qdelta.aics <- qaics$AIC - min(qaics$AIC)  # Delta AIC
qwi <- exp(-0.5*qdelta.aics)/sum(exp(-0.5*qdelta.aics))# Akaike weights

qlogliks <- c(logLik(hacked_clim.nre),logLik(hacked_prod.nre),logLik(hacked_het.nre), logLik(hacked_full.nre))        # log Likelihoods
models <- c("Climatic Stability", "Productivity","Heterogeneity", "Full")                          # model names
data.frame(models, -2*qlogliks, numpar=qaics$df, AIC=qaics$AIC, qdelta.aics,qwi)


##############################################################################
###############################################################################
######################## total qAIC 
##############################################################################

#################### Quasi GLM 


hacked_clim.tot <- update(Clim.tot, family = hacked.quasipoisson)
hacked_het.tot <- update(Het.tot, family = hacked.quasipoisson)
hacked_prod.tot <- update(Prod.tot, family = hacked.quasipoisson)
hacked_full.tot <- update(full.tot, family = hacked.quasipoisson)

AIC(hacked_clim.tot, k = length(coef(Clim.tot))) - 2 *logLik(hacked_clim.tot)
AIC(hacked_het.tot, k = length(coef(Het.tot))) - 2 * logLik(hacked_het.tot)
AIC(hacked_prod.tot, k = length(coef(Prod.tot))) - 2 * logLik(hacked_prod.tot)
AIC(hacked_full.tot, k = length(coef(full.tot))) - 2 * logLik(hacked_full.tot)



qaics_tot <- AIC(hacked_clim.tot,hacked_het.tot,hacked_prod.tot,hacked_full.tot)                                 # AIC
qdelta.aics_tot <- qaics_tot$AIC - min(qaics_tot$AIC)  # Delta AIC
qwi_tot <- exp(-0.5*qdelta.aics_tot)/sum(exp(-0.5*qdelta.aics_tot))# Akaike weights

qlogliks_tot <- c(logLik(hacked_clim.tot),logLik(hacked_het.tot),logLik(hacked_prod.tot), logLik(hacked_full.tot))        # log Likelihoods
models <- c("Climatic Stability", "Productivity","Heterogeneity", "Full")                          # model names
data.frame(models, -2*qlogliks_tot, numpar=qaics_tot$df, AIC=qaics_tot$AIC, qdelta.aics_tot,qwi_tot)


##############################################################################
###############################################################################
######################## heterogeneity qAIC 
##############################################################################

#################### Quasi GLM 


hacked_clim.fct <- update(Clim.fct, family = hacked.quasipoisson)
hacked_het.fct <- update(Het.fct, family = hacked.quasipoisson)
hacked_prod.nre <- update(Prod.nre, family = hacked.quasipoisson)
hacked_full.nre <- update(full.nre, family = hacked.quasipoisson)

AIC(hacked_clim.fct, k = length(coef(Clim.fct))) - 2 *logLik(hacked_clim.fct)
AIC(hacked_het.nre, k = length(coef(Het.fct))) - 2 * logLik(hacked_het.fct)
AIC(hacked_prod.nre, k = length(coef(Prod.nre))) - 2 * logLik(hacked_prod.nre)
AIC(hacked_clim.nre, k = length(coef(Clim.nre))) - 2 * logLik(hacked_clim.nre)



qaics <- AIC(hacked_clim.fct,hacked_het.fct,hacked_prod.nre,hacked_full.nre)                                 # AIC
qdelta.aics <- qaics$AIC - min(qaics$AIC)  # Delta AIC
qwi <- exp(-0.5*qdelta.aics)/sum(exp(-0.5*qdelta.aics))# Akaike weights

qlogliks <- c(logLik(hacked_clim.fct),logLik(hacked_prod.nre),logLik(hacked_het.nre), logLik(hacked_full.nre))        # log Likelihoods
models <- c("Climatic Stability", "Productivity","Heterogeneity", "Full")                          # model names
data.frame(models, -2*qlogliks, numpar=qaics$df, AIC=qaics$AIC, qdelta.aics,qwi)


##############################################################################
###############################################################################
######################## Full  qAIC 
##############################################################################

#################### Quasi GLM 


hacked_clim.nre <- update(Clim.nre, family = hacked.quasipoisson)
hacked_het.nre <- update(Het.nre, family = hacked.quasipoisson)
hacked_prod.nre <- update(Prod.nre, family = hacked.quasipoisson)
hacked_full.nre <- update(full.nre, family = hacked.quasipoisson)

AIC(hacked_full.nre, k = length(coef(full.nre))) - 2 *logLik(hacked_full.nre)
AIC(hacked_het.nre, k = length(coef(Het.nre))) - 2 * logLik(hacked_het.nre)
AIC(hacked_prod.nre, k = length(coef(Prod.nre))) - 2 * logLik(hacked_prod.nre)
AIC(hacked_clim.nre, k = length(coef(Clim.nre))) - 2 * logLik(hacked_clim.nre)



qaics <- AIC(hacked_clim.nre,hacked_het.nre,hacked_prod.nre,hacked_full.nre)                                 # AIC
qdelta.aics <- qaics$AIC - min(qaics$AIC)  # Delta AIC
qwi <- exp(-0.5*qdelta.aics)/sum(exp(-0.5*qdelta.aics))# Akaike weights

qlogliks <- c(logLik(hacked_clim.nre),logLik(hacked_prod.nre),logLik(hacked_het.nre), logLik(hacked_full.nre))        # log Likelihoods
models <- c("Climatic Stability", "Productivity","Heterogeneity", "Full")                          # model names
data.frame(models, -2*qlogliks, numpar=qaics$df, AIC=qaics$AIC, qdelta.aics,qwi)

    
