#Computing “quasi-AIC” (QAIC)
#For this i will require e QAIC: AICcmodavg and MuMIn
#Use the dredge function and model.av function to get the best model 
#fit the model twice, once with a regular likelihood model (family=binomial,
#poisson, etc.) and once with the quasi- variant — extract the loglikelihood 
#from the former and the dispersion parameter from the latter

#1) fit a normal possion model on the full model 
#2) the fit the quasi-possion model 
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


#Load thesis data 

CFR_data_fin <- readRDS("CFR_data_fin")


#We fit our full model with the quasi-poisson 
full.nre.possion <- glm(Narrow_Range_endemics ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                  Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                  Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                , family = "poisson" , data = CFR_data_anlys)

#We also fit our full model with the family extension poisson
full.nre <- glm(Narrow_Range_endemics ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                  Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                  Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                , family = "quasipoisson" , data = CFR_data_anlys)


#We create a function that sums the prodcut of the weights and squared residuals
dfun <- function(object) {
  with(object,sum((weights * residuals^2)[weights > 0])/df.residual)
}

#calculate look for th best covariate using dredge 

if (require("MuMIn")) {
  packageVersion("MuMIn")
  ## from ?QAIC
  x.quasipoisson <- function(...) {
    res <- quasipoisson(...)
    res$aic <- poisson(...)$aic
    res
  }
  full.nre <- update(full.nre,family="x.quasipoisson",
                        na.action=na.fail)
  (gg <- dredge(full.nre,rank="QAIC", chat=dfun(full.nre.possion)))
  
}

write_xlsx(gg, "dredge.xlsx")

#full model 
CFR_data_fin= CFR_data_fin %>% filter(!is.na(Total_species_richness))

#We fit our full model with the quasi-poisson 
Full_mod.poisson <- glm(Total_species_richness~Mean_annual_Temperature.x + mean_Annual_Rainfall.x +
                          Range_Temperature + Roughness_Temperature.x  + Mean_Ext_P.x + Mean_CloudFQ.x+
                          Mean_CSI.x +Median_Soil_ph.x + Wetland_Coverage + Mean_PET.x + Mean_Ext_P.x, family = "quasipoisson" , data = CFR_data_fin)

#We also fit our full model with the family extension poisson
Full_mod.quasi <- glm(Total_species_richness~Mean_annual_Temperature.x + mean_Annual_Rainfall.x +
                        Range_Temperature + Roughness_Temperature.x  + Mean_Ext_P.x + Mean_CloudFQ.x+
                        Mean_CSI.x +Median_Soil_ph.x + Wetland_Coverage + Mean_PET.x + Mean_Ext_P.x, family = "poisson" , data = CFR_data_fin)

dfun <- function(object) {
  with(object,sum((weights * residuals^2)[weights > 0])/df.residual)
}

#calculate look for th best covariate using dredge 

if (require("MuMIn")) {
  packageVersion("MuMIn")
  ## from ?QAIC
  x.quasipoisson <- function(...) {
    res <- quasipoisson(...)
    res$aic <- poisson(...)$aic
    res
  }
  Full_mod.quasi <- update(Full_mod.quasi,family="x.quasipoisson",
                           na.action=na.fail)
  (tt <- dredge(Full_mod.quasi,rank="QAIC", chat=dfun(Full_mod.poisson)))
  
}

write_xlsx(tt, "tt.xlsx")


#facultative 

full.fct.poisson <- glm(Facultative_species ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                  Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                  Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                , family = "poisson" , data = CFR_data_anlys)

#We also fit our full model with the family extension poisson
full.fct <- glm(Facultative_species ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                  Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                  Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                , family = "quasipoisson" , data = CFR_data_anlys)

dfun <- function(object) {
  with(object,sum((weights * residuals^2)[weights > 0])/df.residual)
}

#calculate look for th best covariate using dredge 

if (require("MuMIn")) {
  packageVersion("MuMIn")
  ## from ?QAIC
  x.quasipoisson <- function(...) {
    res <- quasipoisson(...)
    res$aic <- poisson(...)$aic
    res
  }
  full.fct <- update(full.fct,family="x.quasipoisson",
                           na.action=na.fail)
  (fct.d <- dredge(full.fct,rank="QAIC", chat=dfun(full.fct.poisson)))
  
}

write_xlsx(fct.d, "Facultative wetland dependent dredge for full model.xlsx")




#Obligate 


full.ob.poisson <- glm(Obligate_species ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                          Mean_annual_Temperature + mean_annual_Rainfall + 
                          Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                          Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                        , family = "poisson" , data = CFR_data_anlys)

#We also fit our full model with the family extension poisson
full.ob <- glm(Obligate_species ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                  Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                  Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                , family = "quasipoisson" , data = CFR_data_anlys)

dfun <- function(object) {
  with(object,sum((weights * residuals^2)[weights > 0])/df.residual)
}

#calculate look for th best covariate using dredge 

if (require("MuMIn")) {
  packageVersion("MuMIn")
  ## from ?QAIC
  x.quasipoisson <- function(...) {
    res <- quasipoisson(...)
    res$aic <- poisson(...)$aic
    res
  }
  full.ob <- update(full.ob,family="x.quasipoisson",
                     na.action=na.fail)
  (fct.d <- dredge(full.ob,rank="QAIC", chat=dfun(full.ob.poisson)))
  
}

write_xlsx(fct.d, "Obligate wetland dependent dredge for full model.xlsx")



##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################

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
ggplot() + geom_sf(data = CFR_data_fin, aes(fill = narrow_range.x, colour = narrow_range.x))

CFR_data_fin$All_resid <- Full_mod.1$residuals
ggplot() + geom_sf(data = CFR_data_fin, aes(fill = All_resid, colour = All_resid))


#look at the model residuals 
plot(Full_mod.1$residuals)
plot()

#creating queen's neigbours 
Neighbours <- poly2nb(CFR_data_fin)

#matrics of weights
listed_w <- spdep :: nb2listw(Neighbours, zero.policy = TRUE)



# Replace missing values with zeros in Total_species_richness
CFR_data_fin$Total_species_richness <- ifelse(is.na(CFR_data_fin$Total_species_richness),
                                              0,
                                              CFR_data_fin$Total_species_richness)

# Perform Moran's test
MoranI_Total <- moran.test(CFR_data_fin$Total_species_richness, listed_w)

print(MoranI_Total)

MoranI_Obligate <- moran.test(CFR_data_fin$Obligate_species, listed_w)

print(MoranI_Obligate)


MoranI_facultative <- moran.test(CFR_data_fin$Narrow_range_spp, listed_w)

print(MoranI_facultative)



#Checking for NAs
#any(is.na(CFR_data_fin$Narrow_range_spp.x))
#any(is.na(CFR_data_fin))
#any(is.na(listed_w))
#Fit the model

Neighbours <- poly2nb(CFR_data_fin)

#matrics of weights
listed_w <- spdep :: nb2listw(Neighbours, zero.policy = TRUE)

narrow_clim.sp <- lagsarlm(Clim.m, data= CFR_data_fin, listed_w, zero.policy=TRUE)

narrow_prod.sp <- lagsarlm(Prod.m, data= CFR_data_fin, listed_w, zero.policy=TRUE)

narrow_het.sp <- lagsarlm(Het.m, data= CFR_data_fin, listed_w, zero.policy=TRUE)

narrow_full.sp <- lagsarlm(Full_mod.1, data= CFR_data_fin, listed_w, zero.policy=TRUE)

######################

tot_clim.sp <- lagsarlm(Clim.tot, data= CFR_data_fin, listed_w, zero.policy=TRUE)

tot_pro.sp <- lagsarlm(Prod.tot, data= CFR_data_fin, listed_w, zero.policy=TRUE)

tot_het.sp <- lagsarlm(Het.tot, data= CFR_data_fin, listed_w, zero.policy=TRUE)

tot_full.sp <- lagsarlm(Full_mod.tot, data= CFR_data_fin, listed_w, zero.policy=TRUE)

#####


fact_clim.sp <- lagsarlm(Clim.fac, data= CFR_data_fin, listed_w, zero.policy=TRUE)

fact_pro.sp <- lagsarlm(Prod.fac, data= CFR_data_fin, listed_w, zero.policy=TRUE)

fact_het.sp <- llagsarlm(Het.fac, data= CFR_data_fin, listed_w, zero.policy=TRUE)

fact_full.sp <- lagsarlm(Full_mod.fac, data= CFR_data_fin, listed_w, zero.policy=TRUE)

####

obg_clim.sp <- lagsarlm(Clim.obl, data= CFR_data_fin, listed_w, zero.policy=TRUE)

obg_pro.sp <- lagsarlm(Prod.obl, data= CFR_data_fin, listed_w, zero.policy=TRUE)

obg_het.sp <- lagsarlm(Het.obl, data= CFR_data_fin, listed_w, zero.policy=TRUE)

obg_full.sp <- lagsarlm(Full_mod.obl, data= CFR_data_fin, listed_w, zero.policy=TRUE)

##
names(Full_mod.1_Spatial)

#Model summary
summary(Full_mod.1_Spatial)

#Model checking
CFR_data_fin$All_resid_Sp <- Full_mod.1_Spatial$residuals
CFR_data_fin$All_rho <- Full_mod.1_Spatial$rho

ggplot() + geom_sf(data = CFR_data_fin, aes(fill = All_resid_Sp, colour = All_resid_Sp))
ggplot() + geom_sf(data = CFR_data_fin, aes(fill = All_rho, colour = All_rho))

All_sp_pred <- predict(Full_mod.1_Spatial, CFR_data_fin, listed_w, zero.policy=TRUE)

All_pred <- predict(Full_mod.1, CFR_data_fin)

######Model comparison 


#Load thesis data 

CFR_data_fin <- readRDS("CFR_data_fin")

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
#SAR
narrow_clim.sp <- lagsarlm(Clim.nre, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

#Model summary 
summary(narrow_clim.sp)

#water and energy hypothesis model
Prod.nre <- glm(Narrow_Range_endemics~ Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P, family = "quasipoisson" , data = CFR_data_anlys)
#SAR
narrow_prod.sp <- lagsarlm(Prod.nre, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

#Model summary
summary(narrow_prod.sp)

#Environmental heterogeneity Model
Het.nre <- glm(Narrow_Range_endemics ~
                 Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
               , family = "quasipoisson" , data = CFR_data_anlys)

#SAR
narrow_het.sp <- lagsarlm(Het.nre, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

#Model summary
summary(narrow_het.sp)


#full model
full.nre <- glm(Narrow_Range_endemics ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                  Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                  Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                , family = "quasipoisson" , data = CFR_data_anlys)

#SAR
narrow_full.sp <- lagsarlm(full.nre, data= CFR_data_anlys, listed_w, zero.policy=TRUE)

#Model summary
summary(narrow_full.sp)


# ***************************************************************************
# the following piece of code extracts log Likelihoods and AIC, and calculates
# the other things needed for a AIC based model selection.
# In the end, it produces a model selection table
# ************************************************************************

aics_o <- AIC(ob_clim.sp,ob_prod.sp,ob_het.sp,ob_full.sp)                                 # AIC
delta.aics <- aics_o$AIC - min(aics_o$AIC)                # Delta AIC
wi <- exp(-0.5*delta.aics)/sum(exp(-0.5*delta.aics))  # Akaike weights

logliks <- c(logLik(narrow_clim.sp),logLik(narrow_prod.sp),logLik(narrow_het.sp), logLik(narrow_full.sp))        # log Likelihoods
models <- c("Climatic Stability", "Productivity","Heterogeneity", "Full")                          # model names

(modtable_o <- data.frame(models, -2*logliks, numpar=aics_o$df, AIC=aics_o$AIC, delta.aics,wi))
write.csv(modtable, "modtable.csv")

