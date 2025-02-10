##############################################################################
######## Code to explore evidence spatial autocorrelation extent
##############################################################################
######## Compiled by Kagiso Nhlapo
######## Last edited: June 2024
##############################################################################
##############################################################################
###Steps:
###1a) Get libraries etc
###1b) get weights  
###2) Read in species data
###3) use the full model find the best sp model with LM.test
###
##############################################################################

##############################################################################
### Get libraries
##############################################################################

library(tidyverse)
library(spdep)
library(spatialreg)
library(writexl)


##############################################################################
### Get data
##############################################################################

# #Z standardize cfr data
CFR_data_anlys <- readRDS("Data/CFR_data_anlys")

#wetland data 
CFR_data_anlys$wetland_nre <- CFR_data_anlys$Facultative_species+
  CFR_data_anlys$Obligate_species


##############################################################################
### Get the sp weights 
##############################################################################


#creating queen's neigbours 
Neighbours <- poly2nb(CFR_data_anlys)

# Create weights list object
listed_w <- nb2listw(Neighbours, zero.policy = TRUE)

# Convert to spatial weights matrix
W <-nb2mat(Neighbours, style = "B", zero.policy = TRUE)

# Check the structure of the weights matrix
str(W)

# Perform Moran's test for all response varibles 
# SR
MoranI_Total <- moran.test(CFR_data_anlys$Total_species_richness, listed_w)
print(MoranI_Total)

# NRE
MoranI_NRE <- moran.test(CFR_data_anlys$Narrow_Range_endemics, listed_w)
print(MoranI_NRE)

# WET
MoranI_WET <- moran.test(CFR_data_anlys$wetland_nre, listed_w)
print(MoranI_WET)

# FCT
MoranI_FCT <- moran.test(CFR_data_anlys$Facultative_species, listed_w)
print(MoranI_FCT)

# OBG
MoranI_OBG <- moran.test(CFR_data_anlys$Obligate_species, listed_w)
print(MoranI_OBG)

##############################################################################
### Get all the models 
##############################################################################

# Total species richness 
full.tot <- glm(Total_species_richness ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                  Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                  Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                , family = "quasipoisson" , data = CFR_data_anlys)

# narrow range endemics 
nre.tot <- glm(Narrow_Range_endemics ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                  Mean_annual_Temperature + mean_annual_Rainfall + 
                  Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                  Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                , family = "quasipoisson" , data = CFR_data_anlys)

# Wetland dependent nre
wdnre.tot <- glm(wetland_nre ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                 Mean_annual_Temperature + mean_annual_Rainfall + 
                 Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                 Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
               , family = "quasipoisson" , data = CFR_data_anlys)

# facultative nre
fctnre.tot <- glm(Facultative_species ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                   Mean_annual_Temperature + mean_annual_Rainfall + 
                   Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                   Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                 , family = "quasipoisson" , data = CFR_data_anlys)

# obligate nre
obnre.tot <- glm(Obligate_species ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                    Mean_annual_Temperature + mean_annual_Rainfall + 
                    Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                    Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                  , family = "quasipoisson" , data = CFR_data_anlys)

##############################################################################
### LM test 
##############################################################################


#Contact a Lagrange Multiplier test, to test spatial dependence in linear models

#CHECKING spatial autocorrelation (fOR EACH MODEL AND EACH RESPONSE) : 
# 1) residuals 
# 2) response 
# 3) The adjusted residuals 
# 4) The adjusted response 
# 5) The adjusted residuals and response (SARMA) 

# Fit Rao's score for the Full - SR
Tot.LMT<- lm.LMtests(full.tot, listed_w, 
                                test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
#save as a dataframe
Tot.LMT = as.data.frame(summary(Tot.LMT)$result)

# Fit Rao's score for the Full - NRE
NRE.LMT<- lm.LMtests(nre.tot, listed_w, 
                     test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))

#save as a dataframe
NRE.LMT = as.data.frame(summary(NRE.LMT)$result)

# Fit Rao's score for the Full - WD NRE
WD.LMT<- lm.LMtests(wdnre.tot, listed_w, 
                     test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
#save as a dataframe
WD.LMT = as.data.frame(summary(WD.LMT)$result)

# Fit Rao's score for the Full - FCT NRE
FCT.LMT<- lm.LMtests(fctnre.tot, listed_w, 
                     test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
#save as a dataframe
FCT.LMT = as.data.frame(summary(FCT.LMT)$result)

# Fit Rao's score for the Full - OBG NRE
OBG.LMT<- lm.LMtests(obnre.tot, listed_w, 
                     test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
#save as a dataframe
OBG.LMT = as.data.frame(summary(OBG.LMT)$result)

#combine all data
LMT = cbind(Tot.LMT,NRE.LMT, WD.LMT, FCT.LMT, OBG.LMT)

#Then save 
write_xlsx(LMT, "Data/LM-Test.xlsx")
