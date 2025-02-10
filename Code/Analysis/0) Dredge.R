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
library(corrplot)

#Load thesis data 
# Cleaning the data
CFR_data <- readRDS("Data/CFR_data")



##############################################################################
###############################################################################
##########################correlation 

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

#Save output
ggsave("Output/Figure A1.png", plot_cor, width = 120, height = 90, units = "cm")

#Read the Z standardize data  
CFR_data_anlys <- readRDS("Data/CFR_data_anlys")


##############################################################################
###############################################################################
########################## TOTAL spp dredge 
##############################################################################

#fit full poisson model

Total.qpossion <- glm(Total_species_richness ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                          Mean_annual_Temperature + mean_annual_Rainfall + 
                          Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                          Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                        , family = "quasipoisson" , data = CFR_data_anlys)

#fit full poisson model

Total.possion <- glm(Total_species_richness ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                          Mean_annual_Temperature + mean_annual_Rainfall + 
                          Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                          Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                        , family = "poisson" , data = CFR_data_anlys)

# Create a function
dfun <- function(object) {
  with(object,sum((weights * residuals^2)[weights > 0])/df.residual)
}

# Dredge function for get the best variables 

if (require("MuMIn")) {
  packageVersion("MuMIn")
  ## from ?QAIC
  x.quasipoisson <- function(...) {
    res <- quasipoisson(...)
    res$aic <- poisson(...)$aic
    res
  }
  Total.qpossion <- update(Total.qpossion,family="x.quasipoisson",
                     na.action=na.fail)
  (Totalspp <- dredge(Total.qpossion,rank="QAIC", chat=dfun(Total.possion)))
  
}

# Subset to only keep delta 2 and below  
Totalspp = subset(Totalspp, delta < 2)


write_xlsx(Totalspp, "Data/Dredge species richness.xlsx")


# Best model 

Totalspp_best_model <- glm(Total_species_richness ~ percent_wetland + Mean_CSI+
                             Mean_annual_Temperature + mean_annual_Rainfall + 
                             Mean_Ext_P + Range_Temperature + Roughness_Dry_Rainfall
                           , family = "quasipoisson" , data = CFR_data_anlys)


##############################################################################
###############################################################################
########################## nre spp dredge 
##############################################################################

#fit full poisson model

NRE.qpossion <- glm(Narrow_Range_endemics ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                        Mean_annual_Temperature + mean_annual_Rainfall + 
                        Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                        Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                      , family = "quasipoisson" , data = CFR_data_anlys)

#fit full poisson model

NRE.possion <- glm(Narrow_Range_endemics ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                       Mean_annual_Temperature + mean_annual_Rainfall + 
                       Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                       Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                     , family = "poisson" , data = CFR_data_anlys)

# Dredge function for get the best variables 

NRE.qpossion <- update(NRE.qpossion,family="x.quasipoisson",
                           na.action=na.fail)

# Run the dredge function 
Nrespp <- dredge(NRE.qpossion,rank="QAIC", chat=dfun(NRE.possion))

# Subset to only keep delta 2 and below  
Nrespp = subset(Nrespp, delta < 2)

# save output
write_xlsx(Nrespp, "Data/Dredge narrow-range endemics.xlsx")

#Best model 

NRE.best_model <- glm(Narrow_Range_endemics ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                        Mean_Soil_ph + Range_Temperature + Roughness_Dry_Rainfall
                      ,family = "quasipoisson" , data = CFR_data_anlys)

##############################################################################
###############################################################################
########################## wetland spp dredge 
##############################################################################

#fit full poisson model

CFR_data_anlys$Wetland_spp <- CFR_data_anlys$Facultative_species + CFR_data_anlys$Obligate_species

#fit full poisson model

Wetspp.qpossion <- glm(Wetland_spp ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                      Mean_annual_Temperature + mean_annual_Rainfall + 
                      Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                      Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                    , family = "quasipoisson" , data = CFR_data_anlys)

#fit full poisson model

Wetspp.possion <- glm(Wetland_spp ~ Median_CloudFQ + percent_wetland + Mean_CSI+
                     Mean_annual_Temperature + mean_annual_Rainfall + 
                     Mean_PET + Mean_Soil_ph + Mean_Ext_P +
                     Range_Temperature + Roughness_Dry_Rainfall + PET_Roughness
                   , family = "poisson" , data = CFR_data_anlys)

# Dredge function for get the best variables 

Wetspp.qpossion <- update(Wetspp.qpossion,family="x.quasipoisson",
                       na.action=na.fail)
# run the dredge function 
Wetspp <- dredge(Wetspp.qpossion,rank="QAIC", chat=dfun(Wetspp.possion))


# Subset to only keep delta 2 and below  
Nrespp = subset(Wetspp, delta < 2)

# save the dredge output
write_xlsx(Wetspp, "Data/Dredge Wetland-dependent NRE.xlsx")

#Best model wetland

Wetspp_best_model <- glm(Wetland_spp ~ Median_CloudFQ  + Mean_CSI+
                           mean_annual_Rainfall + 
                           Mean_Soil_ph + 
                           Range_Temperature + Roughness_Dry_Rainfall
                         , family = "quasipoisson" , data = CFR_data_anlys)

