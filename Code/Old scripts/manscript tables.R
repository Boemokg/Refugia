
MoranI_Total <- moran.test(CFR_data_anlys$Total_species_richness, listed_w)
total_moran <-MoranI_Total$estimate[1]
total_p <-MoranI_Total$p.value

MoranI_NRE <- moran.test(CFR_data_anlys$Narrow_Range_endemics, listed_w)
NRE_moran <-MoranI_NRE$estimate[1]
NRE_p <-MoranI_NRE$p.value

CFR_data_anlys$Wetland_spp <- CFR_data_anlys$Facultative_species + CFR_data_anlys$Obligate_species
MoranI_wet <- moran.test(CFR_data_anlys$Wetland_spp, listed_w)
wet_moran <-MoranI_wet$estimate[1]
wet_p <-MoranI_wet$p.value

MoranI_fct <- moran.test(CFR_data_anlys$Facultative_species, listed_w)
fct_moran  <-MoranI_fct$estimate[1]
fct_p <-MoranI_fct$p.value

MoranI_oblg <- moran.test(CFR_data_anlys$Obligate_species, listed_w)
oblg_moran <-MoranI_oblg$estimate[1]
oblg_p <-MoranI_oblg$p.value

Response_Type = c('Species Richness',
                  'Narrow range endemics',
         'Wetland-dependent Narrow range endemics',
         'facultative wetland-dependent Narrow range endemics',
          'obligate Wetland-dependent Narrow range endemics') 

Estimates = c(0.4483599 ,
              0.2236273 ,
              0.3314042,
              0.313714,
              0.2935114)
pvalues = c(total_p,
            NRE_p, 
            wet_p, 
            fct_p, 
            oblg_p)
moranI = cbind(Response_Type,Estimates, pvalues) %>% as.data.frame()



# save the dredge output
write_xlsx(moranI, "Data/moranI.xlsx")
