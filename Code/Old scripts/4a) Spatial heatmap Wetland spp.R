#### Work flow ####

####Complied by Kagiso Nhlapo 
#### MSC student Working on Hydrologic Refugia spatial characteristics 
#### Last edited 24 february 2022

####   To do  ####
###1) Get libraries etc
###2) remove geometry from the the capeFR_for_heatmap
### (y should not have class sf; for spatial joins)
###3) use right_join function to retains all rows of the data on the right side (ObNRE).
###3) join the QDS count data with QDS layer


#remove geometry from the the capeFR_for_heatmap
CapeFR_for_heatmap_ObNRE <- CapeFR_for_heatmap %>% st_set_geometry(NULL)

#returns all records from the left table (table1), and the matching records from the right table (table2)
ObNRE_heatmap <- left_join(ObNRE, CapeFR_for_heatmap_ObNRE, by = "Taxon") 

#return only Taxon, QDS, QDScount columns
ObNRE_heatmap <- ObNRE_heatmap %>% select("Taxon", "QDS", "QDScount","geometry")


#the count of species present per QDS with no duplicates
hmm_ObNRE <- ObNRE_heatmap %>% #original data from 
  st_set_geometry(NULL) %>%#removing geometric object
  select(QDS, Taxon) %>% #select for QSD and taxon
  unique() %>% #only unique taxon per QDS
  group_by(QDS) %>% #grouped by QDS
  summarise(species_richness = n()) #Count of the number of species present per QDS 

#Joining the two data sets and omiting nas
hmm.QDS_obnre <- left_join(QDS, hmm_ObNRE, by = c("qdgc"= "QDS")) %>%
  na.omit()
#Important plot to visualize the data 
ggplot()+
  geom_sf(data = hmm.QDS_obnre, aes(fill = species_richness))+
  geom_polygon()

########Facultative #######

CapeFR_for_heatmap_FctNRE <- CapeFR_for_heatmap %>% st_set_geometry(NULL)

#returns all records from the left table (table1), and the matching records from the right table (table2)
FctNRE_heatmap <- left_join(FctNRE, CapeFR_for_heatmap_FctNRE, by = "Taxon") 

#return only Taxon, QDS, QDScount columns
FctNRE_heatmap <- FctNRE_heatmap %>% select("Taxon", "QDS", "QDScount","geometry")


#the count of species present per QDS with no duplicates
hmm_fct <- FctNRE_heatmap %>% #original data from 
  st_set_geometry(NULL) %>%#removing geometric object
  select(QDS, Taxon) %>% #select for QSD and taxon
  unique() %>% #only unique taxon per QDS
  group_by(QDS) %>% #grouped by QDS
  summarise(species_richness = n()) #Count of the number of species present per QDS 

#Joining the two data sets and omiting nas
hmm_fct_QDS <- left_join(QDS, hmm_fct, by = c("qdgc"= "QDS")) %>%
  na.omit()
#Important plot to visualize the data 
ggplot()+
  geom_sf(data = hmm_fct_QDS, aes(fill = species_richness))+
  geom_polygon()




