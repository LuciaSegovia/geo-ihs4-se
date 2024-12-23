####################################################################
#                                                                  #
#                  This script generate the maize Se conc.         #         
#                        for each IHS4 cluster                     #         
#                                                                  # 
#                                                                  #  
####################################################################    

library(dplyr) # Data cleaning 
library(ggplot2) # Data viz
#library(sp) # Spatial data manipulation
library(sf) # Spatial data manipulation
library(stars) # Spatial data manipulation
library(tmap) # Spatial data viz


# Loading data

## Loading Malawi EA shapefile (generated in geo-spatial/00.cleaning-boundaries.R)
ea <- st_read(here::here( "data", "boundaries", "mwi_admbnda_adm4_nso.shp")) %>% 
  mutate(ADM2_PCODE = gsub("MW", "",ADM2_PCODE ))

# Predicted maize Se conc. (predicted in geo-spatial/01_maize-model.R)
predmaize.df <- read.csv(here::here("data", "maize", "2024-05-03Se_raw_OK_expmaize.csv")) %>% 
  dplyr::rename(predSe = "Zhat_exp")
#names(predmaize.df)

# Transforming predicted maize Se conc. into a spatial obj.
geopredmaize.df <-  st_as_sf( predmaize.df , coords =c("Longitude", "Latitude"),
                              crs = "EPSG:4326")

# Offset IHS4 HH locations
geodata.df <- st_read(here::here("data","boundaries", "ihs4-offset.shp"))

geodata.df$district <- as.character(geodata.df$district)

length(unique(geodata.df$ea_id))
length(unique(geodata.df$case_id))
#table(st_is_valid(geodata.df))
#table(st_is_empty(geodata.df))

# Changing orphan HH to EA geometry
geodata.df$geometry[geodata.df$case_id == "103041560095"] <- geodata.df$geometry[geodata.df$case_id == "103041560078"]

# Checking how many EAs has a different geometry.. 
## According to documentation it should all have the same...
#geodata.df %>% distinct(ea_id, geometry) # %>% 
#  st_drop_geometry() %>% duplicated()


## Identifying EAs w/i the buffer area -----
# Getting info on the admin boundaries (EA/district level)
# Allocating Se values to each admin unit

# For some reason it was not working for all buffered areas
#geodata_ea <-  st_join(geodata.df, ea)
geodata_ea <- sf::st_join(geodata.df, ea, join = st_intersects)

# Checking missing cases
#x <- unique(geodata_ea$case_id)
length(unique(geodata_ea$case_id))
length(unique(geodata_ea$ea_id))

#table(st_is_empty(geodata_ea))
#table(st_is_valid(geodata.df))
#geodata.df %>% filter(!case_id %in% x)

# tm_shape(ea) +
#   tm_borders() +
# tm_shape(geodata.df %>% filter(!case_id %in% x))+
#   tm_borders(col= "red")

# Checking if HH/EAs missing EACODE
#geodata_ea %>% filter(is.na(EACODE))

#geodata_ea$district <- as.character(geodata_ea$district)
geodata_ea$DIST_CODE <- as.character(geodata_ea$DIST_CODE)

# tm_shape(ea) +
#   tm_polygons(col = "DIST_CODE") +
# tm_shape(geodata_ea) +
#   tm_borders( col = "blue")

# Need to filter out the EAs from different district
geodata_ea %>%
  mutate(dist_diff = ifelse(district == ADM2_PCODE, "YES", "NO")) %>% 
  filter(dist_diff == "YES") %>%
  st_drop_geometry() %>% distinct(case_id) %>% count()
  
names(geodata_ea)
#This was the HH w/ unique GPS from the other HHs in the cluster
geodata_ea %>% filter(case_id == "103041560095") # dist == 103

# Getting the EA groups corresponding to each HH cluster
cluster.df <- geodata_ea %>%
  # Dropping geometry to speed-up the process. 
  st_drop_geometry() %>% 
  mutate(dist_diff = ifelse(district == ADM2_PCODE, "YES", "NO")) %>% 
  filter(dist_diff == "YES") %>% 
  select(1:4,6, 11, 17:19)  %>% distinct()

sum(is.na(cluster.df$ea_id))
sum(is.na(cluster.df$EACODE))

# This bit was used to identify the HH above
# geodata_ea %>% filter(!case_id %in% unique(cluster.df$case_id)) %>% 
#   st_drop_geometry() %>% distinct(case_id)

 
 # checking the district, they are not the same codes:
 # We may need to get back to the names...
# tm_shape(ea) +
#   tm_borders() +
#   tm_shape(ea %>% filter(DIST_CODE %in% c("105", "107"))) +
#   tm_polygons(col = "DIST_CODE") 

# Getting the list of EAs included 
ea_selected <- ea %>% 
  filter(EACODE %in% unique(cluster.df$EACODE)) 

# Getting maize grain Se concentration -----
# Next step is getting the predicted maize grain Se concentration for 
# each group of EAs

# geopred_ea <-  st_join(geopredmaize.df, ea_selected)
 geopred_ea <- sf::st_join(geopredmaize.df, ea_selected, join = st_intersects)


#sum(is.na(geopred_ea$EACODE))
# sum(is.na(geopred_ea$predSe))


# Checking that all EA groups has predSe values
geopred_ea %>% 
  st_drop_geometry() %>% filter(!is.na(EACODE)) %>% 
  select(predSe, EACODE) %>% right_join(., cluster.df ) %>%  
  filter(is.na(predSe)) %>% distinct(EACODE) %>% count()

# Checking that all cluster has predSe values
ea_missing <-  geopred_ea %>% 
  st_drop_geometry() %>% filter(!is.na(EACODE)) %>% 
  select(predSe, EACODE) %>% 
  right_join(., cluster.df) %>%  
  filter(!is.na(predSe)) %>% distinct(ea_id) # %>% count()

# Found some EAs w/o values
missing <- geopred_ea %>% 
  st_drop_geometry() %>% filter(!is.na(EACODE)) %>% 
  select(predSe, EACODE) %>% 
  right_join(., cluster.df) %>%  
  filter(is.na(predSe)) %>% distinct(EACODE)

# Seems to have zero or very small area
ea %>% filter(EACODE %in% pull(missing)) %>% View()


# Getting mean maize Se per cluster
Se_cluster <- geopred_ea %>% 
  st_drop_geometry() %>% filter(!is.na(EACODE)) %>% 
  select(predSe, EACODE) %>% 
  right_join(., cluster.df %>% dplyr::select(EACODE, ea_id)) %>% 
  distinct() 

sum(is.na(Se_cluster$predSe))

# Calculating the maize grain Se conc per EA group
predmaize_group <- Se_cluster %>% 
 filter(!is.na(predSe)) 

sum(is.na(predmaize_group$predSe))
length(unique(predmaize_group$ea_id))

missing <- setdiff(unique(geodata.df$ea_id), unique(predmaize_group$ea_id))

# checking the missing ea_ids
# They are all from Likoma. Hence, we are using the mean (see docu)
#tm_shape(geodata.df) +
#  tm_borders() +
#  tm_shape(geodata.df %>% filter(ea_id %in% missing)) +
#  tm_polygons(col = "ea_id") 


Se_cluster  <- Se_cluster %>% 
  group_by(ea_id) %>% 
  summarise(Se_mean = mean(predSe, na.rm = TRUE), 
            Se_sd = sd(predSe, na.rm = TRUE), 
            Se_median = median(predSe, na.rm = TRUE), 
            Se_iqr = IQR(predSe, na.rm = TRUE), 
            EA_n = length(unique(EACODE)), 
            Se_n = n()) 

# Fixing missing values for Likoma using global mean 
Se_cluster  <- Se_cluster %>% mutate(
  comments = ifelse(is.na(Se_mean), "NA replaced to global mean/median", NA), 
  Se_n = ifelse(is.na(Se_mean), length(predmaize.df$predSe), Se_n), 
  Se_mean = ifelse(is.na(Se_mean), mean(predmaize.df$predSe), Se_mean), 
  Se_sd = ifelse(is.na(Se_sd), sd(predmaize.df$predSe), Se_sd),
  Se_median = ifelse(is.na(Se_median), median(predmaize.df$predSe), Se_median), 
  Se_iqr = ifelse(is.na(Se_iqr), IQR(predmaize.df$predSe), Se_iqr))

mean(predmaize.df$predSe)

# Saving the predicted maize Se conc. per cluster
# write.csv(Se_cluster, here::here("data", "maize", 
  #         "predSe_ihs4_cluster_v1.0.0.csv"), row.names = FALSE)



# Testing converting the predicted maize point data into a 
# raster 
# test <- st_rasterize(geopredmaize.df %>% dplyr::select( predSe,geometry))
# 
# plot(test)
# 
# # crop the raster
# test2 <- st_crop(test, ea_selected)
# 


