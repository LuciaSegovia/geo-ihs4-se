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
ea <- st_read(here::here( "data", 
            "boundaries", "mwi_admbnda_adm4_nso.shp")) %>% 
  mutate(ADM2_PCODE = gsub("MW", "",ADM2_PCODE ))

# Predicted maize Se conc. (predicted in geo-spatial/01_maize-model.R)
predmaize.df <- read.csv(here::here("data", "maize",
                                    "2024-05-03Se_raw_OK_expmaize.csv")) %>% 
  rename(predSe = "Zhat_exp")
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

# Need to filter out the EAs from different District
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



# sum(is.na(geopred_ea$EACODE))
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

# Calculating the maize grain Se conc per EA group
predmaize_group <- geopred_ea %>% 
  st_drop_geometry() %>% filter(!is.na(EACODE)) %>% 
  # Removing the lakes
  #filter(!grepl("lake", DISTRICT, ignore.case = TRUE))  %>%
#  mutate_at("DIST_CODE", as.character) %>% 
  right_join(., geodata_ea) %>%  filter(!is.na(predSe)) 

Se_cluster  <- Se_cluster %>% 
  group_by(ea_id) %>% 
  summarise(Se_mean = mean(predSe, na.rm = TRUE), 
            Se_sd = sd(predSe, na.rm = TRUE), 
            Se_median = median(predSe, na.rm = TRUE), 
            Se_iqr = IQR(predSe, na.rm = TRUE), 
            EA_n = length(unique(EACODE)), 
            Se_n = n()) 

# Saving the predicted maize Se conc. per cluster
write.csv(Se_cluster, here::here("data", "maize", 
           "predSe_ihs4_cluster.csv"), row.names = FALSE)

#length(unique(geo$case_id)) 12447
length(unique(geodata_ea$case_id)) 

# Checking the values per district

predmaize_group %>% 
  ggplot(aes(Se_mean, DISTRICT)) + geom_boxplot()

## Saving the values per EA group 
predmaize_group %>% 
  write.csv(.,here::here("data", "inter-output",
                                 "predSe-maize_ihs4-EA-group.csv"))

# Calculating the FW & different fractions of maize products ----
## NCT data (IHS5)
nct <- read.csv(here::here("data", "fct_ihs5_v2.1.csv")) %>% 
  # Selecting only variables of interest
  select(1:8, SE)
head(nct)

# Genereting one table only for maize and products
# Checking maize items
nct %>% filter(grepl("maize", ihs5_fooditem, ignore.case = TRUE)) 

maize <- nct %>% filter(grepl("maize", ihs5_fooditem, ignore.case = TRUE)) %>%  
  filter(ihs5_foodid != 118) 


# Need water concentration for conversion from DW to FW. 
# From Malawi FCT (from the IH5 NCT)

maize.df <- predmaize_group %>% # Water and unit conversion (mg 100g-1 FW)
  mutate(
    maize_101 = Se_mean*(100-maize$WATER[maize$ihs5_foodid == "101"])/1000, 
    maize_102 = Se_mean*(100-maize$WATER[maize$ihs5_foodid == "102"])/1000, 
    maize_103 = Se_mean*(100-maize$WATER[maize$ihs5_foodid == "103"])/1000, 
    maize_104 = Se_mean*(100-maize$WATER[maize$ihs5_foodid == "104"])/1000, 
    maize_105 = Se_mean*(100-maize$WATER[maize$ihs5_foodid == "105"])/1000, 
    maize_820 = Se_mean*(100-maize$WATER[maize$ihs5_foodid == "820"])/1000) %>% 
  select(-c(1:4))

# Estimating Se apparent intake ----


test <- st_rasterize(geopredmaize.df %>% dplyr::select( predSe,geometry))

plot(test)

# crop the raster
test2 <- st_crop(test, ea_selected)

plot(hfp_meso)

