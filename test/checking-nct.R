####################################################################
#                                                                  #
#          This script matches the NCT (nutrient info)             #         
#             with the app. consumption IHS4                       #         
#                                                                  # 
#                                                                  #  
####################################################################    

# Loading the packages -----
library(dplyr) # Data cleaning 
library(tidyr) # Data manipulation
library(ggplot2) # Data viz
library(ggridges) # Data viz:ridges
#library(sp) # Spatial data manipulation
library(sf) # Spatial data manipulation
#library(stars) # Spatial data manipulation
#library(tmap) # Spatial data viz

# Loading the data -----

## NCT data (IHS4) from fct repo (NCTs/ihs4_nct.R) 
# https://github.com/LuciaSegovia/fct/blob/main/NCTs/ihs4_nct.R
nct1 <-   read.csv(here::here("data", "nct", "ihs4_nct_SEmcg_v1.0.0.csv")) %>%
  # Excluding 118 not present in ihs4
  filter(!code %in% c("118", "204b", "831b", "832b"))

## NCT data (IHS4) from fct repo (NCTs/ihs4_nct.R) 
# https://github.com/LuciaSegovia/fct/blob/main/NCTs/ihs4_nct.R
nct2 <-   read.csv(here::here("data", "nct", "ihs4_nct_SEmcg_v2.0.0.csv")) %>%
  # Excluding 118 not present in ihs4
  filter(!code %in% c("118", "204b", "831b", "832b"))


names(nct2)

check <- left_join(nct1, nct2, by = c("code", "item"))

check <- check %>% mutate(
  SE_check = ifelse(SEmcg.x==SEmcg.y, "OK", SEmcg.x-SEmcg.y),
  Enerc_check = ifelse(ENERCkcal.x==ENERCkcal.y, "OK", ENERCkcal.x-ENERCkcal.y))

check <- food_list %>% left_join(., check) %>% 
  filter(SE_check >0)
