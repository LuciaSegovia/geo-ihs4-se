###############################################################
#
#                This script produced the IHS4 NCT
#               for cluster-specific maize Se conc.
#
# 
###############################################################

# Loading libraries
library(dplyr) # Data cleaning 
library(tidyr) # Data tidying 
library(ggplot2) # Data viz


# Loading data
# Loading food list
food <- readRDS(here::here("data", "food-list_ihs4_v2.1.0.rds"))

# maize Se cluster (mg kg-1 DM) # generated in se_cluster.R
cluster <- read.csv(here::here("data", "maize", "predSe_ihs4_cluster_v1.0.0.csv"))

# Getting dictionary codes for mineral compo
dict <- read.csv(here::here("data", "dict_fct_compilation_v.1.4.0.csv"))

dict_nct <- read.csv(here::here("data", "fct_ihs5_v2.2.csv")) %>%   
  # Selecting only variables of interest
  select(1:6) %>% 
  separate_rows(c(ref_source, ref_fctcode, ref_fctitem), sep = ";") %>% 
  separate_rows(c(ref_fctcode), sep = ",") %>% 
  # Joining w/ dict. codes
  left_join(., dict,  by = c("ref_fctcode")) 


dict_nct %>% filter(is.na(ID_3)) %>% arrange(ref_source) %>% View()

  ## NCT data (IHS5)
nct <-   read.csv(here::here("data", "fct_ihs5_v2.2.csv")) %>%   
    # Selecting only variables of interest
  select(1:8, SE, comment) 

head(nct)

## Joy et al Se data (Malawi)
Se <- read.csv(here::here("data", "mineral-composition_2021-04-13.csv")) %>% 
  # Selecting only variables of interest
  select(1:6, water, water_ref, se_median, se_mcg_100g)
head(Se)

# Adjusting the dataset: creating a food_desc variable
Se.df <- Se %>% 
  mutate( foodnotes = ifelse(is.na(foodnotes), "raw", tolower(foodnotes)), 
    food_desc = paste(fooditem, tolower(foodtissue), foodnotes, sep = ","))

# Amending codes
Se.df$water_ref[Se.df$water_ref == "04_110"] <- "04_004"

#Joining with the dictionary codes
Se.df <- Se.df %>% left_join(., dict, 
                    by = c("water_ref" = "ref_fctcode")) 



Se.df %>% filter(is.na(ID_3)) %>% View()

## Review NCT matches for Se
sum(is.na(nct$SE))
nct %>% filter(SE>50)
hist(nct$SE)


names(nct)
names(Se.df)

test <- nct %>% 
  left_join(., Se.df, by = c("ref_fctcode"= "water_ref")) 

test %>% mutate(
  food_desc = ifelse(is.na(food_desc), ref_fctitem, food_desc),
  se_mcg_100g = ifelse(is.na(se_mcg_100g), SE, se_mcg_100g)) %>% View()

# Calculating the FW & different fractions of maize products ----

# Checking maize items
nct %>% filter(grepl("maize", ihs5_fooditem, ignore.case = TRUE)) 

## Extracting ratio ----

## Maize to flour ration (Supl.Table6, Joy et al., 2015)
ratio <- readxl::read_excel(here::here("data", "maize",
                                       "40795_2015_36_MOESM1_ESM.xlsx"), 
                            sheet = 6, skip = 2)
head(ratio)

# Only need column 1 (element) and column 7 ratio
ratio <- ratio %>% select(c(1, 7)) %>%
  filter(!is.na(Element)) %>% 
  rename(ratio_refine="Mean ratio refined flour:whole grain") %>% 
  mutate(crop = "maize")

names(ratio)

# Generating bran ratio
Se_ratio <- ratio$ratio_refine[ratio$Element == "Se"]


# Need water concentration for conversion from DW to FW. 
# From Malawi FCT (from the IH5 NCT)
# Water and unit conversion (mg 100g-1 FW) & ratio conversion
maize.df <- cluster %>% 
  mutate(
    maize_101 = Se_mean*(100-nct$WATER[nct$ihs5_foodid == "101"]), 
    maize_102 = Se_mean*(100-nct$WATER[nct$ihs5_foodid == "102"])*Se_ratio, 
    maize_103 = Se_mean*(100-nct$WATER[nct$ihs5_foodid == "103"])*(1-Se_ratio), 
    maize_104 = Se_mean*(100-nct$WATER[nct$ihs5_foodid == "104"]), 
    maize_105 = Se_mean*(100-nct$WATER[nct$ihs5_foodid == "105"]), 
    maize_820 = Se_mean*(100-nct$WATER[nct$ihs5_foodid == "820"])) %>% 
  select(-c(2:5))

head(maize.df)

sum(is.na(maize.df$maize_101))


maize.df <- maize.df %>% pivot_longer(., 
                                      cols = starts_with("maize"),
                                      names_to = "food_code", 
                                      names_prefix = "maize_", 
                                      values_to = "Se_mcg_100g") %>%
  arrange(food_code)

## Saving ea maize Se nct ----

saveRDS(maize.df, here::here("data", "inter-output", "ea-maize-se-nct.RDS"))





