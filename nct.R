###############################################################################
#
#                This script produced the IHS4 NCT
#               for cluster-specific & national-level maize Se conc.
#
# 
###############################################################

# Loading libraries
library(dplyr) # Data cleaning 
library(tidyr) # Data tidying 
library(ggplot2) # Data viz


# Loading data
# Loading food list
# food <- readRDS(here::here("data", "food-list_ihs4_v2.1.0.rds"))

# maize Se cluster (mg kg-1 DM) # generated in se_cluster.R
cluster <- read.csv(here::here("data", "maize", "predSe_ihs4_cluster_v1.0.0.csv"))

# Predicted maize Se conc. (predicted in geo-spatial/01_maize-model.R) (in mg kg-1 DM)
predmaize.df <- read.csv(here::here("data", "maize", "2024-05-03Se_raw_OK_expmaize.csv")) %>% 
  dplyr::rename(predSe = "Zhat_exp")

# Getting dictionary codes for mineral compo
# dict <- read.csv(here::here("data", "dict_fct_compilation_v.1.4.0.csv"))

#dict_nct <- read.csv(here::here("data", "fct_ihs5_v2.2.csv")) %>%   
#  # Selecting only variables of interest
#  select(1:6) %>% 
#  separate_rows(c(ref_source, ref_fctcode, ref_fctitem), sep = ";") %>% 
#  separate_rows(c(ref_fctcode), sep = ",") %>% 
#  # Joining w/ dict. codes
#  left_join(., dict,  by = c("ref_fctcode")) 
#

#dict_nct %>% filter(is.na(ID_3)) %>% arrange(ref_source) %>% View()

## NCT data (IHS4)
nct <-   read.csv(here::here("data", "nct", "ihs4_nct_SEmcg_v1.0.0.csv")) %>%
  # Excluding 118 not present in ihs4
  filter(code != "118")
    # Selecting only variables of interest
 # select(1:8, SEmcg) 

head(nct)

# Calculating the FW & different fractions of maize products ----

# Checking maize items
nct %>% filter(grepl("maize", item, ignore.case = TRUE)) 

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

# Maize Se national-level (mcg/100g FW) -----
# Need water concentration for conversion from DW (mg/Kg) to FW. 
# From Malawi FCT (from the IH5 NCT)
# Water g/100g and unit conversion (mcg 100g-1 FW) & ratio conversion
# As per FAO/ INFOODS Guidelines for Converting Units, Denominators 
# and Expressions Version 1.0, page 12
names(predmaize.df )

Se_med <- median(predmaize.df$predSe)

# mcg/100g = mcg/g * (100-water (g/100g)) 
Se_med*(100-nct$WATER[nct$code == "101"])
# g/100g = g/g * (100-water (g/100g)) 
(Se_med/1000*(100-nct$WATER[nct$code == "101"]))*1000

code <- c("101",
                    "102",
                    "103",
                    "104",
                    "105",
                    "820")

m101 <-  Se_med*(100-nct$WATER[nct$code == "101"])
m102 <-  Se_med*(100-nct$WATER[nct$code == "102"])*Se_ratio 
m103 <-  Se_med*(100-nct$WATER[nct$code == "103"])*(1-Se_ratio) 
m104 <-  Se_med*(100-nct$WATER[nct$code == "104"])
m105 <-  Se_med*(100-nct$WATER[nct$code == "105"])
m820 <-  Se_med*(100-nct$WATER[nct$code == "820"]) # Retention factor (Se, KE18 = 1)

national_maize <- as.data.frame(cbind(code, c(m101, m102, m103, m104, m105,  m820)))
names(national_maize)[2] <- "Se_mcg_100g"

# Maize Se EA group (mcg/100g FW)  -----

# Need water concentration for conversion from DW (mg/Kg) to FW. 
# From Malawi FCT (from the IH5 NCT)
# Water g/100g and unit conversion (mcg 100g-1 FW) & ratio conversion
# As per FAO/ INFOODS Guidelines for Converting Units, Denominators 
# and Expressions Version 1.0, page 12
maize.df <- cluster %>% 
  mutate(
    maize_101 = Se_median*(100-nct$WATER[nct$code == "101"]), 
    maize_102 = Se_median*(100-nct$WATER[nct$code == "102"])*Se_ratio, 
    maize_103 = Se_median*(100-nct$WATER[nct$code == "103"])*(1-Se_ratio), 
    maize_104 = Se_median*(100-nct$WATER[nct$code == "104"]), 
    maize_105 = Se_median*(100-nct$WATER[nct$code == "105"]), 
    maize_820 = Se_median*(100-nct$WATER[nct$code == "820"]) # Retention factor (Se, KE18 = 1)
    ) %>% 
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
saveRDS(national_maize, here::here("data", "inter-output", "national-maize-se-nct.RDS"))


#Run this to clean the environment
rm(list = ls())


