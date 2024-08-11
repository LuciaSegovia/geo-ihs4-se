

# Loading data
library(survey) # survey design
library(dplyr)
library(ggplot2)
library(ggridges)
library(sf) # spatial data manipulation
library(tmap) # map viz

# Loading the data
## Loading Malawi EA shapefile (generated in geo-spatial/00.cleaning-boundaries.R)
ea <- st_read(here::here( "data", "boundaries", "mwi_admbnda_adm4_nso.shp")) %>% 
  mutate(ADM2_PCODE = gsub("MW", "",ADM2_PCODE ))

# Loading cleaned and reviewed IHS4 (from ihs4_exploration.R)
ihs4 <- readRDS(here::here("data", "inter-output", 
                           "hh_cons_AFE_q75_v.1.0.1.RDS")) %>% 
  rename(g_AFE = "g_afe_replace")

# Loading ea maize se nct (from nct.R)
maize.df <- readRDS(here::here("data", "inter-output", "ea-maize-se-nct.RDS"))

## NCT data (IHS5) (from fct repo)
nct <-   read.csv(here::here("data", "fct_ihs5_v2.2.csv")) %>%   
  # Selecting only variables of interest
  select(1:8, SE, comment) 

head(nct)

# Merging the datasets -----

## Cluster-level NCT for Malawi (maize and products Se) ------
# Merging data sets

ihs4$item_code <-  as.character(ihs4$item_code)

ihs4_nct <- ihs4 %>% select(case_id, HHID, ea_id, item, item_code, region, district, 
                reside, hh_wgt, Date, factor, kg_d, g_AFE) %>% 
                left_join(., maize.df,
                   by = c("item_code" = "food_code", "ea_id" )) # %>% 
#  filter(is.na(Se_mcg_100g)) %>% distinct(item_code) %>% arrange(desc(item_code))

# Missing Se values for "other" and Infant formula 
ihs4_nct %>% left_join(., nct,  by = c("item_code" = "ihs5_foodid")) %>% 
  mutate(Se_mcg_100g = ifelse(is.na(Se_mcg_100g), SE, Se_mcg_100g)) %>% 
    filter(is.na(Se_mcg_100g)) %>% 
    count(item_code, item, region) 

# Maybe worth checking what's under "other (specify)" in the southern region

#  ggplot(aes(g_AFE, as.character(item_code), colour = item)) +
#  geom_boxplot()
  
  
ihs4_nct <- ihs4_nct %>% left_join(., nct,  by = c("item_code" = "ihs5_foodid")) %>% 
    mutate(Se_mcg_100g = ifelse(is.na(Se_mcg_100g), SE, Se_mcg_100g)) %>% 
    filter(!is.na(Se_mcg_100g)) %>% 
  mutate(
    Se_afe = SE*g_AFE/100, 
    Se_afe_ea = Se_mcg_100g*g_AFE/100,
    kcal_afe = ENERC1*g_AFE/100)

ihs4_nct %>% filter(is.na(SE), !is.na(Se_mcg_100g))

hist(ihs4_nct$Se_afe)
hist(ihs4_nct$Se_afe_ea)
plot(ihs4_nct$Se_afe,ihs4_nct$Se_afe_ea )
plot(ihs4_nct$SE,ihs4_nct$Se_mcg_100g )

# Checking value per person and food items: 
hist(ihs4_nct$Se_afe)

length(ihs4_nct$case_id[ihs4_nct$Se_afe>300])

ihs4_nct$item[ihs4_nct$Se_afe>300]
ihs4_nct$g_AFE[ihs4_nct$Se_afe>300]
ihs4_nct$SE[ihs4_nct$Se_afe>300]

mean(ihs4_nct$Se_afe[grepl("maize", ihs4_nct$item, ignore.case = TRUE)])
mean(ihs4_nct$Se_afe_ea[grepl("maize", ihs4_nct$item, ignore.case = TRUE)])

mean(ihs4_nct$SE[grepl("maize", ihs4_nct$item, ignore.case = TRUE)])
mean(ihs4_nct$Se_mcg_100g[grepl("maize", ihs4_nct$item, ignore.case = TRUE)])

# Estimating Se apparent intake ----

# Getting Se/AFE/day 45mcg/day (Allen et al, 2019) (UL 300mcg/day)
names(ihs4_nct)

ihs4_summary <- ihs4_nct %>% 
  group_by(HHID, ea_id, hh_wgt, reside, district, Date, region) %>% 
  summarise(
    apparent_se = sum(Se_afe), 
    apparent_se_ea = sum(Se_afe_ea), 
    apparent_kcal = sum(kcal_afe)) %>% 
  # Getting adequacy Se and Enerc flag
  mutate(  #TO-DO: Change it back to text!!! 
    se.inad = ifelse(apparent_se<45, 1, 0), # Inadequate 1/ adequate 0
    se.ul = ifelse(apparent_se>300, 1, 0), # High 1/ adequate 0
    se.inad_ea = ifelse(apparent_se_ea<45, 1, 0), # Inadequate 1/ adequate 0
    se.ul_ea = ifelse(apparent_se_ea>300, 1, 0), # High 1/ adequate 0
    enerc.low = ifelse(apparent_kcal<1900, "Low", "OK")) %>% 
  filter(apparent_kcal>400 & apparent_kcal <8000)

ihs4_summary$district <- as.character(ihs4_summary$district)

ihs4_summary <- ihs4_summary %>% 
  left_join(., ea[, c("ADM2_PCODE", "ADM2_EN")] %>% st_drop_geometry() %>% 
              distinct(), by = c("district" = "ADM2_PCODE"))

hist(ihs4_summary$apparent_se)
mean(ihs4_summary$apparent_se)
hist(ihs4_summary$apparent_se_ea)
mean(ihs4_summary$apparent_se_ea)

ihs4_summary %>% filter(apparent_se>300) %>%  View()

check <- ihs4_summary %>% filter(apparent_se>300) %>%  pull(HHID)

ihs4_nct %>% filter(HHID %in% check) %>% 
  ggplot(aes(g_AFE, HHID)) + geom_boxplot()

ihs4_nct %>% filter(HHID %in% check) %>% arrange(kcal_afe) %>% View()





ihs4_summary %>% select(HHID, ea_id, reside, apparent_se, apparent_se_ea, hh_wgt) %>% 
  pivot_longer(cols = starts_with("apparent_se"), 
               names_to = "method",
               values_to = "apparent_se"
               ) %>% 
  mutate(method = ifelse(method == "apparent_se", "national", "cluster")) %>% 
  ggplot(aes(apparent_se, method, fill = stat(x)), wt = hh_wgt) +
#  geom_density_ridges()
 geom_density_ridges_gradient( ) +
  scale_fill_viridis_c(option = "C") 
