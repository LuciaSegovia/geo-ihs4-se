####################################################################
#                                                                  #
#          This script performs sensitivity analysis               #         
#             with the app. consumption IHS4                       #         
#                                                                  # 
#                                                                  #  
#################################################################### 



# Loading the packages -----
library(dplyr) # Data cleaning 
library(tidyr) # Data manipulation
library(ggplot2) # Data viz
library(ggridges) # Data viz:ridges
library(survey) # survey design
library(srvyr) # survey design


# Loading data
# Food matches to consumtion (raw)
ihs4_nct <- readRDS(here::here("data", "inter-output", 
                             "ihs4-intake-ncts.RDS"))

## Loading Malawi EA shapefile & district names (generated in geo-spatial/00.cleaning-boundaries.R)
ea <- st_read(here::here( "data", "boundaries", "mwi_admbnda_adm4_nso.shp")) %>% 
  mutate(ADM2_PCODE = gsub("MW", "",ADM2_PCODE ))
ea$region <- as.character(ea$region)


# Upper limit for energy
ul <- 5000
#lower limit for energy
ll <- 400

# Estimating Se apparent intake ----
# Getting Se/AFE/day 45mcg/day (Allen et al, 2019) (UL 300mcg/day)

# Summary of app. intakes & adequacies
ihs4_summary <- ihs4_nct %>% 
  group_by(HHID, ea_id, hh_wgt, reside, district, Date, region) %>% 
  summarise(
    apparent_se = sum(Se_afe), 
    apparent_se_N = sum(Se_afe_N), 
    apparent_se_ea = sum(Se_afe_ea), 
    apparent_kcal = sum(kcal_afe)) %>% 
  # Getting adequacy Se and Enerc flag
  mutate(  #TO-DO: Change it back to text!!! 
    se.inad = ifelse(apparent_se<45, 1, 0), # Inadequate 1/ adequate 0
    se.ul = ifelse(apparent_se>300, 1, 0), # High 1/ adequate 0
    se.inad_N = ifelse(apparent_se_N<45, 1, 0), # Inadequate 1/ adequate 0
    se.ul_N = ifelse(apparent_se_N>300, 1, 0), # High 1/ adequate 0
    se.inad_ea = ifelse(apparent_se_ea<45, 1, 0), # Inadequate 1/ adequate 0
    se.ul_ea = ifelse(apparent_se_ea>300, 1, 0), # High 1/ adequate 0
    enerc.low = ifelse(apparent_kcal<1900, "Low", "OK")) %>%
  # Excluding implausible low and high energy intake.
  filter(apparent_kcal>ll & apparent_kcal <ul)

names(ihs4_summary)

ihs4_summary$region <- as.character(ihs4_summary$region)

ihs4_summary <- ihs4_summary %>% 
  left_join(.,ea %>% st_drop_geometry() %>% 
              select(ADM2_PCODE,ADM2_EN, region) %>% distinct(),
            by=c("district" = "ADM2_PCODE", "region"))

sum(is.na(ihs4_summary$ADM2_EN))

# Applying Survey weights & strata -----
# PSU = ea_id
ihs4_summary$region <- as.factor(ihs4_summary$region)
ihs4_design2 <- ihs4_summary %>% ungroup() %>% 
  as_survey_design(strata = c(district, reside), weights = hh_wgt)


## App. intakes -----------
var <- "ADM2_EN"
ihs4_design2 %>% 
  #group_by(region) %>% 
#  group_by(!!sym(var)) %>% 
  summarise(across(starts_with("apparent_se"),
                   ~srvyr::survey_quantile(.x, c(0.25, 0.5, 0.75)))) %>% 
  select(-ends_with("se")) %>% 
  pivot_longer(cols = starts_with("apparent_"), 
               names_to = "method", 
               values_to = "Quartile") %>% 
  #pivot_wider(names_from = region, 
  pivot_wider(names_from = !!sym(var), 
              values_from = Quartile,
              #names_prefix = "region")
              names_prefix = var) 


# App. inadequacy ----
# Applying Survey weights & strata -----
# PSU = ea_id
ihs4_design<-svydesign(id=~ea_id, 
                       weights=~hh_wgt, strata = ~district+reside,
                       data=ihs4_summary)
# Global prop
svyciprop(~se.inad,ihs4_design,  method = "mean" , level = 0.95)
#svymean(~se.inad, ihs4_design)
svyciprop(~se.inad_N,ihs4_design,  method = "mean" , level = 0.95)
#svymean(~se.inad_N, ihs4_design)
svyciprop(~se.inad_ea,ihs4_design,  method = "mean" , level = 0.95)
#svymean(~se.inad_ea, ihs4_design)

