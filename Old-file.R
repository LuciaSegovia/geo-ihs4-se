

# Loading data
library(survey) # survey design
library(srvyr) # survey design
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
ihs4 <- readRDS(here::here("data", "inter-output", "hh_cons_AFE_q75_v.1.0.1.RDS")) %>% 
  dplyr::rename(g_AFE = "g_afe_replace")

length(unique(ihs4$HHID))
length(unique(ihs4$item_code))

ihs4 %>% count(item_code) %>% arrange(desc(n))

ihs4$g_AFE[ihs4$item_code == "835"]

# Loading ea maize se nct (from nct.R)
maize.df <- readRDS(here::here("data", "inter-output", "ea-maize-se-nct.RDS"))

## NCT data (IHS5) (from fct repo)
nct <-   read.csv(here::here("data", "fct_ihs5_v2.2.csv")) %>%   
  # Selecting only variables of interest
  select(1:5, WATER, ENERC1, SE, comment) %>% rename(
    code = "ihs5_foodid", 
    item = "ihs5_fooditem", 
    source_fct = "ref_source", 
    fdc_id = "ref_fctcode", 
    #   food_desc = "ref_fctitem", 
    WATERg = "WATER", 
    ENERCkcal = "ENERC1", 
    SEmcg = "SE", 
    comments = "comment"
  ) %>% mutate(ID_3 = NA)

head(nct)
sum(duplicated(nct$code))

# Partially reviewed NCT
nct1 <- read.csv(here::here("data",  "inter-output",
                            "ihs4-partial-nct_v1.0.0.csv")) 
nct1$code <- as.character(nct1$code)

names(nct1)
sum(duplicated(nct1$code))

nct1 <- nct1 %>% group_by(code, item) %>% 
  summarise(across(is.numeric, mean),
            source_fct = paste0(source_fct, collapse = "; "), 
            fdc_id = paste0(fdc_id, collapse = "; "),
            ID_3 = paste0(ID_3, collapse = "; ")) 

# Function to fill the Energy missing values from the other NCT
test_fn <- function(df, df2, tag = "ENERCkcal", code = "code", comments = "comments"){
  
  if(sum(names(df) == "comments")==0){
    df$comments <- NA
  }
  
  for(i in 1:nrow(df)){
    
    if(is.na(df[i, c("ENERCkcal")])){
      
      test <-  df[i, c("code")][[1]]
      print(test)
      df[i, c("ENERCkcal")] <- df2$ENERCkcal[nct$code == test]
      df[i, c("comments")] <- df2$fdc_id[nct$code == test]
    }
  }
  return(df)
}

nct1 <- test_fn(nct1, nct, "ENERCkcal", "code", "comments")

nct <- nct %>% filter(!code %in% unique(nct1$code)) %>% 
  bind_rows(., nct1) %>% 
  # Only missing values for baby milk (not consumed by WRA & mucuna not reported in IHS4)
  filter(!is.na(SEmcg)) 

names(nct)
