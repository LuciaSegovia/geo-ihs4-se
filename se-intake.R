



# Merging the datasets -----

## Cluster-level NCT for Malawi (maize and products Se) ------
# Merging data sets

ihs4 %>% select(case_id, HHID, ea_id, item, item_code, region, district, 
                reside, hh_wgt, interviewDate, factor, kg_d) %>% 
                left_join(., maize.df,
                   by = c("" = "food_code")) 

%>% 
  mutate(Se_mcg_100g = ifelse(is.na(Se_mcg_100g), SE, Se_mcg_100g)) 

# Estimating Se apparent intake ----
