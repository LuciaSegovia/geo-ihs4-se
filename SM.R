

# Loading the packages -----
library(dplyr) # Data cleaning 
library(tidyr) # Data manipulation
library(ggplot2) # Data viz


# Data -----

ihs4_nct <- readRDS(here::here("data", "inter-output", 
                             "ihs4-intake-ncts-fg.RDS"))

## Supplementary Table 2 ------
# Food list from IHS4
fg_list <-  nct %>% select(code, item) %>% distinct() %>% 
  left_join(., foodgroups)

fg_list %>% 
  write.csv(., here::here("output", "SM", "Suppl.Tab2_food-group-summary_list.csv"), 
            row.names = FALSE)

# Supl. Table 1 -----
# The NCT from fct repo

## Supplementary Table 3 ------

ihs4_nct %>% 
  group_by(case_id, FoodName_1) %>% 
  summarise(total_cons = sum(g_AFE, na.rm = TRUE),
            ener = sum(kcal_afe, na.rm = TRUE), 
            se_ea = sum(Se_afe_ea, na.rm = TRUE), 
            se = sum(Se_afe, na.rm = TRUE),
            seN = sum(Se_afe_N, na.rm = TRUE)) %>% 
  group_by(FoodName_1) %>% 
  summarise(perc = n()/length(unique(ihs4$case_id))*100, 
            mean_cons = mean(total_cons, na.rm = TRUE), 
            sd_cons =sd(total_cons, na.rm = TRUE),
            Q25_cons =quantile(total_cons, c(0.25), na.rm = TRUE),
            median_cons = median(total_cons, na.rm = TRUE), 
            Q75_cons =quantile(total_cons, c(0.75), na.rm = TRUE), 
            #   iqr25_cons =median_cons-IQR(total_cons, na.rm = TRUE),
            #    iqr75_cons =median_cons+IQR(total_cons, na.rm = TRUE),
            mean_ener = mean(ener, na.rm = TRUE), 
            sd_ener =sd(ener, na.rm = TRUE), 
            enerc_per = (mean_ener/2257.252*100),
            median_ener = median(ener, na.rm = TRUE), 
            Q25_ener =quantile(ener, c(0.25), na.rm = TRUE),
            Q75_ener =quantile(ener, c(0.75), na.rm = TRUE), 
            median_Se = median(se, na.rm = TRUE),
            Q25_Se =quantile(se, c(0.25), na.rm = TRUE),
            Q75_Se =quantile(se, c(0.75), na.rm = TRUE),
            median_SeN = median(seN, na.rm = TRUE),
            Q25_SeN =quantile(seN, c(0.25), na.rm = TRUE),
            Q75_SeN =quantile(seN, c(0.75), na.rm = TRUE),
            median_Se_ea = median(se_ea, na.rm = TRUE),
            Q25_Se_ea =quantile(se_ea, c(0.25), na.rm = TRUE),
            Q75_Se_ea =quantile(se_ea, c(0.75), na.rm = TRUE)
            ) %>% arrange(desc(perc)) %>% 
  write.csv(., here::here("output", "SM", "Suppl.Tab3_food-group-summary.csv"), row.names = FALSE)
  # arrange(desc(mean_se_ea)) %>% View()
  # group_by(FoodName_1) %>% 
  # filter(perc >50) %>% View()
  # slice_max(order_by = mean_cons, n = 10) %>% ungroup() %>% 
  # distinct(FoodName_1)






## Testing loop ------

# Saving the food matches to consumption (raw)
ihs4_nct <- readRDS(here::here("data", "inter-output", 
                               "ihs4-intake-ncts-fg.RDS"))


ihs4_nct2 <- ihs4_nct %>% group_by(HHID, ea_id, region, district, 
                                   reside, hh_wgt) %>% 
  summarise(
    maize = sum(FoodName_1 %in% unique(foodgroups$FoodName_1)[1]),
    rice = sum(FoodName_1 %in% unique(foodgroups$FoodName_1)[2]), 
    millet = sum(FoodName_1 %in% unique(foodgroups$FoodName_1)[3]))

test <- as.data.frame(as.matrix(NA))

for(i in 1:length(unique(foodgroups$FoodName_1))){
  print(i)
  
  for(j in 1:nrow(ihs4_nct2)){
    
    n_food <- sum(ihs4_nct$HHID %in% unique(ihs4_nct$HHID[j]) &ihs4_nct$FoodName_1 %in% unique(foodgroups$FoodName_1)[i])
    
    test[j,i] <- n_food
    
    print(j)
  }
  
}

# Adding the HHID    
test[, "HHID"] <- ihs4_nct2$HHID

# Corresponence with
unique(foodgroups$FoodName_1)
names(test)[1:48] 

# Fixing an issue of du
# Saving the dataset
saveRDS(test, here::here("data", "inter-output", "frequency-food-groups.RDS"))


ihs4_nct2 %>% left_join(., test) 

# ba134ca37f1e4262a6a65fe7bd21d6e8
ihs4_nct %>% filter(HHID == "ba134ca37f1e4262a6a65fe7bd21d6e8") %>% 
  select(HHID, ea_id, region, district, reside, hh_wgt)
