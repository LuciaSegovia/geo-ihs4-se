



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
