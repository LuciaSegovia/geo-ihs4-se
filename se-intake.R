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

## Loading Malawi EA shapefile & district names (generated in geo-spatial/00.cleaning-boundaries.R)
ea <- st_read(here::here( "data", "boundaries", "mwi_admbnda_adm4_nso.shp")) %>% 
  mutate(ADM2_PCODE = gsub("MW", "",ADM2_PCODE ))

# Consumption data (ihs4)
# Loading cleaned and reviewed IHS4 (from ihs4_exploration.R)
ihs4 <- readRDS(here::here("data", "inter-output", 
                           "hh_cons_AFE_q75_v.1.0.1.RDS")) %>% 
  rename(g_AFE = "g_afe_replace")

# NCT (ihs4) (from fct repo)
nct <-   read.csv(here::here("data", "nct", "ihs4_nct_SEmcg_v1.0.0.csv")) %>%
  # Excluding 118 not present in ihs4
  filter(code != "118")

# Maize Se ----
# From the nct.R
# EA group (disaggregated)
maize.df <- readRDS(here::here("data", "inter-output", "ea-maize-se-nct.RDS"))
# National (single value)
national_maize <- readRDS( here::here("data", "inter-output",
                                      "national-maize-se-nct.RDS")) %>% 
  rename(Se_mcg_100gN = "Se_mcg_100g") %>% mutate(Se_mcg_100gN = as.numeric(Se_mcg_100gN))


# Merging the datasets -----

## Cluster-level NCT for Malawi (maize and products Se) ------
# Merging data sets

ihs4$item_code <-  as.character(ihs4$item_code)

ihs4$g_AFE[ihs4$item_code == "835"]
names(ihs4)

ihs4_nct <- ihs4 %>% select(case_id, HHID, ea_id, item, item_code, region, district, 
                reside, hh_wgt, Date, factor, kg_d, g_AFE) %>% 
  # Adding maize
  left_join(., national_maize,
            by = c("item_code" = "code" )) %>% 
  # Adding EA group maize
                left_join(., maize.df,
                   by = c("item_code" = "food_code", "ea_id" )) 
#  filter(is.na(Se_mcg_100g)) %>% distinct(item_code) %>% arrange(desc(item_code))

ihs4_nct$district <- as.character(ihs4_nct$district)

ihs4_nct$g_AFE[ihs4_nct$item_code == "835"]
ihs4_nct$item_code[ihs4_nct$item_code == "835"]
ihs4_nct$Se_mcg_100g[ihs4_nct$item_code == "835"]
nct$SEmcg[nct$code == "835"]

nct$SEmcg[nct$code == "835"]
nct$code[nct$code == "414"]

# Missing Se values for "other" and Infant formula 
ihs4_nct %>% left_join(., nct %>% select(-item),  by = c("item_code" = "code")) %>% 
  mutate(
    Se_mcg_100gN = ifelse(is.na(Se_mcg_100gN), SEmcg, Se_mcg_100gN),
    Se_mcg_100g = ifelse(is.na(Se_mcg_100g), SEmcg, Se_mcg_100g)) %>% 
    filter(is.na(Se_mcg_100g)) %>% 
    count(item_code, item) %>% View()

# Maybe worth checking what's under "other (specify)" in the southern region

#  ggplot(aes(g_AFE, as.character(item_code), colour = item)) +
#  geom_boxplot()
  
# Completing the Se value for EA-group and National maize  
ihs4_nct <- ihs4_nct %>% # Here we removed item bc it was excluding items
  left_join(., nct%>% select(-item),  by = c("item_code" = "code")) %>% 
    mutate(
      Se_mcg_100gN = as.numeric(ifelse(is.na(Se_mcg_100gN), SEmcg, Se_mcg_100gN)),
      Se_mcg_100g = ifelse(is.na(Se_mcg_100g), SEmcg, Se_mcg_100g)) %>% 
    filter(!is.na(Se_mcg_100g))

# Calculating the Se supply per food consumed in (mcg/AFE/day)
ihs4_nct <- ihs4_nct %>% 
  mutate(
    Se_afe = SEmcg*g_AFE/100, 
    Se_afe_ea = Se_mcg_100g*g_AFE/100,
    Se_afe_N = Se_mcg_100gN*g_AFE/100,
    kcal_afe = ENERCkcal*g_AFE/100)

ihs4_nct$g_AFE[ihs4_nct$item_code == "835"]

# Saving the food matches to consumption (raw)
saveRDS(ihs4_nct, here::here("data", "inter-output", 
                             "ihs4-intake-ncts.RDS"))

#ihs4_nct %>% filter(is.na(SEmcg), !is.na(Se_mcg_100g))
#
#hist(ihs4_nct$Se_afe)
#hist(ihs4_nct$Se_afe_ea)
#plot(ihs4_nct$Se_afe,ihs4_nct$Se_afe_ea )
#plot(ihs4_nct$SE,ihs4_nct$Se_mcg_100g )
#
## Checking value per person and food items: 
#hist(ihs4_nct$Se_afe)
#
#length(ihs4_nct$case_id[ihs4_nct$Se_afe>300])

#ihs4_nct$item[ihs4_nct$Se_afe>300]
#ihs4_nct$g_AFE[ihs4_nct$Se_afe>300]
#ihs4_nct$SE[ihs4_nct$Se_afe>300]
#
#mean(ihs4_nct$Se_afe[grepl("maize", ihs4_nct$item, ignore.case = TRUE)])
#mean(ihs4_nct$Se_afe_ea[grepl("maize", ihs4_nct$item, ignore.case = TRUE)])
#
#mean(ihs4_nct$SE[grepl("maize", ihs4_nct$item, ignore.case = TRUE)])
#mean(ihs4_nct$Se_mcg_100g[grepl("maize", ihs4_nct$item, ignore.case = TRUE)])
#
# Estimating Se apparent intake ----

# Getting Se/AFE/day 45mcg/day (Allen et al, 2019) (UL 300mcg/day)
names(ihs4_nct)

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
  filter(apparent_kcal>400 & apparent_kcal <9000)

# Checking low energy
sum(ihs4_summary$apparent_kcal<400)
sum(ihs4_summary$apparent_kcal>9000)

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

## Saving IHS4 food consumption & intakes per HHs----

saveRDS(ihs4_summary, here::here("data", "inter-output", "ihs4-hh-intakes.RDS"))


## Ridges graph ----

ihs4_summary %>% select(HHID, ea_id, reside, region, apparent_se, apparent_se_ea, 
                        apparent_se_N, hh_wgt) %>% 
#  filter(apparent_se_ea<300) %>% 
  pivot_longer(cols = starts_with("apparent_se"), 
               names_to = "method",
               values_to = "apparent_se"
               ) %>% 
  mutate(method = case_when(
    method == "apparent_se" ~ "FCT",
    method == "apparent_se_ea" ~ "EA Group",
    method == "apparent_se_N" ~ "National")) %>% 
  ggplot(aes(apparent_se, method, fill = stat(x)), wt = hh_wgt) +
#  geom_density_ridges()
 geom_density_ridges_gradient( ) +
  scale_fill_viridis_c(option = "C")  +
  facet_wrap(~region, scales = "free_x")


## Food results -----

## Getting average by food group

fg <- read.csv(here::here("data", "ihs4", 
                          "ihs4_dictionary_foodgroup1.csv" )) %>% 
  mutate(code = as.character(code))  %>% 
  select(code, FoodName_1) %>% distinct() 

fg$code[!duplicated(fg$code)]

foodgroups <- fg[which(!duplicated(fg$code)),]

foodgroups$FoodName_1[grep("fish", foodgroups$FoodName_1)] <- "fish and products"
foodgroups$FoodName_1[grep("sunflowerseed", foodgroups$FoodName_1)] <- "oils and products"
foodgroups$FoodName_1[foodgroups$code == "105"] <- "maize and products (including white maize)"
foodgroups$FoodName_1[foodgroups$code == "820"] <- "maize and products (including white maize)"


# Checking dupli

 value <- fg %>% count(code) %>% arrange(desc(n)) %>% filter(n>1) %>% pull(code)

fg %>% filter(code %in% value)
ihs4_nct %>% filter(item_code %in% value) %>% select(item_code, item) %>% distinct()

names(fg)

sum(ihs4_nct$g_AFE==0)

ihs4_nct <- ihs4_nct %>% left_join(., foodgroups,
                   by = c("item_code" = "code")) %>% 
  # We are removing consumed zero bc it would lower the "portions" 
  # And it would increase the frequency 
  filter(!is.na(FoodName_1) & g_AFE >0)


ihs4_nct$g_AFE[ihs4_nct$item_code == "835"]

ihs4_nct$FoodName_1[ihs4_nct$item_code == "105"] 

ihs4_nct %>%
  group_by(item_code, item) %>% 
  summarise(perc = n()/length(unique(ihs4$case_id))*100 , 
            mean_cons = mean(g_AFE, na.rm = TRUE), 
            sd_cons =sd(g_AFE, na.rm = TRUE),
            mean_ener = mean(kcal_afe, na.rm = TRUE), 
            sd_ener =sd(kcal_afe, na.rm = TRUE), 
            Q25 =quantile(kcal_afe, c(0.25), na.rm = TRUE),
            median = median(kcal_afe, na.rm = TRUE), 
            Q75 =quantile(kcal_afe, c(0.75), na.rm = TRUE), 
            mean_se_ea = mean(Se_afe_ea, na.rm = TRUE), 
            sd_se_ea =sd(Se_afe_ea, na.rm = TRUE),
            mean_se = mean(Se_afe_ea, na.rm = TRUE), 
            sd_se =sd(Se_afe_ea, na.rm = TRUE)) %>%
  arrange(desc(perc)) %>% View()

ihs4_nct %>% 
group_by(case_id, FoodName_1) %>% 
  summarise(total_cons = sum(g_AFE, na.rm = TRUE),
            ener = sum(kcal_afe, na.rm = TRUE), 
            se_ea = sum(Se_afe_ea, na.rm = TRUE), 
            se = sum(Se_afe, na.rm = TRUE)) %>% 
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
            enerc_per = (mean_ener/2362.6*100),
            Q25_ener =quantile(ener, c(0.25), na.rm = TRUE),
            median_ener = median(ener, na.rm = TRUE), 
            Q75_ener =quantile(ener, c(0.75), na.rm = TRUE), 
            mean_se_ea = mean(se_ea, na.rm = TRUE), 
            sd_se_ea =sd(se_ea, na.rm = TRUE),
            mean_se = mean(se, na.rm = TRUE), 
            sd_se =sd(se, na.rm = TRUE)) %>%
 # arrange(desc(mean_se_ea)) %>% View()
# group_by(FoodName_1) %>% 
  filter(perc >50) %>% View()
  slice_max(order_by = mean_cons, n = 10) %>% ungroup() %>% 
  distinct(FoodName_1)

# Consumption of maize and products by variable -----
food <- c("maize and products (including white maize)")
vari <- c("region")

ihs4_nct %>% # Getting district names
  left_join(., ea[, c("ADM2_PCODE", "ADM2_EN")] %>% st_drop_geometry() %>% 
              distinct(), by = c("district" = "ADM2_PCODE")) %>% 
  #filtering the food 
   filter(FoodName_1 %in% food) %>% 
  group_by(case_id, FoodName_1, !!sym(vari)) %>% 
  summarise(total_cons = sum(g_AFE, na.rm = TRUE),
            ener = sum(kcal_afe, na.rm = TRUE), 
            se_ea = sum(Se_afe_ea, na.rm = TRUE), 
            se = sum(Se_afe, na.rm = TRUE)
  ) %>%  
  group_by(FoodName_1, !!sym(vari)) %>% 
  summarise( 
    mean_cons = mean(total_cons, na.rm = TRUE), 
    sd_cons =sd(total_cons, na.rm = TRUE),
    Q25_cons =quantile(total_cons, c(0.25), na.rm = TRUE),
    median_cons = median(total_cons, na.rm = TRUE), 
    Q75_cons =quantile(total_cons, c(0.75), na.rm = TRUE), 
    mean_ener = mean(ener, na.rm = TRUE), 
    sd_ener =sd(ener, na.rm = TRUE), 
    mean_se_ea = mean(se_ea, na.rm = TRUE), 
    sd_se_ea =sd(se_ea, na.rm = TRUE), 
    mean_se = mean(se, na.rm = TRUE), 
    sd_se_ea =sd(se, na.rm = TRUE)
  ) %>% View()

ihs4_food <- ihs4_nct %>% # Getting district names
  left_join(., ea[, c("ADM2_PCODE", "ADM2_EN")] %>% st_drop_geometry() %>% 
              distinct(), by = c("district" = "ADM2_PCODE")) %>% 
  #filtering the food 
 # filter(FoodName_1 %in% food) %>% 
  group_by(case_id, FoodName_1, !!sym(vari)) %>% 
  summarise(total_cons = sum(g_AFE, na.rm = TRUE),
            ener = sum(kcal_afe, na.rm = TRUE), 
            se_ea = sum(Se_afe_ea, na.rm = TRUE), 
             se = sum(Se_afe, na.rm = TRUE)
          )

ihs4_food %>% 
  group_by(FoodName_1, !!sym(vari)) %>% 
  summarise( 
            mean_cons = mean(total_cons, na.rm = TRUE), 
            sd_cons =sd(total_cons, na.rm = TRUE),
            median_cons = median(total_cons, na.rm = TRUE), 
            iqr_cons =IQR(total_cons, na.rm = TRUE),
            mean_ener = mean(ener, na.rm = TRUE), 
            sd_ener =sd(ener, na.rm = TRUE), 
            mean_se_ea = mean(se_ea, na.rm = TRUE), 
            sd_se_ea =sd(se_ea, na.rm = TRUE), 
            mean_se = mean(se, na.rm = TRUE), 
            sd_se_ea =sd(se, na.rm = TRUE)
            ) 

# Considerable differences in top food groups supplying Se
# depending on the NCT used. *Not surprising*
ihs4_food %>% 
      group_by(!!sym(vari)) %>% 
     slice_max(order_by = mean_se_ea, n = 5) %>% ungroup() %>% 
    distinct(FoodName_1)

# Consumption of food groups for specific district
distr <- c("Nkhotakota")
vari <- c("ADM2_EN")

ihs4_nct %>% # Getting district names
  left_join(., ea[, c("ADM2_PCODE", "ADM2_EN")] %>% st_drop_geometry() %>% 
              distinct(), by = c("district" = "ADM2_PCODE")) %>% 
  #filtering the district(s) 
  filter(ADM2_EN %in% distr) %>% 
  group_by(case_id, FoodName_1, ADM2_EN) %>% 
  summarise(total_cons = sum(g_AFE, na.rm = TRUE),
            ener = sum(kcal_afe, na.rm = TRUE), 
            # se_ea = sum(Se_afe_ea, na.rm = TRUE), 
            #  se = sum(Se_afe, na.rm = TRUE)
  ) %>% 
  group_by(FoodName_1, !!sym(vari)) %>% 
  summarise( 
    mean_cons = mean(total_cons, na.rm = TRUE), 
    sd_cons =sd(total_cons, na.rm = TRUE),
    mean_ener = mean(ener, na.rm = TRUE), 
    sd_ener =sd(ener, na.rm = TRUE)) %>% 
  View()


# Check distribution

ihs4_nct %>% # Getting district names
  left_join(., ea[, c("ADM2_PCODE", "ADM2_EN")] %>% st_drop_geometry() %>% 
              distinct(), by = c("district" = "ADM2_PCODE")) %>% 
  #filtering the food 
 # filter(FoodName_1 %in% food) %>% 
  group_by(case_id, FoodName_1, !!sym(vari)) %>% 
  summarise(total_cons = sum(g_AFE, na.rm = TRUE),
            ener = sum(kcal_afe, na.rm = TRUE), 
            se_ea = sum(Se_afe_ea, na.rm = TRUE), 
            se = sum(Se_afe, na.rm = TRUE)
  ) %>%  
  ggplot(aes(total_cons, FoodName_1)) +
  geom_density_ridges() +
 #geom_density_ridges_gradient( ) +
  scale_fill_viridis_c(option = "C") 


# Checking overall foods for lean season ----

## Checking representativeness of the months
ihs4_nct %>%
#  filter(Date > "2016-09-30" & Date < "2017-03-01") %>% 
  group_by(reside) %>% 
  distinct(case_id) %>% count(reside)/length(unique(ihs4$case_id))


food <- c("maize and products (including white maize)")

maize1 <- ihs4_nct %>%   #filter(Date > "2016-09-30" & Date < "2017-03-01") %>%
  filter(FoodName_1 %in% food) %>% 
  group_by(item_code, item, reside) %>%
  summarise(
            Q25_qty =quantile(g_AFE, c(0.25), na.rm = TRUE),
            med_qty = median(g_AFE, na.rm = TRUE), 
            Q75_qty =quantile(g_AFE, c(0.75), na.rm = TRUE), 
            Q25_Se_ea =quantile(Se_afe_ea, c(0.25), na.rm = TRUE),
            med_Se_ea = median(Se_afe_ea, na.rm = TRUE), 
            Q75_Se_ea =quantile(Se_afe_ea, c(0.75), na.rm = TRUE)) %>%
  arrange(desc(med_Se_ea)) #%>% View()

maize <- left_join(maize1, maize2, by=c("item_code", "item", "reside")) %>% 
  mutate(
    diff_qty = med_qty.x - med_qty.y, 
    diff_Se = med_Se_ea.x - med_Se_ea.y) %>% 
  select(1:3, diff_qty, diff_Se) 


# Checking overall foods for lean season ----
months <- ihs4_nct %>%
  filter(Date > "2016-09-30" & Date < "2017-03-01") %>%
  group_by(item_code, item, region) %>%
  summarise(
    Q25_qty =quantile(g_AFE, c(0.25), na.rm = TRUE),
    med_qty = median(g_AFE, na.rm = TRUE), 
    Q75_qty =quantile(g_AFE, c(0.75), na.rm = TRUE), 
    Q25_Se_ea =quantile(Se_afe_ea, c(0.25), na.rm = TRUE),
    med_Se_ea = median(Se_afe_ea, na.rm = TRUE), 
    Q75_Se_ea =quantile(Se_afe_ea, c(0.75), na.rm = TRUE)) %>%
  arrange(desc(med_Se_ea)) #%>% View()

difference <- left_join(year, months, by=c("item_code", "item", "region")) %>% 
  mutate(
    diff_qty = med_qty.x - med_qty.y, 
    diff_Se = med_Se_ea.x - med_Se_ea.y) %>% 
  select(1:3, diff_qty, diff_Se)  
