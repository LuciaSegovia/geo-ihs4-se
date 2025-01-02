# Envir prep -----
library(survey) # survey design
library(srvyr) # survey design
library(sf) # spatial data manipulation
library(tmap) # map viz
library(dplyr) #data manipulation
library(tidyr) #data transformation
library(ggplot2) #data viz

# Loading data -----

# NCT data w/ he food matches to consumption (raw)
nct <-   read.csv(here::here("data", "nct", "ihs4_nct_SEmcg_v1.0.1.csv")) %>%
  # Excluding 118 not present in ihs4
  filter(!code %in% c("118", "831b", "832b", "204b"))

# From the nct.R
# EA group (disaggregated)
maize.df <- readRDS(here::here("data", "inter-output", "ea-maize-se-nct.RDS"))

# Filtering only maize values:
maize_codes <- c(101, 102, 103, 104, 105, 820)

## Table 1 ---------------------

nct %>% filter(code %in% maize_codes)

nct %>% count(source_fct)
sum((53+4+2+1+2+4+1+1)/nrow(nct), #KE18(
(8+1+1+27+1+4)/nrow(nct), #MW19
(1)/nrow(nct), #LS06
(6)/nrow(nct), #UK21
(11+1)/nrow(nct)) #US19

## Table 2 --------------------

# Filtering only maize values:
maize_codes <- c(101, 102, 103, 104, 105, 820)


# Fig. 1 ---------------------
# Boxplot
test <- nct %>% filter(code %in% maize_codes) %>% 
  select(code,item, SEmcg) %>% 
  mutate(y1 = SEmcg, 
         #  y = row.names()+.3, 
         y2 = SEmcg, 
         #   yend =  nrow()-.3
  ) %>% arrange(code)

test$x1 <- seq(1:6)+.3  
test$x2 <- seq(1:6)-.3  
test$legend <- paste0(test$code, " (", round(test$SEmcg,2),  ")")
test$legend2 <- paste0(test$item, " (", round(test$SEmcg,2),  ")")

title <-expression(paste(
  "Se content (",   mu, "g/", "100g FW EP)", sep=""))

legend_title <- expression(paste(
  "Baseline Se values (",   mu, "g/100g FW EP)", sep=""))

maize.df %>%  left_join(., nct[, c("code", "item", "SEmcg")], 
                        by =c("food_code"= "code")) %>%
  mutate(food_desc =  paste0(item, " (", food_code,  ")")) %>% 
  #  ggplot(aes(reorder(food_desc, as.numeric(food_code)), Se_mcg_100g)) +
  ggplot(aes(reorder(item, as.numeric(food_code)), Se_mcg_100g)) +
  #  ggplot(aes(food_code, Se_mcg_100g)) + 
  geom_boxplot() + 
  coord_flip() +
  # Draw line segment
  # geom_segment(x = 6.3,
  #              y = 3.81,
  #              xend = 5.7,
  #              yend = 3.81, colour = "blue")
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, 
                   colour = legend2), data = test, 
               size = 1.5) + 
  xlab("") +
  ylab(title) +
  labs(color = legend_title) +
  theme_bw() +
  theme(
    #strip.text.x = element_blank(),
    #   strip.background = element_rect( fill="white"),
    #   legend.position=c(.75,.82),
    text = element_text(size = 20))



# Apparent intakes (Table 3) ---------------------

ihs4_summary <- readRDS(here::here("data", "inter-output", "ihs4-hh-intakes.RDS"))
names(ihs4_summary)

ihs4_summary$region <- as.factor(ihs4_summary$region)

ihs4_design2 <- ihs4_summary %>% ungroup() %>% 
  mutate(region = factor(region, levels = c(1, 2, 3), 
                         labels = c("Northern", "Central", "Southern")),
         reside = factor(reside, levels = c(1, 2), 
                         labels = c("Urban", "Rural"))) %>% 
  as_survey_design(strata = c(district, reside), weights = hh_wgt)

## National-level Se & energy estimates -----
ihs4_design2 %>% 
  summarise(across(starts_with("apparent_"),
                   ~srvyr::survey_quantile(.x, c(0.25, 0.5, 0.75)))) %>% 
  select(-ends_with("se")) %>% 
  pivot_longer(cols = starts_with("apparent_"), 
               names_to = "method", 
               values_to = "Quartile") %>% View()

### Table 3 ---------------------
# Apparent intkaes by residency, region and district
# Manually saving one, two, three
var <- c("reside","region",  "ADM2_EN")
#table3 <- list()
# Manually changing 1:3
i =2
# Manually saving 
# one, two, three
two <- ihs4_design2 %>% 
  #group_by(region) %>% 
  group_by(!!sym(var[i])) %>% 
  summarise(across(starts_with("apparent_"),
                   ~srvyr::survey_quantile(.x, c(0.25, 0.5, 0.75)))) %>% 
  select(-ends_with("se")) %>% 
  pivot_longer(cols = starts_with("apparent_"), 
               names_to = "method", 
               values_to = "Quartile") %>% 
  pivot_wider(names_from = method, 
              values_from = Quartile
  ) %>% 
  rename(variable = var[i]) 

names(one)

table3 <- rbind(one, two, three) %>% 
  dplyr::select("variable", "apparent_kcal_q50", "apparent_kcal_q25",  "apparent_kcal_q75", 
                "apparent_se_q50","apparent_se_q25", "apparent_se_q75" ,  
                "apparent_se_N_q50" , "apparent_se_N_q25" , "apparent_se_N_q75", 
                "apparent_se_ea_q50",   "apparent_se_ea_q25",  "apparent_se_ea_q75")

table3 <- table3 %>% 
  mutate(across(starts_with("apparent_kcal"), ~round(.))) %>% 
  mutate(apparent_kcal_iqr = paste0("(", apparent_kcal_q25, "-", apparent_kcal_q75, ")")) %>% 
  mutate(across(starts_with("apparent_se"), ~round(., 2))) %>% 
  mutate(apparent_se_iqr = paste0("(", apparent_se_q25, "-", apparent_se_q75, ")")) %>% 
  mutate(apparent_se_N_iqr = paste0("(", apparent_se_N_q25, "-", apparent_se_N_q75, ")")) %>% 
  mutate(apparent_se_ea_iqr = paste0("(", apparent_se_ea_q25, "-", apparent_se_ea_q75, ")")) %>% 
  dplyr::select("variable", "apparent_kcal_q50", "apparent_kcal_iqr",
                "apparent_se_q50","apparent_se_iqr",   
                "apparent_se_N_q50" , "apparent_se_N_iqr" , 
                "apparent_se_ea_q50",   "apparent_se_ea_iqr") 

# Saving Table 3 -----
write.csv(table3 , here::here("output", "median-apparent-intakes_v4.0.1.csv"), 
          row.names = FALSE)


# Figure 2 prep. ----
## Loading Malawi EA shapefile & district names (generated in geo-spatial/00.cleaning-boundaries.R)
ea <- st_read(here::here( "data", "boundaries", "mwi_admbnda_adm4_nso.shp")) %>% 
  mutate(ADM2_PCODE = gsub("MW", "",ADM2_PCODE ))

# Loading IHS4 intkes
# ihs4_summary <-  readRDS(here::here("data", "inter-output", "ihs4-hh-intakes.RDS"))
#sum(is.na(ihs4_summary$ea_id))
## Median using srvyr package ----
# PSU = ea_id

#ihs4_summary$region <- as.factor(ihs4_summary$region)
#ihs4_design2 <- ihs4_summary %>% ungroup() %>% 
#  as_survey_design(strata = c(district, reside), weights = hh_wgt)

## Fig. 2: difference intakes by region & residency ----
# Loading the data

pal_base <- c("#ffa600","#003f5c", "#35D0BA" )
n <- length(unique(three$variable)) - 1

title <-expression(paste(
  "Apparent Se intake (",   mu, "g/", "AFE/day)", sep=""))

ihs4_design2 %>% 
  group_by(region, reside) %>% 
  summarise(across(starts_with("apparent_se"),
                   ~srvyr::survey_quantile(.x, c(0.5)))) %>% 
  select(-ends_with("se")) %>% 
  rowwise() %>% 
  mutate(mymedian = median(c(apparent_se_q50, apparent_se_N_q50, apparent_se_ea_q50))) %>% 
  arrange(mymedian) %>% 
  mutate(reside = factor(reside, reside)) %>% 
  pivot_longer(cols = starts_with("apparent_se"),
               names_to = "method", 
               values_to = "app_Se") %>% 
  mutate(Scenario = case_when(
    method == "apparent_se_q50" ~ "Baseline NCT", 
    method == "apparent_se_ea_q50" ~ "Spatially-resolved maize NCT", 
    method == "apparent_se_N_q50" ~ "National maize NCT")) %>% 
#  mutate(region = factor(region, levels = c(1, 2, 3), 
#                          labels = c("Northern", "Central", "Southern")), 
       #  reside = factor(reside, levels = c(1, 2), 
       #                  labels = c("Urban", "Rural"))) %>% 
  ggplot(aes(x = app_Se, y = reside )) +
  stat_summary(
    geom = "linerange", fun.min = "min", fun.max = "max", 
    linewidth = 0.8, color = "black")+
  ## white point to overplot line endings
  geom_point(
    aes(x = app_Se), size = 5,  stroke = 1, color = "white", fill = "white"
  ) +
  geom_point(
    aes(x = app_Se, colour = Scenario), size = 5, alpha =.6 ,stroke = 1) +
  scale_colour_manual(values = pal_base) +
  xlab(title) +
  ylab("") +
  facet_wrap(~ region) +
  theme_bw() +
  theme(
    legend.position = "top", 
    text = element_text(size = 20)) 

# Loading data on Malawi lake boundaries
lakes <-  st_read(here::here("data",
                             "boundaries", "EN_NSO" , "eas_bnd.shp")) %>% 
  dplyr::filter(grepl("Lake", DISTRICT))

# Combining the three geom for lake Chilwa into one
lakes[2, "geometry"] <- st_union(lakes[2:4,]) %>% st_as_sf()

# Saving the new file
mwi_lakes <- lakes[c(1:2,5),]


## Figure 3: Maps of intake -----
# Palettes from 
#https://r-charts.com/color-palettes/#continuous
var <- "ADM2_EN"
#apparent_se_q50, apparent_se_N_q50, apparent_se_ea_q50 
map3 <- ihs4_design2 %>% 
  #Grouping by District
    group_by(!!sym(var)) %>% 
  summarise(across(starts_with("apparent_se"),
                   ~srvyr::survey_quantile(.x, c(0.25, 0.5, 0.75)))) %>% 
  select(-ends_with("se")) %>% 
  left_join(., ea,  by = c("ADM2_EN"= "ADM2_EN")) %>% st_as_sf() %>% 
  tm_shape() +
  tm_fill( 
   "apparent_se_ea_q50",  # This var. to be changed
 fill.scale = tm_scale_continuous(ticks = c(20, 35, 45, 80, 100)),
 fill.legend = tm_legend(show = FALSE)) +
  # Adding the lakes 
  tm_shape(mwi_lakes)+
  tm_borders(colour ="lightgrey", col_alpha = 0.2)

title_legend <- expression(paste(
  "Apparent Se intake (",
  mu, "g/", "AFE/day)", sep=""))

title_legend1 <- expression(paste(
  "Apparent Se intake\n",
   "(", mu, "g/", "AFE/day)", sep=""))

legend.map2 <- ihs4_design2 %>% 
  #group_by(region) %>% 
  group_by(!!sym(var)) %>% 
  summarise(across(starts_with("apparent_se"),
                   ~srvyr::survey_quantile(.x, c(0.25, 0.5, 0.75)))) %>% 
  select(-ends_with("se")) %>% 
  left_join(., ea,  by = c("ADM2_EN"= "ADM2_EN")) %>% st_as_sf() %>% 
  tm_shape() +
  #   tm_polygons(
  tm_fill( 
    "apparent_se_ea_q50", 
    # style = "cont", # changed fixed to continous
    #    breaks = c(20, 35, 45, 80, 100), 
    fill.scale = tm_scale_continuous(ticks = c(20, 35, 45, 80, 100)),
    fill.legend = tm_legend(title = title_legend, orientation = "landscape")) +
  tm_layout(legend.only = TRUE)

tmap_arrange(map1, map2, map3, legend.map2, ncol =3, nrow = 2)
#tmap_arrange(map1, map2, map3, legend.map)

## Suppl. Fig. 3: difference intakes by region & district ----
# Loading the data

pal_base <- c("#ffa600","#003f5c", "#35D0BA" )
# n <- length(unique(three$variable)) - 1

title <-expression(paste(
  "Apparent Se intake (",   mu, "g/", "AFE/day)", sep=""))

ihs4_design2 %>% 
  group_by(region, ADM2_EN ) %>% 
  summarise(across(starts_with("apparent_se"),
                   ~srvyr::survey_quantile(.x, c(0.5)))) %>% 
  select(-ends_with("se")) %>% 
  rowwise() %>% 
  mutate(mymedian = median(c(apparent_se_q50, apparent_se_N_q50, apparent_se_ea_q50))) %>% 
  arrange(mymedian) %>% 
  mutate(ADM2_EN = factor(ADM2_EN, ADM2_EN)) %>% 
  pivot_longer(cols = starts_with("apparent_se"),
               names_to = "method", 
               values_to = "app_Se") %>% 
  mutate(Scenario = case_when(
    method == "apparent_se_q50" ~ "Baseline NCT", 
    method == "apparent_se_ea_q50" ~ "Spatially-resolved maize NCT", 
    method == "apparent_se_N_q50" ~ "National maize NCT")) %>% 
  mutate(region = factor(region, levels = c(1, 2, 3), 
                         labels = c("Northern", "Central", "Southern"))) %>% 
  ggplot(aes(x = app_Se, y = ADM2_EN )) +
  stat_summary(
    geom = "linerange", fun.min = "min", fun.max = "max", 
    linewidth = 0.8, color = "black")+
  ## white point to overplot line endings
  geom_point(
    aes(x = app_Se), size = 5,  stroke = 1, color = "white", fill = "white"
  ) +
  geom_point(
    aes(x = app_Se, colour = Scenario), size = 5, alpha =.6 ,stroke = 1) +
  scale_colour_manual(values = pal_base) +
  xlab(title) +
  ylab("") +
  facet_wrap(~ region, scales = "free_y") +
  theme_bw() +
  theme(
    legend.position = "top", 
    text = element_text(size = 20)) 

## Suppl. Table. 2: inadequacy risk by residency region & district ----


ihs4_summary  <- ihs4_summary %>% 
  mutate(region = factor(region, levels = c(1, 2, 3), 
                         labels = c("Northern", "Central", "Southern")),
         reside = factor(reside, levels = c(1, 2), 
                         labels = c("Urban", "Rural"))) 

# Applying Survey weights & strata -----
# PSU = ea_id
ihs4_design<-svydesign(id=~ea_id, 
                       weights=~hh_wgt, strata = ~district+reside,
                       data=ihs4_summary)

# App. inadequacy ----
# Global mean
svyciprop(~se.inad,ihs4_design,  method = "mean" , level = 0.95)*100
#svymean(~se.inad, ihs4_design)
svyciprop(~se.inad_N,ihs4_design,  method = "mean" , level = 0.95)*100
#svymean(~se.inad_N, ihs4_design)
svyciprop(~se.inad_ea,ihs4_design,  method = "mean" , level = 0.95)*100
#svymean(~se.inad_ea, ihs4_design)



table <- read.csv(here::here("output", "risk-app-Se-inadequacy_v4.0.0.csv")) %>%
  dplyr::select(-c(se, se.x, se.y))

table <- table %>%
  dplyr::select(-c(se, se.x, se.y))

names(table) 

names(table) <- c("variable", "inadeq_se","inadeq_se_ci_l", "inadeq_se_ci_u" ,  
                "inadeq_se_N" , "inadeq_se_N_ci_l" , "inadeq_se_N_ci_u", 
                "inadeq_se_ea",   "inadeq_se_ea_ci_l",  "inadeq_se_ea_ci_u")

table <- table %>% 
  mutate(across(starts_with("inadeq_se"), ~round(., ))) %>% 
  mutate(inadeq_se_ci = paste0("(", inadeq_se_ci_l, "-", inadeq_se_ci_u, ")")) %>% 
  mutate(inadeq_se_N_ci = paste0("(", inadeq_se_N_ci_l, "-", inadeq_se_N_ci_u, ")")) %>% 
  mutate(inadeq_se_ea_ci = paste0("(", inadeq_se_ea_ci_l, "-", inadeq_se_ea_ci_u, ")")) %>% 
  dplyr::select("variable", 
                "inadeq_se","inadeq_se_ci",   
                "inadeq_se_N" , "inadeq_se_N_ci" , 
                "inadeq_se_ea",   "inadeq_se_ea_ci") 


# Saving Sp. Table 4 -----
write.csv(table , here::here("output", "SM", "Suppl.Table4_risk-app-Se-inadequacy_v4.0.1.csv"), 
          row.names = FALSE)


## Fig. 4: difference inadequacy ----
# Loading the data

three <- read.csv(here::here("output", "risk-app-Se-inadequacy_v4.0.1.csv")) %>% 
  slice(6:37)

pal_base <- c("#ffa600","#003f5c", "#35D0BA" )
n <- length(unique(three$variable)) - 1

title <- "Risk of apparent Se inadequacy (%)"

three %>% 
  rowwise() %>% 
  mutate(mymean = mean(c(se.inad, se.inad_N, se.inad_ea))) %>% 
  arrange(mymean) %>% 
  mutate(variable = factor(variable, variable)) %>% 
  pivot_longer(cols = starts_with("se.inad"),
               names_to = "method", 
               values_to = "app_Se") %>% 
  mutate(Scenario = case_when(
    method == "se.inad" ~ "Baseline NCT", 
    method == "se.inad_ea" ~ "Spatially-resolved maize NCT", 
    method == "se.inad_N" ~ "National maize NCT")) %>% 
  left_join(., ea[, c("ADM2_EN", "ADM1_EN" )] %>% st_drop_geometry() %>% 
              distinct(), by = c("variable" = "ADM2_EN")) %>% 
  filter(!is.na(ADM1_EN)) %>% 
  mutate(ADM1_EN = factor(ADM1_EN, 
                          levels = c("Northern", "Central", "Southern"))) %>% 
  ggplot(aes(x = app_Se, y = variable )) +
  stat_summary(
    geom = "linerange", fun.min = "min", fun.max = "max", 
    linewidth = 0.8, color = "black")+
  ## white point to overplot line endings
  geom_point(
    aes(x = app_Se), size = 5,  stroke = 1, color = "white", fill = "white"
  ) +
  geom_point(
    aes(x = app_Se, colour = Scenario), size = 5, alpha =.6 ,stroke = 1) +
  ## app. estima labels
  # geom_text(
  #   aes(label = round(app_Se), 
  #       x = app_Se, vjust = -1, color = method),
  # #  fontface = c(rep("plain", n*2), rep("bold", 2)),
  #   family = "sans", size = 4.2) +
  scale_colour_manual(values = pal_base) +
  xlab(title) +
  ylab("") +
  facet_wrap(~ ADM1_EN, scales = "free_y") +
  theme_bw() +
  theme(
    legend.position = "top", 
    text = element_text(size = 20)) 
