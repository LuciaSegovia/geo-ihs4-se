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
sum((58+3+2+1+2+2+1)/nrow(nct), #KE18
(2+6+2+1+1+23+3)/nrow(nct), #MW19
(1)/nrow(nct), #LS06
(16+1+1)/nrow(nct), #UK21
(3)/nrow(nct)) #US19

## Fig. 1 ---------------------



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
    strip.background = element_rect( fill="white"),
     legend.position=c(.75,.82),
    text = element_text(size = 20))

# Loading IHS4 intkes
ihs4_summary <-  readRDS(here::here("data", "inter-output", "ihs4-hh-intakes.RDS"))
#sum(is.na(ihs4_summary$ea_id))

## Loading Malawi EA shapefile & district names (generated in geo-spatial/00.cleaning-boundaries.R)
ea <- st_read(here::here( "data", "boundaries", "mwi_admbnda_adm4_nso.shp")) %>% 
  mutate(ADM2_PCODE = gsub("MW", "",ADM2_PCODE ))


# Results: Figures -----

## Median using srvyr package ----
# PSU = ea_id

ihs4_summary$region <- as.factor(ihs4_summary$region)
ihs4_design2 <- ihs4_summary %>% ungroup() %>% 
  as_survey_design(strata = c(district, reside), weights = hh_wgt)

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


## Figure 3: Maps of intake -----
# Palettes from 
#https://r-charts.com/color-palettes/#continuous
var <- "ADM2_EN"
#apparent_se_q50, apparent_se_N_q50, apparent_se_ea_q50 
map3 <- ihs4_design2 %>% 
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
 fill.legend = tm_legend(show = FALSE)) +
 #fill.legend = tm_legend(title = "Apparent Se intake")) 
    #  palette = "YlOrBr") +
  #  palette = paletteer::paletteer_c("ggthemes::Blue-Teal", 30))
#tm_style("natural", earth_boundary = c( 33, -18,  36 , -10), inner.margins = .05)
  tm_style("classic_v3") 

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
    fill.legend = tm_legend(title = title_legend1, orientation = "portrait")) +
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



## Fig. 4: difference inadequacy ----
# Loading the data

three <- read.csv(here::here("output", "risk-app-Se-inadequacy_v3.0.0.csv")) %>% 
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
