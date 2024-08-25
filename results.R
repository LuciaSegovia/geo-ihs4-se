

# Envir prep -----
library(survey) # survey design
library(srvyr) # survey design
library(sf) # spatial data manipulation
library(tmap) # map viz
library(dplyr) #data manipulation
library(tidyr) #data transformation
library(ggplot2) #data viz

# Loading data -----
# Loading IHS4 intkes
ihs4_summary <-  readRDS(here::here("data", "inter-output", "ihs4-hh-intakes.RDS"))
#sum(is.na(ihs4_summary$ea_id))

## Loading Malawi EA shapefile & district names (generated in geo-spatial/00.cleaning-boundaries.R)
ea <- st_read(here::here( "data", "boundaries", "mwi_admbnda_adm4_nso.shp")) %>% 
  mutate(ADM2_PCODE = gsub("MW", "",ADM2_PCODE ))

# RESULTS -----

# NCT ----
nct %>% count(source_fct)
(51+8+6+4)/150 #KE18
(23+11+4+2+1+1)/150 #MW19

## Table: Maize Se values ----

names(maize.df)

maize.df %>% group_by(food_code) %>% 
  dplyr::summarise(
    mean = mean(Se_mcg_100g), 
    sd =sd(Se_mcg_100g, na.rm = TRUE),
    Q25 =quantile(Se_mcg_100g, c(0.25), na.rm = TRUE),
    median = median(Se_mcg_100g, na.rm = TRUE), 
    Q75 =quantile(Se_mcg_100g, c(0.75), na.rm = TRUE), 
 #   iqr =IQR(Se_mcg_100g, na.rm = TRUE),
    min =min(Se_mcg_100g,  na.rm = TRUE),
    max =max(Se_mcg_100g,  na.rm = TRUE),
#    Q95 =quantile(Se_mcg_100g, c(0.95), na.rm = TRUE), 
    ) %>% left_join(., national_maize, by =c("food_code"= "code")) %>% 
  left_join(., nct[, c("code", "item", "SEmcg", "source_fct")], 
            by =c("food_code"= "code")) %>%
  relocate( item, .after = "food_code") %>% 
  dplyr::mutate(across(is.numeric, round,  3)) %>% 
  View()
  
### Boxplots: Maize Se

# Draw line segment
data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)

test <- nct %>% filter(code %in% maize.df$food_code) %>% 
  select(code, SEmcg) %>% 
  mutate(y1 = SEmcg, 
       #  y = row.names()+.3, 
         y2 = SEmcg, 
      #   yend =  nrow()-.3
      ) %>% arrange(code)

test$x1 <- seq(1:6)+.3  
test$x2 <- seq(1:6)-.3  
test$legend <- paste0(test$code, " (", round(test$SEmcg,2),  ")")

geom_segment(x = 3.81,
               y = food_code,
               xend = 3.81,
               yend = food_code)

maize.df %>%  left_join(., nct[, c("code", "item", "SEmcg")], 
                        by =c("food_code"= "code")) %>%
  mutate(food_desc =  paste0(item, " (", food_code,  ")")) %>% 
  ggplot(aes(reorder(food_desc, as.numeric(food_code)), Se_mcg_100g)) +
#  ggplot(aes(food_code, Se_mcg_100g)) + 
  geom_boxplot() + 
  coord_flip() +
  # Draw line segment
 # geom_segment(x = 6.3,
 #              y = 3.81,
 #              xend = 5.7,
 #              yend = 3.81, colour = "blue")
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, 
                   colour = legend), data = test, 
               size = 1.5) + 
  theme_bw() #+
#  theme(legend.position = "top") 

  
# Applying Survey weights & strata -----
# PSU = ea_id
ihs4_design<-svydesign(id=~ea_id, 
                       weights=~hh_wgt, strata = ~district+reside,
                       data=ihs4_summary)

# Apparent intakes -----
svyhist(~apparent_kcal, ihs4_design)
abline(v = svymean(~apparent_kcal, ihs4_design)[1], lwd = 3, lty = 2)
abline(v = 2050, lwd = 3, lty = 2, col = "green") # Energy req for WRA
# abline(v = 1900, lwd = 3, lty = 2, col = "red")

# AFE kcal (2,050)
svymean(~apparent_se_ea, ihs4_design)
svymean(~apparent_se_N, ihs4_design)
svymean(~apparent_se, ihs4_design)
svymean(~apparent_kcal, ihs4_design)
svyby(~apparent_kcal, ~reside*region,  ihs4_design, svymean, vartype=c("se","ci"))


## Urban/rural ----
one <- svyby(~apparent_kcal, ~reside,  ihs4_design, svymean, vartype=c("se","ci")) %>% 
left_join(., svyby(~apparent_se_ea, ~reside,  ihs4_design, svymean, 
                   vartype=c("se","ci")), by = "reside") %>% 
  left_join(., svyby(~apparent_se, ~reside,  ihs4_design, svymean, 
                     vartype=c("se","ci")),by = "reside" ) %>% 
left_join(., svyby(~apparent_se_N, ~reside,  ihs4_design, svymean, 
                   vartype=c("se","ci")),by = "reside" ) # %>% View()

names(one)[1] <- "variable"

## region ----
two <- svyby(~apparent_kcal, ~region,  ihs4_design, svymean, vartype=c("se","ci")) %>% 
  left_join(., svyby(~apparent_se_ea, ~region,  ihs4_design, svymean,
                     vartype=c("se","ci")), by = "region") %>% 
  left_join(., svyby(~apparent_se, ~region,  ihs4_design, svymean, 
                     vartype=c("se","ci")),by = "region" ) %>% 
  left_join(., svyby(~apparent_se_N, ~region,  ihs4_design, svymean, 
                     vartype=c("se","ci")),by = "region" ) #%>% View()

names(two)[1] <- "variable"

## district ----
three <- svyby(~apparent_kcal, ~ADM2_EN,  ihs4_design, svymean, vartype=c("se","ci")) %>% 
  left_join(., svyby(~apparent_se_ea, ~ADM2_EN,  ihs4_design, svymean,
                     vartype=c("se","ci")), by = "ADM2_EN") %>% 
  left_join(., svyby(~apparent_se, ~ADM2_EN,  ihs4_design, svymean, 
                     vartype=c("se","ci")),by = "ADM2_EN" )  %>%
left_join(., svyby(~apparent_se_N, ~ADM2_EN,  ihs4_design, svymean, 
                   vartype=c("se","ci")),by = "ADM2_EN" ) 
names(three)[1] <- "variable"

table <- rbind(one, two, three)

# Saving Table 3 -----
write.csv(table , here::here("output", "apparent-intakes_v3.0.0.csv"), 
          row.names = FALSE)


## Fig. 1: difference intake ----
pal_base <- c("#ffa600","#003f5c", "#35D0BA" )
n <- length(unique(three$variable)) - 1


three %>% 
  rowwise() %>% 
  mutate(mymean = mean(c(apparent_se,apparent_se_ea, apparent_se_N) )) %>% 
  arrange(mymean) %>% 
  mutate(variable = factor(variable, variable)) %>% 
  mutate(mymean = mean(c(apparent_se,apparent_se_ea, apparent_se_N) )) %>% 
  arrange(mymean) %>% 
  pivot_longer(cols = starts_with("apparent_se"),
               names_to = "method", 
               values_to = "app_Se") %>% 
  ggplot(aes(x = app_Se, y = variable )) +
  stat_summary(
    geom = "linerange", fun.min = "min", fun.max = "max", 
    linewidth = 0.8, color = c(rep("black", n-1), rep("grey", 2)))+
  ## white point to overplot line endings
  geom_point(
    aes(x = app_Se), size = 5,  stroke = 1, color = "white", fill = "white"
  ) +
  geom_point(
    aes(x = app_Se, colour = method), size = 5, alpha =.6 ,stroke = 1) +
  ## app. estima labels
 # geom_text(
 #   aes(label = round(app_Se), 
 #       x = app_Se, vjust = -1, color = method),
 # #  fontface = c(rep("plain", n*2), rep("bold", 2)),
 #   family = "sans", size = 4.2) +
  scale_colour_manual(values = pal_base) +
  xlab("Apparent Se intake (mcg/AFE/day)") +
  ylab("") +
  theme_bw()# +
  #Legend
  #  annotate(
  #   geom = "text", y = n + 1.8, x = c(30, 65, 110) ,
  #    label = c("conventional NCT", "national maize NCT","Cluster NCT"), family = "sans", 
  #    fontface = "bold", color = pal_base, size = 5, hjust = .5) 


#pal_base <- c("#EFAC00", "#28A87D")
pal_base <- c("#ffa600","#003f5c", "#35D0BA" )
n <- length(unique(three$variable)) - 1

three %>% 
rowwise() %>% 
  mutate(mymean = mean(c(apparent_se,apparent_se_ea, apparent_se_N) )) %>% 
  arrange(mymean) %>% 
  mutate(variable = factor(variable, variable)) %>% 
  ggplot() +
  geom_segment(aes(x=variable, xend=variable, 
                   y=apparent_se, yend=apparent_se_ea), color="grey") +
  geom_point(aes(x=variable, y=apparent_se), 
             color= "#003f5c", alpha = 0.5, size=3) +
  geom_point( aes(x=variable, y=apparent_se_ea), 
              color="#ffa600",alpha = 0.5, size=3) +
  xlab("") +
  ylab("Apparent Se intake (mcg/AFE/day)") +
 # hrbrthemes::theme_ipsum() +
  theme_bw() +
  #Legend
#  annotate(
#   geom = "text", x = n + 1.8, y = c(30, 110) ,
#    label = c("National FCT", "Geo-ref. FCT"), family = "sans", 
#    fontface = "bold", color = pal_base, size = 5, hjust = .5
#  ) +
  coord_flip(
                  clip = 'off') +   # This keeps the labels from disappearing
  
  # scale_colour_manual(values = pal_base) 
 # scale_x_discrete(expand = expansion(add =  c(.35, 1.5))) +
  # scale_y_continuous(expand = expansion(add = c(5, 10))) 
  theme(axis.ticks.x.top = element_blank(),
        axis.text.x.top = element_text(size = 14, color = "black"),
    plot.margin = margin(t = 15,  # Top margin
                       r = 15,  # Right margin
                       b = 5,  # Bottom margin
                       l = 1)) 

## Fig. 2: seasonal variation -----

svyby(~apparent_se_ea, ~Date*reside,  ihs4_design, svymean) %>% 
left_join(., svyby(~apparent_se, ~Date*reside,  ihs4_design, svymean), 
          by = c("Date", "reside")) %>% 
  ggplot() +
  geom_line(aes( Date, apparent_se), colour = "grey") +
  geom_line(aes( Date, apparent_se_ea), colour = "blue") +
  geom_vline(xintercept = as.Date("2016-10-01"), colour = "red") +
  geom_vline(xintercept = as.Date("2017-03-01"), colour = "red") +
# Date > "2016-09-30" & Date < "2017-03-01"
  
  theme_minimal()+
  facet_wrap(~reside, nrow =2)


# Appartent Se inadeq. per district/region/reside
svyby(~se.inad, ~reside,  ihs4_design, svyciprop) %>% 
  left_join(., svyby(~se.inad_ea, ~reside,  ihs4_design, svyciprop)) %>% 
  mutate(diff_inad = se.inad-se.inad_ea) %>% 
  relocate(c(se.inad_ea,diff_inad), .after = "se.inad") %>% 
  arrange(desc(diff_inad)) %>% View()

## Maps of intake -----
# Palettes from 
#https://r-charts.com/color-palettes/#continuous
svyby(~apparent_kcal, ~district,  ihs4_design, svymean, vartype=c("se","ci")) %>% 
  left_join(., ea,  by = c("district"= "ADM2_PCODE")) %>% st_as_sf() %>% 
  tm_shape() +
  #  tm_polygons(
  tm_fill( 
    "apparent_kcal", 
    style = "cont", # changed fixed to continous
    #  breaks = c(6, 7, 9, 10, 12, 14), 
  #  palette = "YlOrBr") +
    palette = paletteer::paletteer_c("ggthemes::Blue-Teal", 30)) +
  tm_layout(legend.outside = TRUE, legend.text.size = 1)

# Do a divergent map using 45 cut-off
svyby(~apparent_se, ~district,  ihs4_design, svymean, vartype=c("se","ci")) %>% 
  left_join(., ea,  by = c("district"= "ADM2_PCODE")) %>% st_as_sf() %>% 
  tm_shape() +
  #  tm_polygons(
  tm_fill( 
    "apparent_se", 
    style = "cont", # changed fixed to continous
   #   breaks = c(20, 40, 60, 80, 100), 
      breaks = c(20, 35, 45, 80, 100), 
 #  values.range =c(0.15,1),
     palette = paletteer::paletteer_c("ggthemes::Red-Green-White Diverging", 20))+
     #palette = paletteer::paletteer_c("ggthemes::Red-Blue Diverging", 20))+
 # tm_scale_continuous(values.range = c(0.15, 1),
#  palette = "YlOrBr") +
  tm_layout(legend.outside = TRUE, legend.text.size = 1)

# App. inadequacy ----
# Global mean
svymean(~se.inad, ihs4_design)
svymean(~se.inad_N, ihs4_design)
svymean(~se.inad_ea, ihs4_design)


## Urban/rural ----
one <- svyby(~se.inad, ~reside,  ihs4_design, svymean, vartype=c("se","ci")) %>% 
  left_join(., svyby(~se.inad_N, ~reside,  ihs4_design, svymean, 
                     vartype=c("se","ci")), by = "reside") %>% 
  left_join(., svyby(~se.inad_ea, ~reside,  ihs4_design, svymean, 
                     vartype=c("se","ci")),by = "reside" ) # %>% View()

names(one)[1] <- "variable"

## region ----
two <- svyby(~se.inad, ~region,  ihs4_design, svymean, vartype=c("se","ci")) %>% 
  left_join(., svyby(~se.inad_N, ~region,  ihs4_design, svymean,
                     vartype=c("se","ci")), by = "region") %>% 
  left_join(., svyby(~se.inad_ea, ~region,  ihs4_design, svymean, 
                     vartype=c("se","ci")),by = "region" ) #%>% View()

names(two)[1] <- "variable"

## district ----
three <- svyby(~se.inad, ~ADM2_EN,  ihs4_design, svymean, vartype=c("se","ci")) %>% 
  left_join(., svyby(~se.inad_N, ~ADM2_EN,  ihs4_design, svymean,
                     vartype=c("se","ci")), by = "ADM2_EN") %>% 
  left_join(., svyby(~se.inad_ea, ~ADM2_EN,  ihs4_design, svymean, 
                     vartype=c("se","ci")),by = "ADM2_EN" )  
names(three)[1] <- "variable"

# Binding them all into one table
table <- rbind(one, two, three)

# Rounding & changing into perc. (%)
table[,c(2:13)] <- round(table[,c(2:13)], 4)*100

# Saving Table 4 -----
write.csv(table , here::here("output", "risk-app-Se-inadequacy_v3.0.0.csv"), 
          row.names = FALSE)


## Fig. 3: difference inadequacy ----
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
  left_join(., ea[, c("ADM2_EN", "ADM1_EN" )] %>% st_drop_geometry() %>% 
              distinct(), by = c("variable" = "ADM2_EN")) %>% 
  filter(!is.na(ADM1_EN)) %>% 
  mutate(ADM1_EN = factor(ADM1_EN, 
                          levels = c("Northern", "Central", "Southern"))) %>% 
  ggplot(aes(x = app_Se, y = variable )) +
  stat_summary(
    geom = "linerange", fun.min = "min", fun.max = "max", 
    linewidth = 0.8, color = c(rep("black", n-1), rep("grey", 2)))+
  ## white point to overplot line endings
  geom_point(
    aes(x = app_Se), size = 5,  stroke = 1, color = "white", fill = "white"
  ) +
  geom_point(
    aes(x = app_Se, colour = method), size = 5, alpha =.6 ,stroke = 1) +
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
  theme_bw() 
  
## Maps of app. inadeq -----
svyby(~se.inad_N, ~district,  ihs4_design, svyciprop) %>% 
  left_join(., ea,  by = c("district"= "ADM2_PCODE")) %>% st_as_sf() %>% 
  mutate(se.inad = se.inad_N*100) %>% 
  tm_shape() +
  # tm_polygons(
  tm_fill(
    "se.inad", 
    style = "cont", 
    breaks = c(0, 20, 50, 80, 100), 
   # palette = "OrRd"
   palette = paletteer::paletteer_c("ggthemes::Red-Gold", 20)) +
  tm_layout(legend.outside = TRUE, legend.text.size = 1)

round(svyby(~se.inad, ~district,  ihs4_design, svyciprop, vartype=c("se","ci"))[,2:5], 3)*100

svyciprop(~se.inad,  ihs4_design)*100

# Check only from Oct, 2015 - Feb, 2016. 




# Table with overall characteristics (incl. No HH)

# Food intake: Maize and maize products total and fractions (mean (SD) in g/AFE/day)

# Food intake: Maize and maize products (district map) 

# Food intake: Maize and maize products (seasonality total and fractions)



# Check only from Oct, 2015 - Feb, 2016. -----

# Checking overall energy for lean season
ihs4_summary %>% filter(Date > "2016-09-30" & Date < "2017-03-01") %>% 
  ungroup() %>% 
  group_by(reside) %>% 
  summarise(across(starts_with("apparent_"), ~mean(.x, na.rm = TRUE)))

class(ihs4$interviewDate)
max(ihs4$interviewDate)


#ihs4$Date <- as.Date(ihs4$interviewDate, '%Y-%m-%d')
ggplot( data = ihs4, aes( Date, g_AFE )) + geom_line() 

ihs4 %>% filter(
  grepl("maize", item, ignore.case = TRUE),  
  !grepl("green|boil", item, ignore.case = TRUE)) %>% 
  # filter(Date>"2016-10-01" & Date<"2017-02-28") %>% 
  group_by(Date, item) %>% 
  summarise(mean_day = mean(g_AFE)) %>% 
  ggplot(aes( Date, mean_day, colour = item)) + geom_line() +
  theme_minimal()

## Median using srvyr package ----
# PSU = ea_id
ihs4_design2 <- ihs4_summary %>%
  as_survey_design(strata = c(district, reside), weights = hh_wgt)

ihs4_design2 %>%
  summarise(md = srvyr::survey_median(apparent_se, na.rm=TRUE), 
            mean = survey_mean(apparent_se, na.rm=TRUE))




## Checking differences in the means ---------

svyttest( apparent_se_ea ~ reside , ihs4_design )

#https://stackoverflow.com/questions/70626996/r-how-to-conduct-two-sample-t-test-with-two-different-survey-designs
#svyhist(~log(apparent_se_ea), ihs4_design)
# App. intake
m_ea <- svymean(~log(apparent_se_ea), ihs4_design)
m_N <- svymean(~log(apparent_se_N), ihs4_design)
m_Se <- svymean(~log(apparent_se), ihs4_design)

# App. indeq.
m_ea <- svymean(~se.inad, ihs4_design)
m_N <- svymean(~se.inad_N, ihs4_design)
m_Se <- svymean(~se.inad_ea, ihs4_design)

coef_one <- coef( m_ea )
coef_two <- coef( m_N )
coef_three <- coef( m_Se )
se_one <- SE( m_ea )
se_two <- SE( m_N )
se_three <- SE( m_Se )

# Comparing Cluster-level w/ national-level
t_statistic <- abs( coef_one - coef_two ) / sqrt ( se_one ^2 + se_two ^2 )
p_value <- ( 1 - pnorm( abs( coef_one - coef_two ) / sqrt( se_one ^2 + se_two ^2 ) ) ) * 2
sig_diff <- ifelse( 1 - pnorm( abs( coef_one - coef_two ) / sqrt( se_one ^2 + se_two ^2 ) ) < 0.025 , "*" , "" )

# Comparing Cluster-level w/ FCT
t_statistic <- abs( coef_one - coef_three ) / sqrt ( se_one ^2 + se_three ^2 )
p_value <- ( 1 - pnorm( abs( coef_one - coef_three ) / sqrt( se_one ^2 + se_three ^2 ) ) ) * 2
sig_diff <- ifelse( 1 - pnorm( abs( coef_one - coef_three ) / sqrt( se_one ^2 + se_three ^2 ) ) < 0.025 , "*" , "" )
