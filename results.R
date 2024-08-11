

# Envir prep -----
library(survey) # survey design
library(sf) # spatial data manipulation
library(tmap) # map viz

# RESULTS -----


# Applying Survey weights & strata -----
# PSU = ea_id
ihs4_design<-svydesign(id=~ea_id, 
                       weights=~hh_wgt, strata = ~district+reside,
                       data=ihs4_summary)

svyhist(~apparent_kcal, ihs4_design)
abline(v = svymean(~apparent_kcal, ihs4_design)[1], lwd = 3, lty = 2)
abline(v = 2050, lwd = 3, lty = 2, col = "green")
# abline(v = 1900, lwd = 3, lty = 2, col = "red")

# AFE kcal (2,050)
svymean(~apparent_se_ea, ihs4_design)
svymean(~apparent_se, ihs4_design)
svymean(~apparent_kcal, ihs4_design)
svyby(~apparent_kcal, ~reside*region,  ihs4_design, svymean, vartype=c("se","ci"))


## Urban/rural ----
one <- svyby(~apparent_kcal, ~reside,  ihs4_design, svymean, vartype=c("se","ci")) %>% 
left_join(., svyby(~apparent_se_ea, ~reside,  ihs4_design, svymean, 
                   vartype=c("se","ci")), by = "reside") %>% 
  left_join(., svyby(~apparent_se, ~reside,  ihs4_design, svymean, 
                     vartype=c("se","ci")),by = "reside" ) # %>% View()

names(one)[1] <- "variable"

## region ----
two <- svyby(~apparent_kcal, ~region,  ihs4_design, svymean, vartype=c("se","ci")) %>% 
  left_join(., svyby(~apparent_se_ea, ~region,  ihs4_design, svymean,
                     vartype=c("se","ci")), by = "region") %>% 
  left_join(., svyby(~apparent_se, ~region,  ihs4_design, svymean, 
                     vartype=c("se","ci")),by = "region" )# %>% View()

names(two)[1] <- "variable"

## district ----
three <- svyby(~apparent_kcal, ~ADM2_EN,  ihs4_design, svymean, vartype=c("se","ci")) %>% 
  left_join(., svyby(~apparent_se_ea, ~ADM2_EN,  ihs4_design, svymean,
                     vartype=c("se","ci")), by = "ADM2_EN") %>% 
  left_join(., svyby(~apparent_se, ~ADM2_EN,  ihs4_design, svymean, 
                     vartype=c("se","ci")),by = "ADM2_EN" ) # %>% View()

names(three)[1] <- "variable"

table <- rbind(one, two, three)

write.csv(table , here::here("output", "apparent-intakes_v1.0.0.csv"), 
          row.names = FALSE)


## Fig. 1: difference intake ----

three %>% 
rowwise() %>% 
  mutate(mymean = mean(c(apparent_se,apparent_se_ea) )) %>% 
  arrange(mymean) %>% 
  mutate(variable = factor(variable, variable)) %>% 
  ggplot() +
  geom_segment(aes(x=variable, xend=variable, 
                   y=apparent_se, yend=apparent_se_ea), color="grey") +
  geom_point(aes(x=variable, y=apparent_se), 
             color=rgb(0.2,0.7,0.1,0.5), size=3) +
  geom_point( aes(x=variable, y=apparent_se_ea), 
              color=rgb(0.7,0.2,0.1,0.5), size=3) +
  coord_flip()+
  xlab("") +
  ylab("apparent Se inatke (mcg/AFE/day)") +
 # hrbrthemes::theme_ipsum() +
  theme_bw() 
  
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

## Maps of intake
svyby(~apparent_kcal, ~district,  ihs4_design, svymean, vartype=c("se","ci")) %>% 
  left_join(., ea,  by = c("district"= "ADM2_PCODE")) %>% st_as_sf() %>% 
  tm_shape() +
  #  tm_polygons(
  tm_fill( 
    "apparent_kcal", 
    style = "cont", # changed fixed to continous
    #  breaks = c(6, 7, 9, 10, 12, 14), 
    palette = "YlOrBr") +
  tm_layout(legend.outside = TRUE, legend.text.size = 1)

svyby(~apparent_se_ea, ~district,  ihs4_design, svymean, vartype=c("se","ci")) %>% 
  left_join(., ea,  by = c("district"= "ADM2_PCODE")) %>% st_as_sf() %>% 
  tm_shape() +
  #  tm_polygons(
  tm_fill( 
    "apparent_se_ea", 
    style = "cont", # changed fixed to continous
    #  breaks = c(6, 7, 9, 10, 12, 14), 
    palette = "YlOrBr") +
  tm_layout(legend.outside = TRUE, legend.text.size = 1)

svyby(~se.inad_ea, ~~district,  ihs4_design, svyciprop) %>% 
  left_join(., ea,  by = c("district"= "ADM2_PCODE")) %>% st_as_sf() %>% 
  mutate(se.inad = se.inad_ea*100) %>% 
  tm_shape() +
  # tm_polygons(
  tm_fill(
    "se.inad", 
    style = "cont", 
    breaks = c(0, 20, 50, 80, 100), 
    palette = "OrRd") +
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
