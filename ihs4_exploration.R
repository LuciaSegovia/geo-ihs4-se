####################################################################
#                                                                  #
#                  This script explore the cleaned IHS4            #         
#                        for data inconsistencies                  #         
#                                                                  # 
#                                                                  #  
####################################################################    

library(dplyr) # Data cleaning 
library(ggplot2) # Data viz
#library(sp) # Spatial data manipulation
#library(sf) # Spatial data manipulation
#library(stars) # Spatial data manipulation
#library(tmap) # Spatial data viz

# Loading data

## IHS4 cleaned - Need AFE!
ihs4.df <- read.csv(here::here("data", "ihs4", "hh_mod_g_processed.csv")) %>% 
  dplyr::filter(consYN == 1)
names(ihs4)

## AFE
hh.hme <- read.csv(here::here("data", "ihs4", 
                              "ihs4.afe_v1.0.0.csv")) %>% 
  mutate(comments = ifelse(afe<1, 
                      paste("AFE <1: changed from", afe, "to 1"), NA),   
              afe = ifelse(afe<1, 1, afe))
names(hh.hme)
# There is one missing values for one HH in the AFE/AME. Check it.
hh.hme %>% filter(is.na(afe))

# Combining dataset & calcuating consumption per AFE (g/AFE/d)

ihs4 <- ihs4.df %>% left_join(., hh.hme) %>%
  mutate(g_AFE = kg_d/afe*1000) %>% 
  filter(!is.na(g_AFE))

# Checking the data
ihs4 %>% filter(g_AFE>5000) %>% 
  ggplot(aes(g_AFE, as.character(item_code), colour = item)) +
  geom_point()

value <- c("907", "817", "803", "801", "703", "701", "410", "114")
cut.off <- c(5000, 400, 5000, 400, 400, 5000, 2000, 2000)

ihs4$item[ihs4$item_code == "701"]
# Checking extreme/ implausible values: 
hist(ihs4$g_AFE[ihs4$item_code == "106"])
hist(ihs4$g_AFE[ihs4$item_code == "106" & ihs4$g_AFE<1000])
# No. of values afected
length(ihs4$g_AFE[ihs4$item_code == "106"])
length(ihs4$g_AFE[ihs4$item_code == "106" & ihs4$g_AFE>1000])

# Boxplot
ihs4 %>% filter(item_code %in% ihs4$item_code[ihs4$g_AFE>1000]) %>% 
  ggplot(aes(g_AFE, as.character(item_code), colour = item)) +
  geom_boxplot()

# Loop for fixing and adding comments to outliers
for(j in 1:nrow(ihs4)){
  
  value <- c("907", "913", "817", "803", "801", "703", "701", "410", "114", "106", "404", "814")
  cut.off <- c(5000, 2000 ,400, 5000, 400, 400, 5000, 2000, 2000, 2000, 1000, 1000)
  
for(i in 1:length(value)){
  
  if(ihs4$item_code[j] %in% value[i] & ihs4$g_AFE[j]>cut.off[i]){
  
x <- median(ihs4$g_AFE[ihs4$item_code %in% value[i] & ihs4$g_AFE<cut.off[i]])
text <- paste("outliers: changed AFE_g from", round(ihs4$g_AFE[j],2) , "to", round(x, 2), "for item code", value[i])

if(is.na(ihs4$comments[j])){
  
ihs4$comments[j] <- text
ihs4$g_AFE[j] <- x

} else{

  ihs4$comments[j] <- paste(ihs4$comments[j], text, sep = ";") 
  ihs4$g_AFE[j] <- x
  
}
  } else{
  next
}
}
}

ihs4 %>% filter(item_code == "817" & g_AFE>1000)


# Checking the data
ihs4 %>% filter(g_AFE>1000) %>% 
  ggplot(aes(g_AFE, as.character(item_code), colour = item)) +
  geom_point()



ihs4$item[ihs4$item_code == "114"]
# Checking extreme/ implausible values: 
hist(ihs4$g_AFE[ihs4$item_code == "404"])
hist(ihs4$g_AFE[ihs4$item_code == "404" & ihs4$g_AFE<1000])
# No. of values afected
length(ihs4$g_AFE[ihs4$item_code == "404"])
length(ihs4$g_AFE[ihs4$item_code == "404" & ihs4$g_AFE>1000])

