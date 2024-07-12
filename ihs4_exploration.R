


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

# Combining dataset & calcuating consumption per AFE (g/AFE/d)

ihs4 <- ihs4.df %>% left_join(., hh.hme) %>%
  mutate(g_AFE = kg_d/afe*1000)

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

# Fixing them >5000kg
for(i in 1:length(value)){
  
x <- median(ihs4$g_AFE[ihs4$item_code %in% value[i] & ihs4$g_AFE<cut.off[i]])
text <- paste("outliers: changed AFE_g for item", value[i])

if(is.na(ihs4$comments[ihs4$item_code %in% value[i] & ihs4$g_AFE>cut.off[i]])){
  
ihs4$comments[ihs4$item_code %in% value[i] & ihs4$g_AFE>cut.off[i]] <- text
ihs4$g_AFE[ihs4$item_code %in% value[i] & ihs4$g_AFE>cut.off[i]] <- x

} else{
  
  tex2 <- paste(ihs4$comments[ihs4$item_code %in% value[i] &
                                ihs4$g_AFE>cut.off[i]], text, sep = ";") 

  ihs4$comments[ihs4$item_code %in% value[i] & ihs4$g_AFE>cut.off[i]] <- text
  ihs4$g_AFE[ihs4$item_code %in% value[i] & ihs4$g_AFE>cut.off[i]] <- x
  
}
}

ihs4 %>% filter(item_code == "404" & g_AFE>1000)


# Checking the data
ihs4 %>% filter(g_AFE>1000) %>% 
  ggplot(aes(g_AFE, as.character(item_code), colour = item)) +
  geom_point()

value <- c("106", )
cut.off <- c(1000 )

ihs4$item[ihs4$item_code == "404"]
# Checking extreme/ implausible values: 
hist(ihs4$g_AFE[ihs4$item_code == "404"])
hist(ihs4$g_AFE[ihs4$item_code == "404" & ihs4$g_AFE<1000])
# No. of values afected
length(ihs4$g_AFE[ihs4$item_code == "404"])
length(ihs4$g_AFE[ihs4$item_code == "404" & ihs4$g_AFE>1000])

# Fixing them >1000kg
for(i in 1:length(value)){
  
  x <- median(ihs4$g_AFE[ihs4$item_code %in% value[i] & ihs4$g_AFE<cut.off[i]])
  text <- paste("outliers: changed AFE_g for item", value[i])
  
  ihs4$comments[ihs4$item_code %in% value[i] & ihs4$g_AFE>cut.off[i]] <- text
  ihs4$g_AFE[ihs4$item_code %in% value[i] & ihs4$g_AFE>cut.off[i]] <- x
  
}