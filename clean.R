#Clean.R
#
#
#Script cleans fifa.csv and prepares file fo visualization
#
#Alexander Kleefeldt - Jan 2018





library(readr)
library(dplyr)

data_fifa <- read_csv("data/fifa.csv")

#adding country columns
data_fifa <- data_fifa %>% mutate(country = ifelse(grepl("Spanish", league),"Spain"," "))
data_fifa <- data_fifa %>% mutate(country = ifelse(grepl("English", league),"England",country))
data_fifa <- data_fifa %>% mutate(country = ifelse(grepl("Italian", league),"Italy",country))
data_fifa <- data_fifa %>% mutate(country = ifelse(grepl("German", league),"Germany", country))
data_fifa <- data_fifa %>% mutate(country = ifelse(grepl("French", league),"France",country))


#remove countries that are not mentioned above 
data_fifa <- data_fifa %>% filter(country %in% c("England", "France", "Germany","Italy", "Spain"))


#create "position" column 
data_fifa <- data_fifa %>% mutate(position = ifelse(prefers_cb == "True"|
                                                      prefers_lb == "True"|
                                                      prefers_lcb == "True"|
                                                      prefers_lwb== "True"|
                                                      prefers_rb == "True"|
                                                      prefers_rcb == "True"|
                                                      prefers_rwb == "True",
                                                    "Defence", " "))

data_fifa <- data_fifa %>% mutate(position = ifelse(prefers_cf=="True"|
                                                      prefers_lf == "True"|
                                                      prefers_ls == "True"|
                                                      prefers_rf== "True"|
                                                      prefers_rs == "True"|
                                                      prefers_st == "True",
                                                    "Attack", position))

data_fifa <- data_fifa %>% mutate(position = ifelse(prefers_cam=="True"|
                                                      prefers_cdm == "True"|
                                                      prefers_cm == "True"|
                                                      prefers_lam== "True"|
                                                      prefers_lcm == "True"|
                                                      prefers_ldm == "True"|
                                                      prefers_lm == "True"|
                                                      prefers_lw == "True"|
                                                      prefers_ram == "True"|
                                                      prefers_rcm == "True"|
                                                      prefers_rdm == "True"|
                                                      prefers_rm == "True"|
                                                      prefers_rw == "True"|
                                                      prefers_lam == "True",
                                                    "Midfield", position))

data_fifa <- data_fifa %>% mutate(position = ifelse(prefers_gk == "True",
                                                    "Goalkeeper", position))




data_fifa <- data_fifa %>% rename(Passing = pas, 
                                  Shooting = sho, 
                                  Dribbling = dri,
                                  Pace = pac,
                                  Defending = def,
                                  Physical = phy,
                                  Overall_skill = overall,
                                  Height_cm = height_cm,
                                  Weight_kg = weight_kg)



write_csv(data_fifa, "data/fifa_clean.csv")




