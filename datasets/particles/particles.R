# POPULATION DENSITY DATASET
# Adds grouping to population density dataset for color mapping

library(tidyverse)
library(readxl)
library(here)
library(countrycode)

migmat <- here("datasets", "matrix version 1.1.xls") %>% 
  read_excel()

continents <- tibble(wb = migmat$wbcode,
                     source = tolower(migmat$wbcode),
                     continent = countrycode(
                       migmat$wbcode, 
                       origin = "wb", 
                       destination = "continent"
                      ))

continents$continent[continents$wb == "OAN"] <- "Asia"
continents$continent[continents$wb == "ZAR"] <- "Africa"
continents$continent[continents$wb == "CIL"] <- "Oceania"
continents$continent[continents$wb == "TMP"] <- "Oceania"
continents$continent[continents$wb == "NIU"] <- "Oceania"
continents$continent[continents$wb == "ROM"] <- "Europe"
continents$continent[continents$wb == "YUG"] <- "Europe"
continents$continent[continents$continent == "Americas" | continents$continent == "Oceania"] <- "Americas & Oceania"

groups <- migmat %>% 
  pivot_longer(
    cols = !c("wbcode", "wbname", "update"),
    names_to = "source",
    values_to = "share"
  ) %>% 
  select(!update) %>% 
  
  # Aggregate migrant source by continent
  left_join(continents, by = "source") %>% 
  mutate(continent = case_when(wbcode == wb ~ "Own",
                               TRUE ~ continent)) %>% 
  group_by(wbcode, wbname, continent) %>% 
  summarize(share = sum(share)) %>% 
  ungroup() %>% 
  
  # Assign migrant sources to groups
  mutate(rank = case_when(continent == "Own" ~ 2,
                          TRUE ~ share)) %>% 
  group_by(wbcode, wbname) %>% 
  arrange(-rank, .by_group = TRUE) %>% 
  mutate(group = seq(1, n())) %>% 
  ungroup()

group_assign <- here("datasets", "particles", "density.csv") %>% 
  read_csv() %>% 
  slice(rep(1:n(), each = 5)) %>% 
  mutate(points = round(density / 100),
         group = rep(1:5, n() / 5)) %>% 
  left_join(groups, by = c("wbcode", "group")) %>% 
  select(city, points, group, share) %>% 
  mutate(points_ingroup = case_when(
    group == 1 ~ 0, 
    TRUE ~ round(points * share),
  )) %>% 
  group_by(city) %>% 
  mutate(points_ingroup = case_when(
    group == 1 ~ points[1] - sum(points_ingroup),
    TRUE ~ points_ingroup
  )) %>% 
  ungroup()

write_csv(group_assign, here("datasets", "particles", "group_assign.csv"))
