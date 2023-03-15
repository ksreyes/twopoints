# Extract Nobel laureates data from Nobel API
# 2 March 2023

library(here)
library(tidyverse)
library(tidyjson)
library(lubridate)
library(httr)

prizes <- httr::GET("https://api.nobelprize.org/2.1/nobelPrizes",
  query = list(limit = 1000)
) %>%
  httr::content() %>%
  magrittr::use_series(nobelPrizes) %>%
  spread_values(
    year = jstring(awardYear),
    category = jstring(category, en),
  ) %>%
  enter_object(laureates) %>%
  gather_array() %>%
  spread_values(id = jstring(id)) %>%
  as_tibble()

laureates <- httr::GET("https://api.nobelprize.org/2.1/laureates",
  query = list(limit = 1000)
) %>%
  httr::content() %>%
  magrittr::use_series(laureates) %>%
  spread_values(
    id = jstring(id),
    name = jstring(knownName, en),
    sex = jstring(gender),
    birth = jstring(birth, date),
    birth_country = jstring(birth, place, countryNow, en),
    birth_continent = jstring(birth, place, continent, en)
  ) %>%
  as_tibble()

nobel <- prizes %>%
  left_join(laureates, by = "id") %>%
  drop_na(name)

# Manually enter missing birth country
nobel$birth_country[nobel$name == "Abdulrazak Gurnah"] <- "Tanzania"
nobel$birth_continent[nobel$name == "Abdulrazak Gurnah"] <- "Africa"

# Manually enter missing birthdates ("YEAR-00-00")
nobel$birth[nobel$name == "A. Michael Spence"] <- "1943-11-7"
nobel$birth[nobel$name == "Venkatraman Ramakrishnan"] <- "1952-4-1"
nobel$birth[nobel$name == "Saul Perlmutter"] <- "1959-9-22"
nobel$birth[nobel$name == "Paul M. Romer"] <- "1955-11-6"
nobel$birth[nobel$name == "Nadia Murad"] <- "1993-3-10"
nobel$birth[nobel$name == "Abdulrazak Gurnah"] <- "1948-12-20"
nobel$birth[nobel$name == "Dmitry Muratov"] <- "1961-10-29"
nobel$birth[nobel$name == "Ardem Patapoutian"] <- "1967-10-2"
nobel$birth[nobel$name == "Morten Meldal"] <- "1954-1-16"

# Set unknown birthdates as June 30
nobel$birth[nobel$name == "Michael Houghton"] <- "1949-6-30"
nobel$birth[nobel$name == "Albert Lutuli"] <- "1898-6-30"
nobel$birth[nobel$name == "David Card"] <- "1956-6-30"

# Finishing touches
nobel <- nobel %>%
  mutate(
    birth = ymd(birth),
    age = as.numeric(as.Date(paste0(year, "-12-10")) - birth) %/% 365.25,
    category = case_when(
      category == "Physiology or Medicine" ~ "Medicine",
      category == "Economic Sciences" ~ "Economics",
      TRUE ~ category
    )
  ) %>%
  select(year, category, name, sex, birth, age, birth_country, birth_continent)

write_csv(nobel, here::here("datasets", "nobel", "nobel.csv"))
