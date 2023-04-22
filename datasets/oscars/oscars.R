# OSCARS DATA
# Scrapes data on Best Picture nominees over 2000-2023 from Wikipedia, Box Office Mojo, and Metacritic
# 26 Jan 2023

rm(list = ls())

library(tidyverse)
library(here)
library(magrittr)
library(rvest)
library(ggplot2)

# Get list of nominees and their Wikipedia URLs

html <- read_html("https://en.wikipedia.org/wiki/Academy_Award_for_Best_Picture")

df <- tibble(year = NULL, title = NULL, url_wiki = NULL)

for (i in 10:12) {
  df_i <- html %>%
    html_elements("table") %>%
    extract2(i) %>%
    html_table() %>%
    drop_na() %>%
    mutate(
      year = str_sub(`Year of Film Release`, end = 4),
      url_wiki = html %>%
        html_elements("table") %>%
        extract2(i) %>%
        html_elements("i a") %>%
        html_attr("href")
    ) %>%
    select(year, title = Film, url_wiki)

  df <- bind_rows(df, df_i)

  rm(df_i)
}

# Get URLs for IMDB and Metacritic

url_imdb <- NULL
url_metacritic <- NULL

for (i in 1:nrow(df)) {
  wiki_i <- paste0("https://en.wikipedia.org/", df$url_wiki[i]) %>%
    read_html()

  url_imdb_i <- wiki_i %>%
    html_elements("ul a") %>%
    html_attr("href") %>%
    str_subset(regex("www.imdb.com/title/", ignore_case = TRUE)) %>%
    str_subset("archive|awards|locations|reviews|releaseinfo|trivia|combined|soundtrack|business|fullcredits|ref", negate = TRUE) %>%
    extract(1) %>%
    str_sub(start = 28, end = -2)

  url_metacritic_i <- wiki_i %>%
    html_elements("a") %>%
    html_attr("href") %>%
    str_subset(regex("metacritic.com/movie|metacritic.com/film/titles", ignore_case = TRUE)) %>%
    str_subset("archive", negate = TRUE) %>%
    gsub("critic-reviews", "", .) %>%
    extract(1)

  url_imdb <- c(url_imdb, url_imdb_i)
  url_metacritic <- c(url_metacritic, url_metacritic_i)

  rm(url_imdb_i, url_metacritic_i)
}

# Scape box office receipts and Metacritic scores

gross_us <- NULL
gross_int <- NULL
mc_critics <- NULL
mc_users <- NULL

for (i in 1:nrow(df)) {
  gross_i <- paste0("https://www.boxofficemojo.com/title/", url_imdb[i]) %>%
    read_html() %>%
    html_elements(".mojo-performance-summary-table .money") %>%
    html_text2() %>%
    str_sub(start = 2) %>%
    gsub(",", "", .) %>%
    as.numeric()

  gross_us <- c(gross_us, gross_i[1])
  gross_int <- c(gross_int, gross_i[2])

  mc_i <- read_html(url_metacritic[i]) %>%
    html_elements(".metascore_w.larger") %>%
    html_text2()

  mc_critics <- c(mc_critics, mc_i[1])
  mc_users <- c(mc_users, mc_i[2])

  rm(gross_i, mc_i)

  paste0(i, " done out of ", nrow(df)) %>%
    print()
}

# Consolidate and save

df1 <- df %>%
  mutate(
    gross_us = gross_us,
    gross_int = gross_int,
    gross = gross_us + gross_int,
    mc_critics = as.numeric(mc_critics),
    mc_users = as.numeric(mc_users),
    url_imdb = url_imdb,
    url_metacritic = url_metacritic
  ) %>%
  select(year, title, gross_us:mc_users, url_wiki, url_imdb, url_metacritic)

write_csv(df1, here::here("datsets", "oscars", "oscars.csv"))
