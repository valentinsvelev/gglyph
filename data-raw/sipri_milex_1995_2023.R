# --- Load packages --- #
library(tidyverse)
library(readxl)

# --- Setup --- #
set.seed(42)
options(scipen = 999)

# --- Load the data --- #
sipri_dat <- read_xlsx(
  path = "SIPRI-Milex-data-1948-2023.xlsx",
  sheet = "Current US$",
  skip = 5,
  col_names = TRUE
  ) %>%
  drop_na(3) %>%
  dplyr::select(-Notes) %>%
  mutate(across(everything(), ~ na_if(., "..."))) %>%
  mutate(across(!Country, ~ as.numeric(.))) %>%
  pivot_longer(
    cols = -Country,
    names_to = "Year",
    values_to = "Spending",
  )

# --- Wrangle the data --- #
sipri_dat_subset <- sipri_dat %>%
  filter(
    Country %in% c("China", "Russia", "India", "Saudi Arabia", "Iran", "Ukraine"),
    Year %in% c(1995, 1999, 2003, 2007, 2011, 2015, 2019, 2023)
  ) %>%
  mutate(
    Year = as.numeric(Year),
    Country = case_when(Country == "Saudi Arabia" ~ "KSA", TRUE ~ Country)
  )

country_pairs <- expand.grid(
  Year  = c(1995, 1999, 2003, 2007, 2011, 2015, 2019, 2023),
  Country1 = unique(sipri_dat_subset$Country),
  Country2 = unique(sipri_dat_subset$Country)
  ) %>%
  filter(Country1 != Country2) %>%
  rowwise() %>%
  filter(as.character(Country1) < as.character(Country2))

sipri_milex_1995_2023 <- left_join(country_pairs, sipri_dat_subset, by = c("Country1" = "Country", "Year")) %>%
  dplyr::rename(Spending_Country1 = Spending) %>%
  left_join(sipri_dat_subset, by = c("Country2" = "Country", "Year")) %>%
  dplyr::rename(Spending_Country2 = Spending) %>%
  filter(Spending_Country1 > 5000 & Spending_Country2 > 5000) %>%
  mutate(
    from = case_when(
      Spending_Country1 > Spending_Country2 ~ Country1,
      Spending_Country1 < Spending_Country2 ~ Country2
    ),
    to = case_when(
      Spending_Country1 > Spending_Country2 ~ Country2,
      Spending_Country1 < Spending_Country2 ~ Country1
    )) %>%
  dplyr::rename(group = Year) %>%
  dplyr::select(from, to, group)

# --- Write to .rda --- #
save(sipri_milex_1995_2023, file = "sipri_milex_1995_2023.rda")
