# Load packages
library(tidyverse)
library(readxl)
library(writexl)

# Setup
set.seed(42)
options(scipen = 999)

# Data
sipri_dat <- read_xlsx(
  path = "Data/SIPRI-Milex-data-1948-2023.xlsx",
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

sipri_dat_subset <- sipri_dat %>%
  filter(
    Country %in% c("China", "Russia", "India", "Saudi Arabia", "Iran", "Ukraine"),
    Year %in% c(1995, 1999, 2003, 2007, 2011, 2015, 2019, 2023)
  ) %>%
  mutate(
    Year = as.numeric(Year),
    Country = case_when(Country == "Saudi Arabia" ~ "KSA", TRUE ~ Country)
  )

write_xlsx(pisa2022_subset, "Data/SIPRI_MilEx_1949_2023_subset.xlsx")
