# Load packages
library(tidyverse)
library(readr)
library(haven)
library(writexl)

# Setup
set.seed(42)
options(scipen = 999)

# Data
con <- unz("Data/13382904.zip", "CY08MSP_STU_QQQ.sav")
pisa2022 <- read_sav(con)
closeAllConnections()

pisa2022_subset <- pisa2022 %>%
  dplyr::select(CNT, MISCED, FISCED, HISCED, matches("PV[1-9]MATH|PV10MATH"))

write_xlsx(pisa2022_subset, "Data/PISA_2022_subset.xlsx")
