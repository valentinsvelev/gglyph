# --- Load the packages --- #
library(tidyverse)
library(readr)
library(haven)

# --- Setup --- #
set.seed(42)
options(scipen = 999)

# --- Load the data --- #
con <- unz("13382904.zip", "CY08MSP_STU_QQQ.sav")
pisa2022 <- read_sav(con)
closeAllConnections()

# --- Define mappings --- #
# Countries
eu_countries <- c(
  AUT = "Austria",
  BEL = "Belgium",
  BGR = "Bulgaria",
  HRV = "Croatia",
  CYP = "Cyprus",
  CZE = "Czech Republic",
  DNK = "Denmark",
  EST = "Estonia",
  FIN = "Finland",
  FRA = "France",
  DEU = "Germany",
  GRC = "Greece",
  HUN = "Hungary",
  IRL = "Ireland",
  ITA = "Italy",
  LVA = "Latvia",
  LTU = "Lithuania",
  LUX = "Luxembourg",
  MLT = "Malta",
  NLD = "Netherlands",
  POL = "Poland",
  PRT = "Portugal",
  ROU = "Romania",
  SVK = "Slovakia",
  SVN = "Slovenia",
  ESP = "Spain",
  SWE = "Sweden"
)

# Education
labels <- c("ISCED <=1", "ISCED 2", "ISCED 3", "ISCED 4&5", "ISCED >=6")

isced_map <- setNames(
  c(
    rep(labels[1], 2),  # 1–2  -> ISCED <=1
    labels[2],          # 3    -> ISCED 2
    rep(labels[3], 2),  # 4–5  -> ISCED 3
    rep(labels[4], 2),  # 6–7  -> ISCED 4&5
    rep(labels[5], 3)   # 8–10 -> ISCED >=6
  ),
  as.character(1:10)
)

# --- Wrangle the data --- #
pisa2022_subset <- pisa2022 %>%
  dplyr::select(CNT, MISCED, FISCED, HISCED, matches("PV[1-9]MATH|PV10MATH")) %>%
  dplyr::mutate(MATH = rowMeans(across(matches("PV[1-9]MATH|PV10MATH")), na.rm = TRUE)) %>%
  dplyr::select(CNT, HISCED, MATH) %>%
  dplyr::filter(CNT %in% names(eu_countries)) %>%
  dplyr::mutate(group = eu_countries[CNT], edu = isced_map[HISCED])

# --- Conduct statistical analyses --- #
pisa2022_ttest_less <- pisa2022_subset %>%
  group_by(group) %>%
  summarise(
    results = list(
      pairwise.t.test(
        x = MATH,
        g = edu,
        p.adjust.method = "bonferroni",
        alternative = "less"
      )
    ),
    .groups = "drop"
  )

pisa2022_ttest_greater <- pisa2022_subset %>%
  group_by(group) %>%
  summarise(
    results = list(
      pairwise.t.test(
        x = MATH,
        g = edu,
        p.adjust.method = "bonferroni",
        alternative = "greater"
      )
    ),
    .groups = "drop"
  )

# --- Tidy up the statistical analysis results --- #
ttests_to_long <- function(ttests, group) {
  ttests %>%
    rowwise() %>%
    mutate(
      p_value_matrix = list(
        if (!is.null(results) && !is.null(results$p.value)) results$p.value else NULL
      )
    ) %>%
    filter(!is.null(p_value_matrix)) %>%
    mutate(
      tidy_results = list(as.data.frame(as.table(p_value_matrix)))
    ) %>%
    unnest(cols = tidy_results) %>%
    rename(p_value = Freq) %>%
    mutate(
      cat1 = as.character(Var1),
      cat2 = as.character(Var2)
    ) %>%
    dplyr::select(group, cat1, cat2, p_value)
}

pisa_2022 <- bind_rows(
  ttests_to_long(ttests = pisa2022_ttest_less, group = "group") %>%
    drop_na(p_value) %>%
    mutate(direction = "less"),
  ttests_to_long(ttests = pisa2022_ttest_greater, group = "group") %>%
    drop_na(p_value) %>%
    mutate(direction = "greater")
  ) %>%
  mutate(
    from = dplyr::if_else(direction == "less", cat1, cat2),
    to = dplyr::if_else(direction == "less", cat2, cat1)
  ) %>%
  dplyr::rename(sig = p_value) %>%
  dplyr::select(from, to, group, sig)

attr(pisa_2022$group, "label") <- NULL # Remove left-over label

# --- Write to .rda --- #
save(pisa_2022, file = "pisa_2022.rda")
