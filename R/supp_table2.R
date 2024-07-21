################################################################################
# title: Supplement Table 2
# author: Mehmet Göktuğ Öztürk
# project: Rethinking lightning-induced fires: Spatial variability and 
#          implications for management policies
################################################################################

# load packages ----------------------------------------------------------------
libs <- c("readr", "dplyr", "tidyr")
sapply(libs, require, character.only = TRUE) |> suppressPackageStartupMessages()

# read fire data
fire <- read_csv("./data/output/ogm/fire_data/fire_tidy2.csv")

df <- fire |> 
  group_by(obm, primary_cause) |>
  summarise(n = n()) |>
  ungroup() |>
  pivot_wider(names_from = primary_cause, values_from = n)

ratios <- df |>
  group_by(obm) |>
  summarise(
    total = sum(human, lightning, unknown),
    human_ratio = round(human / total * 100, digits = 1),
    lightning_ratio = round(lightning / total * 100, digits = 1),
    unknown_ratio = round(unknown / total * 100, digits = 1)
  ) |>
  ungroup()

df_joined <- df |>
  left_join(ratios, by = "obm") |>
  rename(
    "Regional unit" = "obm",
    "Human" = "human",
    "Lightning" = "lightning",
    "Unknown" = "unknown",
    "Total" = "total",
    "Human (%)" = "human_ratio",
    "Lightning (%)" = "lightning_ratio",
    "Unknown (%)" = "unknown_ratio"
  ) |>
  relocate(Total, .before = "Human")

# write table
write_csv(df_joined, "supp_table2.csv")
