################################################################################
# title: Table 2 - Supplement Table 1
# author: Mehmet Göktuğ Öztürk
# project: Rethinking lightning-induced fires: Spatial variability and 
#          implications for management policies
################################################################################

# load libraries ---------------------------------------------------------------
libs <- c("readr", "dplyr")
sapply(libs, require, character.only = TRUE) |> suppressPackageStartupMessages()

# read fire data ---------------------------------------------------------------
fire <- read_csv("./data/output/ogm/fire_data/fire_tidy2.csv")

# create categorical data for burnt area ---------------------------------------
fire <- fire  |> 
  mutate(burnt_area_category = cut(
    burnt_area,
    breaks = c(0, 1, 10, 100, 1000, 10000, 100000),
    labels = c(
      "0-1", "1-10", "10-100", "100-1000", "1000-10000", "10000-100000"
    ),
    include.lowest = TRUE)
  )

# create tables ----------------------------------------------------------------
# create table for the whole Turkiye
turkey_overall <- fire |>
  group_by(primary_cause, burnt_area_category) |>
  summarise(
    number_of_fires = n(),
    total_burnt_area = sum(burnt_area)
  ) |>
  ungroup() |> 
  mutate(
    fires = round(number_of_fires / sum(number_of_fires) * 100, 2),
    area = round(total_burnt_area / sum(total_burnt_area) * 100, 2)
  ) |> 
  mutate(region = "Türkiye") |> 
  relocate(region, .before = "primary_cause") |> 
  select(-number_of_fires, -total_burnt_area) 

turkey_total <- fire |>
  group_by(burnt_area_category) |>
  summarise(
    number_of_fires = n(),
    total_burnt_area = sum(burnt_area)
  ) |>
  ungroup() |> 
  mutate(
    fires = round(number_of_fires / sum(number_of_fires) * 100, 2),
    area = round(total_burnt_area / sum(total_burnt_area) * 100, 2)
  ) |> 
  mutate(region = "Türkiye", primary_cause = "total") |> 
  relocate(primary_cause, .before = "burnt_area_category") |> 
  relocate(region, .before = "primary_cause") |> 
  select(-number_of_fires, -total_burnt_area) 

turkey_overall <- rbind(turkey_overall, turkey_total)

# create table for the specified regions
# table func for specified regions
region_func <- function(region) {
  region_specific <- fire |>
    filter(obm == region) |>
    group_by(obm, primary_cause, burnt_area_category) |>
    summarise(
      number_of_fires = n(),
      total_burnt_area = sum(burnt_area)
    ) |>
    ungroup() |> 
    mutate(
      fires = round(number_of_fires / sum(number_of_fires) * 100, 2),
      area = round(total_burnt_area / sum(total_burnt_area) * 100, 2)
    ) |>
    select(-number_of_fires, -total_burnt_area) |> 
    rename(region = obm) 
  
  return(region_specific)
} 

region_total_func <- function(region) {
  region_specific_total <- fire |>
    filter(obm == region) |>
    group_by(obm, burnt_area_category) |>
    summarise(
      number_of_fires = n(),
      total_burnt_area = sum(burnt_area)
    ) |>
    ungroup() |> 
    mutate(
      fires = round(number_of_fires / sum(number_of_fires) * 100, 2),
      area = round(total_burnt_area / sum(total_burnt_area) * 100, 2)
    ) |>
    mutate(primary_cause = "total") |> 
    select(-number_of_fires, -total_burnt_area) |> 
    rename(region = obm) |> 
    relocate(primary_cause, .before = "burnt_area_category") |> 
    relocate(region, .before = "primary_cause")
  
  return(region_specific_total)  
}

# execute function and merge lists
specified_regions <- c(
  "izmir", "mugla", "istanbul", "bolu", "erzurum", "elazig"
)
region_specific_list <- lapply(specified_regions, region_func)
region_specific <- do.call(rbind, region_specific_list)

region_specific_total_list <- lapply(specified_regions, region_total_func)
region_specific_total <- do.call(rbind, region_specific_total_list)

region_specific <- rbind(region_specific, region_specific_total)

# merge all tables -------------------------------------------------------------
table_binded <- rbind(turkey_overall, region_specific)

# write tables -----------------------------------------------------------------
write_csv(table_binded, "table2.csv")
