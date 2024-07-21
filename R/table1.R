################################################################################
# title: Summary Table for Forest Administration Boundaries
# author: Mehmet Göktuğ Öztürk
# project: Rethinking lightning-induced fires: Spatial variability and 
#          implications for management policies
################################################################################

# load packages ----------------------------------------------------------------
library(tidyverse)
library(sf)

# read and wrangle data --------------------------------------------------------
# read data
obm <- read_sf("data/output/ogm/boundaries/obm.gpkg")
oim <- read_sf("data/output/ogm/boundaries/oim.gpkg")
ois <- read_sf("data/output/ogm/boundaries/ois.gpkg")

obm <- obm |> 
  filter(!grepl("bolge", obm)) |>
  filter(!grepl("ae", obm)) |>
  mutate(area_km2 = units::set_units(st_area(geom), "km2")) |>
  relocate(area_km2, .after = "obm") |>
  st_drop_geometry()

oim <- oim |> 
  filter(!grepl("bolge", obm)) |>
  filter(!grepl("ae", obm)) |>
  mutate(area_km2 = units::set_units(st_area(geom), "km2")) |>
  relocate(area_km2, .after = "oim") |>
  st_drop_geometry()

ois <- ois |> 
  filter(!grepl("bolge", obm)) |>
  filter(!grepl("ae", obm)) |>
  mutate(area_km2 = units::set_units(st_area(geom), "km2")) |>
  relocate(area_km2, .after = "ois") |>
  st_drop_geometry()

obm_summ <- obm |> 
  mutate(adm_unit = "obm") |>
  group_by(adm_unit) |>
  summarise(
    min = min(area_km2),
    median = median(area_km2),
    mean = mean(area_km2),
    max = max(area_km2),
  )

oim_summ <- oim |> 
  mutate(adm_unit = "oim") |>
  group_by(adm_unit) |>
  summarise(
    min = min(area_km2),
    median = median(area_km2),
    mean = mean(area_km2),
    max = max(area_km2),
  )

ois_summ <- ois |> 
  mutate(adm_unit = "ois") |>
  group_by(adm_unit) |>
  summarise(
    min = min(area_km2),
    median = median(area_km2),
    mean = mean(area_km2),
    max = max(area_km2),
  )

adm_summ <- rbind(obm_summ, oim_summ, ois_summ)
adm_summ <- adm_summ |>
  mutate(across(where(is.numeric), round, 2))

# write table
write_csv(adm_summ, "adm_summ.csv")
