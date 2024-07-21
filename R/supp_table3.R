################################################################################
# title: Supplement Table 3
# author: Mehmet Göktuğ Öztürk
# project: Rethinking lightning-induced fires: Spatial variability and 
#          implications for management policies
################################################################################

# load packages ----------------------------------------------------------------
library(tidyverse)

# read fire data ---------------------------------------------------------------
fire <- read_csv("./data/output/ogm/fire_data/fire_tidy2.csv")

# create table -----------------------------------------------------------------
fire_ratio_ois <- fire |> 
  group_by(ois, obm, primary_cause) |> 
  count() |> 
  group_by(ois, obm) |> 
  summarise(
    total_fires = sum(n),
    lightning_fires = sum(n[primary_cause == "lightning"]),
    human_fires = sum(n[primary_cause == "human"]),
    unknown_fires = sum(n[primary_cause == "unknown"]),
    lightning_fire_ratio = round(lightning_fires * 100 / total_fires),
    human_fire_ratio = round(human_fires * 100 / total_fires),
    unknown_fire_ratio = round(unknown_fires * 100 / total_fires)
  ) |> 
  select(-lightning_fires, -human_fires, -unknown_fires) |> 
  filter(total_fires > 9) |> 
  arrange(desc(lightning_fire_ratio)) |> 
  head(10)

# write table ------------------------------------------------------------------
write_csv(fire_ratio_ois, "supp_table3.csv")
