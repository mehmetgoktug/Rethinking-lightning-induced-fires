################################################################################
# title: Edit Forest Administrative Boundary Data
# author: Mehmet Göktuğ Öztürk
# project: Rethinking lightning-induced fires: Spatial variability and 
#          implications for management policies
################################################################################

# load packages ----------------------------------------------------------------
library(dplyr)
library(sf)

# function of convert to turkish character -------------------------------------
tr_char <- function(data) {
  if (is.data.frame(data)) {
    data <- data |>
      mutate(
        across(
          where(is.character),
          ~ stringi::stri_trans_general(
            stringi::stri_trans_tolower(.x, "tr-TR"), "Latin-ASCII"
          )
        )
      )
  } else {
    data <- stringi::stri_trans_general(
      stringi::stri_trans_tolower(data, "tr-TR"), "Latin-ASCII"
    )
  }
  return(data)
}

# read layers and wrangle them -------------------------------------------------
obm <- read_sf("./data/input/ogm/boundaries/obm.gpkg")
oim <- read_sf("./data/input/ogm/boundaries/oim.gpkg")
ois <- read_sf("./data/input/ogm/boundaries/ois.gpkg")

# edit colnames and strings
obm <- obm |>
  select(BOLGE_ADI) |>
  rename(obm = BOLGE_ADI) |>
  mutate(across(where(is.character), tr_char)) |>
  st_transform("EPSG:3035")

oim <- oim |>
  select(BOLGE_ADI, ISLETME_AD) |>
  rename(
    obm = BOLGE_ADI,
    oim = ISLETME_AD
  ) |>
  rename_with(tr_char) |>
  mutate(across(where(is.character), tr_char)) |> 
  st_transform("EPSG:3035")

ois <- ois |>
  select(BOLGE_ADI, ISLETME_AD, SEFLIK_ADI) |>
  rename(
    obm = BOLGE_ADI,
    oim = ISLETME_AD,
    ois = SEFLIK_ADI
  ) |>
  rename_with(tr_char) |>
  mutate(across(where(is.character), tr_char)) |>
  st_transform("EPSG:3035")

# edit a value
ois[ois$obm == "canakkale" & ois$ois == "yenice", ]$oim <- "yenicecanakkale"

# create dir and write layers --------------------------------------------------
dir.create("./data/output/ogm/boundaries")

# write layers
write_sf(obm, "./data/output/ogm/boundaries/obm.gpkg")
write_sf(oim, "./data/output/ogm/boundaries/oim.gpkg")
write_sf(ois, "./data/output/ogm/boundaries/ois.gpkg")
