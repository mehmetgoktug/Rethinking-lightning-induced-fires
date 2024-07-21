################################################################################
# title: Create Data to Model The Association of LIW's and Lightnings
# author: Mehmet Göktuğ Öztürk
# project: Rethinking lightning-induced fires: Spatial variability and 
#          implications for management policies
################################################################################

# load packages-----------------------------------------------------------------
libs <- c(
  "dplyr",
  "sf",
  "ggplot2",
  "stars",
  "terra"
)
sapply(libs, require, character.only = TRUE) |> suppressPackageStartupMessages()

# read lightning data ----------------------------------------------------------
# list files
lg_list <- list.files(
  "./data/output/calc_lightning_count_ois/", 
  full.names = TRUE,
  pattern = "grd"
)

# read lg data
lg_list_dat <- lapply(lg_list, read_sf)

# bind columns of df's in the list, after that remove unnecessary columns,
# calculate mean for years and convert to sf object
lg_binded <- bind_cols(lg_list_dat) |>
  select(c(1, 2, 3, 4, 5, 9, 14, 19, 24, 29, 34)) |>
  mutate(count_mean = rowMeans(across(starts_with("count")))) |>
  st_as_sf()

# calculate area of local units
lg_binded$area_km2 <- lg_binded |>
  st_area() |>
  units::set_units("km2")

# select, rename, relocate the data and transform to df 
lg_df <- lg_binded |>
  select(-c(4, 6:11)) |>
  rename(
    obm = obm...1,
    oim = oim...2,
    ois = ois...3,
    geometry = geom...5
  ) |>
  relocate(geometry, .after = "area_km2") |>
  st_drop_geometry()

# read fire and local unit data ------------------------------------------------
fires <- read_sf("./data/output/ogm/fire_data/fire_tidy2.gpkg")
ois <- read_sf("./data/output/ogm/boundaries/ois.gpkg")

# filter fire data by year
fires <- fires |> filter(year >= 2016)

# create centroids for intersection and filter LIW's
fire_cent <- st_centroid(fires)
fire_lightning <- fire_cent |>
  filter(primary_cause == "lightning")

# count points into the polygons
ois$count_all_point <- lengths(
  st_intersects(ois, fire_cent)
)
ois$count_lightning_point <- lengths(
  st_intersects(ois, fire_lightning)
)

# calculate ratio
ois <- ois |>
  mutate(
    ratio_all = round(
      count_lightning_point * 100 / count_all_point,
      digits = 1
    )
  )

# transform to df and select variables
ois_df <- st_drop_geometry(ois)
ois_df <- select(ois_df, -count_all_point)

# join lightning and LIW data
df <- left_join(ois_df, lg_df, by = join_by(obm, oim, ois))

# rename data
names(df) <- c("obm", "oim", "ois", "lightning_fire_count", "lightning_fire_ratio", "lightning_count", "area_km2")

# create direction
path <- "./data/output/data_for_model/"
if (!file.exists(path)) dir.create(path)

# write data
write_csv(df, paste0(path, "liw_lightning.csv"))
