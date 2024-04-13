################################################################################
# title: Tidy Lightning Data
# author: Mehmet Göktuğ Öztürk
# project: Rethinking lightning-induced fires: Spatial variability and 
#          implications for management policies
################################################################################

# load packages ----------------------------------------------------------------
libs <- c("stringr", "purrr", "tidytable", "sf")
sapply(libs, require, character.only = TRUE) |> suppressPackageStartupMessages()

# 2016 - 2018 ------------------------------------------------------------------
# functions for binding df's
bind_df_func <- function(year) {
  
  # list files
  path <- list.files(
    paste0("./data/input/mgm/lightning_data/lightning_", year), 
    full.names = TRUE
  )
  
  # read and bind all files
  lightning_df <- path |> 
    map(\(x) readxl::read_excel(x, skip = 2)) |> 
    list_rbind() |>
    drop_na(2)
  return(lightning_df)
}

# execute functions by years
lightning_16 <- bind_df_func(2016)
lightning_17 <- bind_df_func(2017)
lightning_18 <- bind_df_func(2018)

# create a list with year df's in it
lightning_list <- list(lightning_16, lightning_17, lightning_18)

# remove unnecessary obj and free up ram
rm(lightning_16, lightning_17, lightning_18)
gc()

# convert to tidytable and remove unnecessary variable
lightning_list <- map(lightning_list, as_tidytable)
lightning_list <- lightning_list |> map(\(x) select(x, -contains("Saat UTC")))

# create colnames vector
col_name <- c(
  "date",
  "lat",
  "long",
  "storm_heigh",
  "amount_of_current",
  "type"
)

# set data.table threads
setDTthreads(7)

# tidier data
lightning_tidy <- lightning_list |> 
  map(\(x) set_names(x, col_name)) |> 
  map(\(x) separate_wider_delim(
    x, 
    date, 
    delim = " ", 
    names = c("date", "hour")
  )) |>
  map(\(x) separate_wider_delim(
    x, 
    date, 
    delim = "-", 
    names = c("year", "month", "day"), 
    cols_remove = FALSE
  )) |>
  map(\(x) separate_wider_delim(
    x, 
    hour, 
    delim = ":", 
    names = c("hour", "minute", "second")
  )) |>
  map(\(x) filter(x, type == "Yıldırım")) |> 
  map(\(x) select(
    x, 
    -c(
      amount_of_current, 
      type, 
      storm_heigh,
      second
    )
  )) |>
  map(\(x) relocate(x, hour, .after = "day"))

# read tr boundariy
tr <- read_sf("./data/output/geoBoundaries/tr_adm0.gpkg")

# convert to sf, transform to 3035, subset by tr and bind rows
lightning_sf <- lightning_tidy |>
  map(\(x) st_as_sf(x, coords = c("long", "lat"), crs = "EPSG:4326")) |>
  map(\(x) st_transform(x, "EPSG:3035")) |> 
  map(\(x) x[tr, ]) |>
  bind_rows()

# get coords
x <- st_coordinates(lightning_sf)[, "X"]
y <- st_coordinates(lightning_sf)[, "Y"]

# create data.table
lightning_dt <- tidytable(st_drop_geometry(lightning_sf), x, y)

# remove unnecessary obj
rm(lightning_list, lightning_tidy)
gc()

# 2019 - 2022 ------------------------------------------------------------------

# read data
lg <- fread("./data/input/mgm/lightning_data/lightning_19_22.txt", sep = " ")

# tidier data
lg <- lg |> 
  mutate(y = as.numeric(str_sub(V2, 13, 22)),
         V2 = str_sub(V2, 1, 12),
         V1 = paste(V1, V2, sep = " ")) |>
  mutate(
    V6 = case_when(
      V6 == "Y\xfdld\xfdr\xfdm" ~ "lightning"
    )
  ) |>
  filter(V6 == "lightning") |>
  select(-c(V2, V4, V5, V6)) |>
  rename(
    date = V1,
    lat = y,
    long = V3,
  ) |>
  separate_wider_delim(
    date, 
    delim = " ", 
    names = c("date", "hour")
  ) |>
  separate_wider_delim(
    date, 
    delim = "-", 
    names = c("year", "month", "day"), 
    cols_remove = FALSE
  ) |>
  separate_wider_delim(
    hour, 
    delim = ":", 
    names = c("hour", "minute", "second")
  ) |>
  select(-second) |> 
  relocate(lat, .after = "date") |>
  relocate(year, .after = "long") |>
  relocate(month, .after = "year") |>
  relocate(day, .after = "month") |>
  relocate(hour, .after = "day") |>
  relocate(minute, .after = "hour")

# convert to sf, transform to 3035, subset by tr and bind rows
lg_sf <- lg |>
  st_as_sf(coords = c("long", "lat"), crs = "EPSG:4326") |>
  st_transform("EPSG:3035")

# subset lightning data by tr boundaries
lg_subset <- lg_sf[tr, ]

# remove unnecessary obj
rm(lg, lg_sf)
gc()

# get coords
x <- st_coordinates(lg_subset)[, "X"]
y <- st_coordinates(lg_subset)[, "Y"]

# create data.table
lg_dt <- tidytable(st_drop_geometry(lg_subset), x, y)

# merge all --------------------------------------------------------------------
lightning <- rbind(lightning_dt, lg_dt)
lightning_sf <- rbind(lightning_sf, lg_subset)

# write files
fwrite(lightning, "./data/output/mgm/lightning.csv")
write_sf(lightning_sf, "./data/output/mgm/lightning.gpkg")
