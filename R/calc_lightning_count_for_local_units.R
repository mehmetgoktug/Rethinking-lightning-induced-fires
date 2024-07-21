################################################################################
# title: Calculate Lightning Count for Local Units
# author: Mehmet Göktuğ Öztürk
# project: Rethinking lightning-induced fires: Spatial variability and 
#          implications for management policies
################################################################################

# load packages-----------------------------------------------------------------
libs <- c(
  "dplyr",
  "data.table",
  "sf",
  "geos",
  "stars"
)
sapply(libs, require, character.only = TRUE) |> suppressPackageStartupMessages()

# read lightning data and convert to sf after that convert to data.table with 
# geos column for faster spatial processing ------------------------------------
# read lightning data
lightning <- fread("./data/output/mgm/lightning.csv")

# convert to sf
lightning_sf <- st_as_sf(lightning, coords = c("x", "y"), crs = "EPSG:3035")

# convert to dt and name the geometry column as geometry
lightning_dt <- data.table(lightning, as_geos_geometry(lightning_sf))
names(lightning_dt)[ncol(lightning_dt)] <- "geometry" 

# remove unnecessary obj and free up ram
rm(lightning)
gc()

# read and transform polygon vector data----------------------------------------
# read tr boundary
tr <- read_sf("./data/output/geoBoundaries/tr_adm0.gpkg")
ois <- read_sf("./data/output/ogm/boundaries/ois.gpkg")

# convert to dt and name the geometry column as geometry
dat_dt <- data.table(st_drop_geometry(ois),
                     as_geos_geometry(ois))
names(dat_dt)[ncol(dat_dt)] <- "geometry"

# free up ram
gc()

# create directory to write
dir.create(
  "./data/output/calc_lightning_count_ois/", 
  recursive = TRUE,
  showWarnings = FALSE
)

# count points into the polygons------------------------------------------------
# subset of lightning data according to i -year- for each year 

for (i in 2016:2022) {
  lightning_year <- lightning_dt[year == i,]
  
  # count points into the polygons
  dat_dt$count_point <- lengths(
    geos_intersects_matrix(dat_dt, lightning_year)
  )
  
  # convert to sf
  dat_sf <- st_as_sf(dat_dt)
  
  # write data
  write_sf(
    dat_sf, paste0(
      "./data/output/calc_lightning_count_ois/lg_grd", 
      "_year_", 
      i, 
      ".gpkg"
    )
  )
}

