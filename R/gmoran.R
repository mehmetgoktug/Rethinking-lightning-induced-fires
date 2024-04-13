################################################################################
# title: Calculate Global Moran's I
# author: Mehmet Göktuğ Öztürk
# project: Rethinking lightning-induced fires: Spatial variability and 
#          implications for management policies
################################################################################

# load packages ----------------------------------------------------------------
libs <- c(
	"dplyr",
	"sf",
  "spdep"
)
sapply(libs, require, character.only = TRUE) |> suppressPackageStartupMessages()

# read data and wrangle them ---------------------------------------------------
fire_sf <- read_sf("./data/output/ogm/fire_data/fire_tidy2.gpkg")
ois <- read_sf("./data/output/ogm/boundaries/ois.gpkg")

fire_cent <- st_centroid(fire_sf)
fire_lightning <- fire_cent[fire_cent$primary_cause == "lightning", ]

# count points into the polygons and calculate percentage ----------------------
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

# remove unnecessaries
ois2 <- na.omit(ois)
ois2 <- ois2[-c(488, 754), ] # remove 2 regions with no links

# calculate global moran's i ---------------------------------------------------
set.seed(111)
nb <- poly2nb(ois2, queen = TRUE)
nbw <- nb2listw(nb, style = "W", zero.policy = TRUE)
gmoran <- moran.test(
	ois2$ratio_all,
	nbw,
	alternative = "greater",
	zero.policy = TRUE
)

# write results ----------------------------------------------------------------
sink("./gmoran.txt")
print(gmoran)
sink()
