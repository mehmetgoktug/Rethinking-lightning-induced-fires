################################################################################
# title: Figure 1a
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

# read data --------------------------------------------------------------------
# list files
grd_list <- list.files(
  "./data/output/calc_lightning_count/", 
  full.names = TRUE,
  pattern = "grd"
)

# read grid data
grd_list_dat <- lapply(grd_list, read_sf)

# transform to 3857 for visualization ------------------------------------------
# read boundary of turkey
tr_il <- read_sf("./data/output/geoBoundaries/tr_adm1.gpkg") 
tr_il <- st_transform(tr_il, "EPSG:3857")

# bind columns of df's in the list, after that remove unnecessary columns,
# calculate mean for years and convert to sf object
grd_binded <- bind_cols(grd_list_dat) |>
  select(-c(4, 6, 7, 9)) |>
  mutate(count_mean = rowMeans(across(starts_with("count")))) |>
  st_as_sf() |>
  st_transform("EPSG:3857")

# create empty raster for target grid
target_grd <- st_bbox(tr_il) |> 
  st_as_sfc() |>
  st_transform("EPSG:3857") |>
  st_bbox() |>
  st_as_stars(dx = 1000, dy = 1000)

# rasterize vector
lightning_stars <- st_rasterize(grd_binded[, "count_mean"], target_grd)

# mask raster by tr boundary
lightning_stars <- st_as_stars(
  crop(rast(lightning_stars), vect(tr_il), mask = TRUE)
)

# plot -------------------------------------------------------------------------
# create breaks and color palette
grd_breaks <- seq(0, 11, 1)
grd_cols <- c(
  "#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", 
  "#e6f598", "#ffffbf", "#fee08b", "#fdae61",
  "#f46d43", "#d53e4f", "#9e0142"
)
grd_palette <- colorRampPalette(grd_cols)(11)

plot <- ggplot() +
  geom_stars(data = lightning_stars, aes()) +
  scale_fill_gradientn(
    colours = grd_palette, 
    breaks = grd_breaks, 
    na.value = NA,
    limits = c(min(grd_breaks), max(grd_breaks)), 
    label = c(grd_breaks[-12], expression(phantom(x) >=11)), 
    oob = scales::squish
  ) +
  guides(fill = guide_colorbar(
    barwidth = 20,
    barheight = .5,
    title.position = "right"
  )) +
  labs(
    #    title = "Lightning Counts by Grids",
    x = "Longitude",
    y = "Latitude",
    fill = "lightning count / km^2") +
  coord_sf(
    crs = "EPSG:3857"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(margin = margin(l = 10, r = 10, unit = "pt")),
    plot.background = element_rect("white", colour = "white"),
    text = element_text(family = "Ubuntu Mono", size = 16)
  )

# save plot
ggsave(
  filename = "./figs/fig1a.png",
  width = 10,
  height = 6,
  dpi = 600,
  device = "png",
  plot
)
