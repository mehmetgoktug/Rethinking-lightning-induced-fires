################################################################################
# title: Figure 1c 
# author: Mehmet Göktuğ Öztürk 
# project: Rethinking lightning-induced fires: Spatial variability and 
#          implications for management policies
################################################################################

# load packages ----------------------------------------------------------------
libs <- c(
  "dplyr",
  "ggplot2",
  "sf"
)
sapply(libs, require, character.only = TRUE) |> suppressPackageStartupMessages()

# read data and wrangle them ---------------------------------------------------
fire_sf <- read_sf("./data/output/ogm/fire_data/fire_tidy2.gpkg")
ois <- read_sf("./data/output/ogm/boundaries/ois.gpkg")

fire_cent <- st_centroid(fire_sf)

# count points into the polygons -----------------------------------------------
ois$count_all_point <- lengths(
  st_intersects(ois, fire_cent)
)

# calculate area and scale data ------------------------------------------------
ois$area <- st_area(ois) |> units::set_units("km2")
ois$scaled_value <- ois$count_all_point / ois$area

# plot -------------------------------------------------------------------------
# create palette for map
ois_brks <- seq(0, 0.5, 0.05)
ois_cols <- c(
  "#5e4fa2", "#3288bd", "#66c2a5", "#abdda4",
  "#e6f598", "#ffffbf", "#fee08b", "#fdae61",
  "#f46d43", "#d53e4f", "#9e0142"
)

ois_palette <- colorRampPalette(ois_cols)(10)

plot <- ggplot() +
  geom_sf(
    data = ois,
    aes(fill = as.numeric(scaled_value)),
    colour = "grey50",
    linewidth = .1
  ) +
  scale_fill_gradientn(
    colours = ois_palette,
    breaks = ois_brks,
    limits = c(min(ois_brks), max(ois_brks)),
    oob = scales::squish,
    na.value = "grey50"
  ) +
  guides(fill = guide_colorsteps(
    barwidth = 20,
    barheight = .5,
    title.position = "right"
  )) +
  labs(
    #   title = "Lightning-Induced Fire Ratio by OIS Boundaries",
    fill = "total fire count / km^2",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(crs = 3857) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(margin = margin(l = 10, r = 10, unit = "pt")),
    plot.background = element_rect("white", colour = "white"),
    text = element_text(family = "ubuntu mono", size = 16)
  )

# save plot
ggsave(
  filename = "./figs/fig1c.png",
  width = 10,
  height = 6,
  dpi = 600,
  device = "png",
  plot
)
