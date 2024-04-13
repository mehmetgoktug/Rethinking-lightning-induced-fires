################################################################################
# title: Calculate Local Moran's I - Figure 5
# author: Mehmet Göktuğ Öztürk 
# project: Rethinking lightning-induced fires: Spatial variability and 
#          implications for management policies
################################################################################

# load packages ----------------------------------------------------------------
libs <- c(
	"dplyr",
	"ggplot2",
	"sf",
  "spdep"
)
sapply(libs, require, character.only = TRUE) |> suppressPackageStartupMessages()

# read data and wrangle them ---------------------------------------------------
fire_sf <- read_sf("./data/output/ogm/fire_data/fire_tidy2.gpkg")
ois <- read_sf("./data/output/ogm/boundaries/ois.gpkg")

fire_cent <- st_centroid(fire_sf)
fire_lightning <- fire_cent[fire_cent$primary_cause == "lightning", ]

# count points into the polygons------------------------------------------------
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

# calculate lisa ---------------------------------------------------------------
set.seed(111)
nb <- poly2nb(ois2, queen = TRUE)
nbw <- nb2listw(nb, style = "W", zero.policy = TRUE)
lisa <- localmoran(
  ois2$ratio_all, nbw, alternative = "greater", zero.policy = TRUE
)

ois2$lisa_q <- attr(lisa, "quadr")[, 1]
ois2$lisa_p <- lisa[, 5]
ois2_subset <- ois2 |> filter(lisa_p < 0.05)

# plot -------------------------------------------------------------------------
l_plot <- ggplot() + 
  geom_sf(data = ois2_subset, aes(fill = lisa_q), color = "grey50") +
  geom_sf(data = ois, aes(), fill = NA) + 
  scale_fill_manual(
    values = c(
      "High-High" = "red", 
      "Low-Low" = "blue", 
      "Low-High" = "lightblue"
    )
  ) +
  labs(
    fill = "LISA Values",
    x = "Longitude",
    y = "Latitude"
  ) + 
  coord_sf(crs = 3857) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect("white", colour = "white"),
    text = element_text(family = "ubuntu mono", size = 16)
  )

# save plot
ggsave(
  "./figs/fig5.png", 
  plot = l_plot, 
  width = 10, 
  height = 6, 
)
