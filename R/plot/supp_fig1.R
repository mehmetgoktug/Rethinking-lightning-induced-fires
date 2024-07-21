################################################################################
# title: Supplement Figure 2
# author: Mehmet Göktuğ Öztürk
# project: Rethinking lightning-induced fires: Spatial variability and 
#          implications for management policies
################################################################################

# load packages ----------------------------------------------------------------
library(tidyverse)
library(sf)

# read data --------------------------------------------------------------------
obm <- read_sf("./data/output/ogm/boundaries/obm.gpkg")
oim <- read_sf("./data/output/ogm/boundaries/oim.gpkg")
ois <- read_sf("./data/output/ogm/boundaries/ois.gpkg")

# OBM MAP ----------------------------------------------------------------------
# select only obm's
obm2 <- obm |>
	slice(1:28) |>
  mutate(obm = str_to_title(obm))

# plot
obm_plot <- ggplot() +
	geom_sf(data = obm) +
  geom_sf(data = obm2) +
  geom_sf_label(data = obm2, label = obm2$obm, size = 2.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_sf(crs = "EPSG:3857") +
  theme_minimal() +
  theme(
    panel.background = element_rect(colour = "white"),
    plot.background = element_rect(colour = "white"),
    text = element_text(family = "Ubuntu Mono", size = 16)
  )

# save plot
ggsave(
  filename = "./figs/obm.png",
  width = 10,
  height = 6,
  dpi = 600,
  device = "png",
  obm_plot
)

# OIM MAP ----------------------------------------------------------------------
# plot
oim_plot <- ggplot() +
	geom_sf(data = oim) +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_sf(crs = "EPSG:3857") +
  theme_minimal() +
  theme(
    panel.background = element_rect(colour = "white"),
    plot.background = element_rect(colour = "white"),
    text = element_text(family = "Ubuntu Mono", size = 16)
  )

# save plot
ggsave(
  filename = "./figs/oim.png",
  width = 10,
  height = 6,
  dpi = 600,
  device = "png",
  oim_plot
)

# OIS MAP ----------------------------------------------------------------------
# plot
ois_plot <- ggplot() +
	geom_sf(data = ois) +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_sf(crs = "EPSG:3857") +
  theme_minimal() +
  theme(
    panel.background = element_rect(colour = "white"),
    plot.background = element_rect(colour = "white"),
    text = element_text(family = "Ubuntu Mono", size = 16)
  )

# save plot
ggsave(
  filename = "./figs/ois.png",
  width = 10,
  height = 6,
  dpi = 600,
  device = "png",
  ois_plot
)
