
################################################################################
# title: Figure X
# author: Mehmet Göktuğ Öztürk
# project: Rethinking lightning-induced fires: Spatial variability and 
#          implications for management policies
################################################################################

# load packages ----------------------------------------------------------------
libs <- c("readr", "dplyr", "forcats", "ggplot2", "gridExtra")
sapply(libs, require, character.only = TRUE) |> suppressPackageStartupMessages()

# read fire data ---------------------------------------------------------------
fire <- read_csv("./data/output/ogm/fire_data/fire_tidy2.csv")

# plot -------------------------------------------------------------------------
# create palette
graph_cols <- c(
  "lightning" = "#7de167", "human" = "#e16767", "unknown" = "#6776e1"
)
graph_palette <- colorRampPalette(graph_cols)(3)
names(graph_palette) <- names(graph_cols)

df <- fire |> 
  group_by(year, primary_cause) |>
  summarise(n = n()) |>
  ungroup() |>
  mutate(primary_cause = fct_reorder(
           as.character(primary_cause), n, .desc = TRUE)
  )

p <- ggplot(
	data = df,
	aes(
		x = year,
		y = n, 
		fill = factor(primary_cause, levels = c("lightning", "human", "unknown"))
		)
	) +
  geom_bar(stat = "identity") +
	scale_fill_manual(values = graph_palette) +
	scale_x_continuous(breaks = seq(2002, 2022, by = 1)) +
	labs(
		x = "Years",
		y = "Number of Fires",
		fill = "Cause",
	) +
  theme_minimal() +
  theme(
    plot.background = element_rect("white", colour = "white"),
    text = element_text(family = "Ubuntu Mono", size = 16),
    title = element_text(face = "bold"),
		axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) 

# save plot
ggsave(
  "./figs/fig5a.png", 
  plot = p, 
  width = 10, 
  height = 6, 
)
