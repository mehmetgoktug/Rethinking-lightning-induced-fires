################################################################################
# title: Supplement Figure 1
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
  group_by(month, primary_cause) |>
  summarise(n = n()) |>
  ungroup() |>
  mutate(
    primary_cause = fct_reorder(
      as.character(primary_cause), n, .desc = TRUE
    )
  )

plot <- ggplot(data = df, aes(x = month, y = n, fill = primary_cause)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = graph_palette) +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  labs(
    x = "Months",
    y = "Number of Fires",
    fill = "Cause"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect("white", colour = "white"),
    text = element_text(family = "Ubuntu Mono", size = 16),
    title = element_text(face = "bold")
  )

# save plot
ggsave(
  "./figs/fig5b.png", 
  plot = plot, 
  width = 10, 
  height = 6, 
)



