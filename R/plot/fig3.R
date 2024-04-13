################################################################################
# title: Figure 3
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

r1 <- fire |> 
	mutate(across(
		obm, ~factor(., levels = c("izmir", "mugla", "istanbul")
		)
	)) |>
  filter(obm %in% c("izmir", "mugla", "istanbul")) |>
    group_by(month, primary_cause, obm) |>
  summarise(n = n()) |>
  ungroup() |>
  mutate(primary_cause = fct_reorder(
           as.character(primary_cause), n, .desc = TRUE)
  )

r2 <- fire |> 
	mutate(across(
		obm, ~factor(., levels = c("erzurum", "bolu", "elazig")
		)
	)) |>
  filter(obm %in% c("bolu", "erzurum", "elazig")) |>
    group_by(month, primary_cause, obm) |>
  summarise(n = n()) |>
  ungroup() |>
  mutate(primary_cause = fct_reorder(
           as.character(primary_cause), n, .desc = TRUE)
  )

g1 <- ggplot(
	data = r1,
	aes(
		x = month,
		y = n, 
		fill = factor(primary_cause, levels = c("lightning", "human", "unknown"))
		)
	) +
  geom_bar(stat = "identity") +
	scale_fill_manual(values = graph_palette) +
	scale_x_continuous(breaks = seq(1, 12, by = 1)) +
	labs(
		x = "Months",
		y = "Number of Fires",
		fill = "Primary Cause",
	) +
  theme_minimal() +
  theme(
    text = element_text(family = "Ubuntu Mono", size = 16),
    title = element_text(face = "bold"),
		legend.position = "none"
  ) +
	facet_wrap( ~ obm, nrow = 1)

g2 <- ggplot(
	data = r2,
	aes(
		x = month,
		y = n, 
		fill = factor(primary_cause, levels = c("lightning", "human", "unknown"))
		)
	) +
  geom_bar(stat = "identity") +
	scale_fill_manual(values = graph_palette) +
	scale_x_continuous(breaks = seq(1, 12, by = 1)) +
	labs(
		x = "Months",
		y = "Number of Fires",
		fill = "Primary Cause",
	) +
  theme_minimal() +
  theme(
    text = element_text(family = "Ubuntu Mono", size = 16),
    title = element_text(face = "bold"),
		legend.position = "bottom"
  ) +
	facet_wrap( ~ obm, nrow = 1)

# merge plots
merge_plots <- arrangeGrob(g1, g2)

# save plot
ggsave(
  "./figs/fig3.png", 
  plot = merge_plots, 
  width = 12, 
  height = 10, 
)
