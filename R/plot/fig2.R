################################################################################
# title: Figure 2
# author: Mehmet Göktuğ Öztürk
# project: Rethinking lightning-induced fires: Spatial variability and 
#          implications for management policies
################################################################################

# load packages ----------------------------------------------------------------
libs <- c("readr", "dplyr", "forcats", "ggplot2", "gridExtra")
sapply(libs, require, character.only = TRUE) |> suppressPackageStartupMessages()

# read fire data
fire <- read_csv("./data/output/ogm/fire_data/fire_tidy2.csv")

# create palette
graph_cols <- c(
  "lightning" = "#7de167", "human" = "#e16767", "unknown" = "#6776e1"
)
graph_palette <- colorRampPalette(graph_cols)(3)
names(graph_palette) <- names(graph_cols)

# plot -------------------------------------------------------------------------
# first plot
df1 <- fire |> 
  group_by(obm, primary_cause) |>
  summarise(n = n()) |>
  ungroup() |>
  mutate(obm = fct_reorder(obm, n, .fun = sum, .desc = FALSE),
         primary_cause = fct_reorder(primary_cause, n, .desc = FALSE))

plot1 <- ggplot(data = df1, aes(x = n, y = obm)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = graph_palette) +
  labs(
    x = "Number of Fires",
    y = "Forest Regional Directorate",
    fill = "Cause"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Ubuntu Mono", size = 16),
    title = element_text(face = "bold")
  )

# second plot
total_df <- df1 |>
  group_by(obm) |>
  summarise(total = sum(n))

lightning_ratios <- df1 |>
  group_by(obm) |>
  summarise(total = sum(n),
            lightning = sum(n[primary_cause == "lightning"]),
            lightning_ratio = lightning / total) |>
  ungroup() |>
  arrange(lightning_ratio) |>
  select(obm, lightning_ratio)

df_joined <- df1 |>
  left_join(lightning_ratios, by = "obm")

plot2 <- df_joined |>
  mutate(obm = fct_reorder(obm, lightning_ratio, .desc = FALSE)) |>
  ggplot(aes(x = n, y = obm, fill = primary_cause)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(
    data = total_df,
    aes(y = obm, x = total, label = total),
    position = position_fill(vjust = 0.1),
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = graph_palette) +
  labs(
    x = "Proportion of Fire Causes",
    y = "Forest Regional Directorate",
    fill = "Cause"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Ubuntu Mono", size = 16),
    title = element_text(face = "bold")
  )

# merge plots
merge_plots <- arrangeGrob(plot1, plot2, nrow = 1)

# save plot
ggsave(
  "./figs/fig2.png", 
  plot = merge_plots, 
  width = 12, 
  height = 10, 
)
