############################################################
# 06_figures_tables.R
#
# Purpose: Produce all thesis visualizations
# 1. Descriptive: Price Index & Density Distribution
# 2. Main Heatmaps: Including COVID (Red Palette)
# 3. Appendix Heatmaps: Excluding COVID (Blue Palette)
############################################################

library(tidyverse)
library(patchwork)
library(ggrepel)
library(scales)

# ----------------------------- 1. Load Data -----------------------------
# Using the processed global panel for descriptives and main plots
df_global_incl <- read_csv("data/processed/superstar_panel_global.csv", show_col_types = FALSE)
df_period_incl <- read_csv("data/processed/superstar_panel_period.csv", show_col_types = FALSE)

# Generate 'Excl' samples for the Appendix
df_global_excl <- df_global_incl %>% filter(year < 2020)
df_period_excl <- df_period_incl %>% filter(year < 2020)

# ----------------------------- 2. Descriptive Plots -----------------------------

# Plot A: Price Index Time Series
p_index <- ggplot(df_global_incl, aes(x = year, y = value, color = city)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_text_repel(
    data = df_global_incl %>% group_by(city) %>% filter(year == max(year)),
    aes(label = city), size = 3, nudge_x = 2, direction = "y", segment.size = 0.2
  ) +
  scale_x_continuous(breaks = seq(1995, 2025, by = 5), expand = expansion(mult = c(0.05, 0.25))) +
  labs(title = "Housing Price Index by City", x = "Year", y = "Price Index", caption = "Data: GREIX") +
  theme_minimal(base_size = 11) + 
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

# Plot B: Density Shift (1995 vs 2020)
p_density <- df_global_incl %>%
  filter(year %in% c(1995, 2020)) %>%
  mutate(year = factor(year)) %>%
  ggplot(aes(x = value, fill = year, color = year)) +
  geom_density(alpha = 0.4, linewidth = 0.8) +
  scale_x_log10(labels = label_number(big.mark = ",")) +
  scale_fill_manual(values = c("1995" = "#69b3a2", "2020" = "#404080")) +
  scale_color_manual(values = c("1995" = "#69b3a2", "2020" = "#404080")) +
  labs(title = "Shift in Price Distribution", x = "Mean House Value (Log Scale)", y = "Density") +
  theme_minimal(base_size = 11) + 
  theme(legend.position = "top", legend.title = element_blank(), plot.title = element_text(face = "bold"))

# ----------------------------- 3. Heatmap Helper -----------------------------

build_heatmap <- function(data, ver_label, color_hex) {
  data %>%
    filter(!is.na(growth_window), growth_window != "1995-1999") %>%
    ggplot(aes(x = factor(growth_window), y = fct_rev(city), fill = factor(superstar))) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_manual(values = c("0" = "#eeeeee", "1" = color_hex), labels = c("Standard", "Superstar")) +
    labs(title = ver_label, x = "Five-Year Period", y = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 7),
      panel.grid = element_blank(),
      legend.position = "none"
    )
}

# ----------------------------- 4. Assemble & Export -----------------------------

if(!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)

# 4a. Export Descriptives
ggsave("output/figures/desc_index.pdf", p_index, width = 9, height = 6)
ggsave("output/figures/desc_density.pdf", p_density, width = 8, height = 5)

# 4b. Main Heatmaps (Red)
combined_main <- (build_heatmap(df_global_incl, "Static Classification", "#d73027") | 
                    build_heatmap(df_period_incl, "Period-Specific Classification", "#d73027")) +
  plot_annotation(title = "Superstar Classification", tag_levels = "A")

ggsave("output/figures/heatmaps_main.pdf", combined_main, width = 10, height = 7)

# 4c. Appendix Heatmaps (Blue)
combined_app <- (build_heatmap(df_global_excl, "Static Classification", "#2c7fb8") | 
                   build_heatmap(df_period_excl, "Period-Specific Classification", "#2c7fb8")) +
  plot_annotation(title = "Superstar Classification (Excluding COVID)", tag_levels = "A")

ggsave("output/figures/heatmaps_appendix.pdf", combined_app, width = 10, height = 7)

message("Success: All thesis figures packed into output/figures/")