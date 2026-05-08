############################################################
# 08_macro_trends_plot.R
# 
# Purpose: Generate a macro-level comparison of YoY growth 
# in national construction activity and house prices.
# Handles mismatched start years across series.
############################################################

library(tidyverse)

# ----------------------------- 1. Load Data -----------------------------
build_raw <- read_csv("data/raw/debuild.csv", show_col_types = FALSE)
price_raw <- read_csv("data/raw/deprice.csv", show_col_types = FALSE)

# ----------------------------- 2. Data Transformation -----------------------------

# Compute YoY changes for Construction (keeps all available years)
build_yoy <- build_raw %>%
  arrange(year) %>%
  mutate(construction = 100 * (construction_value / lag(construction_value) - 1)) %>%
  select(year, construction)

# Compute YoY changes for Prices (keeps all available years)
price_yoy <- price_raw %>%
  arrange(year) %>%
  mutate(prices = 100 * (price_value / lag(price_value) - 1)) %>%
  select(year, prices)

# Merge datasets using full_join to preserve years where only one series exists
df_macro <- full_join(build_yoy, price_yoy, by = "year") %>%
  arrange(year) %>%
  # Filter to start only when we have at least one valid YoY calculation
  filter(!is.na(construction) | !is.na(prices))

# ----------------------------- 3. Visualization -----------------------------

macro_plot <- df_macro %>%
  pivot_longer(-year, names_to = "series", values_to = "yoy") %>%
  ggplot(aes(x = year, y = yoy, color = series, linetype = series)) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "black", alpha = 0.5) +
  geom_line(linewidth = 1, na.rm = TRUE) + # na.rm = TRUE prevents gaps in lines
  scale_color_manual(
    values = c("construction" = "#2c3e50", "prices" = "#e74c3c"),
    labels = c("Construction Volume", "Residential Price Index")
  ) +
  scale_linetype_manual(
    values = c("construction" = "solid", "prices" = "dashed"),
    labels = c("Construction Volume", "Residential Price Index")
  ) +
  labs(
    title = "National Trends in Housing Supply and Valuation",
    subtitle = "Year-over-Year percentage change (Germany)",
    x = "Year",
    y = "Growth Rate (%)",
    color = "Indicator",
    linetype = "Indicator"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

# ----------------------------- 4. Export -----------------------------

if(!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)

ggsave(
  filename = "output/figures/macro_price_vs_build.pdf",
  plot = macro_plot,
  width = 8,
  height = 5,
  device = "pdf"
)