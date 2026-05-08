############################################################
# 06_figures_tables.R
#
# Purpose:
# Produce final figures and visualizations for the
# thesis manuscript.
#
# Inputs:
# - data/processed/superstar_panel_global.csv
# - data/processed/superstar_panel_period.csv
#
# Outputs:
# - output/figures/*.pdf
#
############################################################

############################################################
# Libraries

required_packages <- c(
  "tidyverse",
  "patchwork"
)

invisible(
  lapply(
    required_packages,
    library,
    character.only = TRUE
  )
)

############################################################
# Load Data

superstar_global <- read_csv(
  "data/processed/superstar_panel_global.csv",
  show_col_types = FALSE
)

superstar_period <- read_csv(
  "data/processed/superstar_panel_period.csv",
  show_col_types = FALSE
)

############################################################
# Plotting Helper Function

build_heatmap <- function(df, title_text) {
  
  ggplot(
    df,
    aes(
      x = factor(growth_window),
      y = fct_rev(city),
      fill = factor(superstar)
    )
  ) +
    
    geom_tile(
      color = "white",
      linewidth = 0.5
    ) +
    
    scale_fill_manual(
      values = c(
        "0" = "#eeeeee",
        "1" = "#d73027"
      ),
      labels = c(
        "Standard",
        "Superstar"
      )
    ) +
    
    labs(
      title = title_text,
      x = "Five-Year Period",
      y = NULL,
      fill = "Market Type"
    ) +
    
    theme_minimal() +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 12
      ),
      
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ),
      
      panel.grid = element_blank()
    )
}

############################################################
# Build Heatmaps
# We filter out the first "training" period (1995-1999)
############################################################

plot_global <- superstar_global %>%
  filter(!is.na(growth_window), growth_window != "1995-1999") %>%
  build_heatmap("Static Superstar Identification")

plot_period <- superstar_period %>%
  filter(!is.na(growth_window), growth_window != "1995-1999") %>%
  build_heatmap("Period-Specific Superstar Identification")

############################################################
# Combine Figures
############################################################

combined_figure <- (plot_global | plot_period) +
  plot_layout(guides = "collect") + # Optional: merges the legends into one
  plot_annotation(
    title = "Superstar City Identification",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

############################################################
# Export Figure

ggsave(
  filename = "output/figures/superstar_heatmaps.pdf",
  plot = combined_figure,
  width = 12,
  height = 8
)