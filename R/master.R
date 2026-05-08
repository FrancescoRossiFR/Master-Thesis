############################################################
# MASTER RUN SCRIPT: Superstar City Effect
# Author: Francesco Rossi
# Purpose: Execute the complete data pipeline and analysis
############################################################

# --- 0. Initialize Environment ---
# NO setwd() needed here if you opened the .Rproj file first.
# R will automatically be at the "Master-Thesis/" root.

cat("--- Project Root Identified at:", getwd(), "---\n")

# List of required packages
required_packages <- c(
  "tidyverse", "janitor", "zoo", "fixest", "readxl",
  "modelsummary", "patchwork", "ggrepel", "scales", 
  "sandwich", "lmtest", "clubSandwich"
)

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load core library
library(tidyverse)

# Ensure output directories exist in the root
dirs <- c("data/processed", "output/tables", "output/figures")
lapply(dirs, function(d) if(!dir.exists(d)) dir.create(d, recursive = TRUE))

# --- 1. Data Preparation (Cleaning & Merging) ---
cat("\n--- Stage 1: Data Preparation ---\n")
source("R/01_prepare_greix_data.R")      
source("R/02_prepare_income_data.R")     
source("R/03_build_analysis_panel.R")    

# --- 2. Econometric Analysis ---
cat("\n--- Stage 2: Econometric Models ---\n")
source("R/05_regressions.R")             
source("R/05_regressions_sf.R")          
source("R/07_iv_analysis.R")             

# --- 3. Visualizations & Figures ---
cat("\n--- Stage 3: Generating Visualizations ---\n")
source("R/06_figures_tables.R")          
source("R/08_macro_trends_plot.R")