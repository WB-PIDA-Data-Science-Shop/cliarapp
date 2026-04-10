# This script generates the data and plot for PFM Equatorial Guinea analysis


# set-up -----------------------------------------------------------------
# load libraries
library(tidyverse)
library(ggplot2)
library(haven)
library(here)

ggsave <- partial(
  ggplot2::ggsave,
  bg     = "white",
  width  = 16,
  height = 6
)

# data load and functions -------------------------------------------------

db_variables <-
  cliaretl::db_variables_final

family_order <- 
  cliaretl::family_order

db_variables <- 
  left_join(db_variables, family_order, by = "family_name")

source(here("auxiliary", "vars-control.R"))

# Function that defines quantiles based on country, comparison and variables
source(here("auxiliary", "fun_quantiles.R"))
source(here("auxiliary", "fun_family_data.R"))
source(here("auxiliary", "fun_missing_var.R"))

# data-processing ---------------------------------------------------------

# --- Parameters (mirroring dashboard selections) ---
base_country     <- "Equatorial Guinea"
comparison_group <- "Africa Western and Central"
threshold        <- "Default"   # "Default" = 25/50, "Terciles" = 33/66

# --- Load base datasets ---
global_data <- cliaretl::closeness_to_frontier_static |> ungroup()

country_list <- cliaretl::wb_country_list

country_groups <- cliaretl::wb_country_list

variable_names <-
  db_variables |>
  select(variable, var_level, var_name, family_var, family_name) |>
  filter(family_var != "vars_other")

family_names <-
  db_variables |>
  select(variable = family_var, var_name = family_name) |>
  distinct() |>
  filter(variable != "vars_other")

# --- Resolve comparison countries from group ---
comparison_countries <-
  country_groups |>
  filter(group == comparison_group) |>
  pull(country_name) |>
  unique()

# --- Step 1: Build the family-level average data (Overview row) ---
# This replicates `data_family()` in the server
pfm_family_data <-
  family_data(
    global_data,
    base_country,
    variable_names,
    comparison_countries
  ) |>
  def_quantiles(
    base_country,
    country_list,
    comparison_countries,
    vars_family,
    family_names,
    threshold
  ) |>
  filter(variable == "vars_pfm")

# --- Step 2: Build per-indicator data (individual PFM rows) ---
# This replicates `data_avg()` in the server
static_avg_data <- global_data |> select(-matches("_avg"))
vars_static_avg_data <- names(static_avg_data)[6:length(static_avg_data)]
static_avg <- compute_family_average(
  static_avg_data, vars_static_avg_data, "static",
  db_variables, base_country, comparison_countries
)
static_avg <- static_avg |> select(-matches("NA"))
static_avg_data <- static_avg_data |> left_join(static_avg, by = "country_code")

pfm_indicator_data <-
  static_avg_data |>
  def_quantiles(
    base_country,
    country_list,
    comparison_countries,
    vars_all,
    variable_names,
    threshold
  ) |>
  filter(variable %in% vars_pfm)

# --- Step 3: Combine family average + individual indicators ---
pfm_plot_data <- bind_rows(pfm_family_data, pfm_indicator_data) |> 
  select(-q_lv_75, q_lv_25) # not needed

write_dta(
  pfm_plot_data,
  here("eg-pfm-analysis", "pfm_plot_data.dta")
)


write_csv(
  pfm_plot_data,
  here("eg-pfm-analysis", "pfm_plot_data.csv")
)

# OPTIONAL:
# # Filter to base country only (what the dashboard shows as the dot)
# pfm_base <- pfm_plot_data |> filter(country_name == base_country)




# graph ------------------------------------------------------------------

# Prepare the data for the plot
plot_data <- pfm_plot_data |>
  mutate(
    # Uppercase family-average labels (matches dashboard behaviour)
    var_name = if_else(
      str_detect(var_name, regex("average", ignore_case = TRUE)),
      toupper(var_name),
      var_name
    ),
    # Remove "Institutions" from labels (dashboard Issue #283)
    var_name = str_remove(var_name, "Institutions") |> str_squish()
  )

# Order indicators: family average first, then by rank_id from db_variables
indicator_order <- plot_data |>
  left_join(db_variables |> select(variable, rank_id), by = "variable") |>
  distinct(var_name, rank_id) |>
  arrange(desc(rank_id)) |>
  pull(var_name)

plot_data <- plot_data |>
  mutate(var_name = factor(var_name, levels = indicator_order, ordered = TRUE))

# Tooltip text
plot_data <- plot_data |>
  group_by(dtf) |>
  mutate(
    text = paste(
      "Closeness to frontier:", round(dtf, 3), "<br>",
      "Country:", paste(country_name, collapse = ", ")
    )
  ) |>
  ungroup()

# Status colour palette
status_levels <- c(
  "Weak\n(bottom 25%)",
  "Emerging\n(25% - 50%)",
  "Strong\n(top 50%)"
)

status_colors <- c(
  "Weak\n(bottom 25%)"     = "#D2222D",
  "Emerging\n(25% - 50%)"  = "#FFBF00",
  "Strong\n(top 50%)"      = "#238823"
)

# Ensure all three levels are present in the factor so the legend is complete
plot_data <- plot_data |>
  mutate(status = factor(status, levels = status_levels)) |> 
  filter(variable != "vars_pfm") # Exclude 'Other' family if present (not in PFM but just in case)

# Build the plot
pfm_plot <-
  ggplot() +
  # --- Red segment: 0 → q_cutoff1 ---
  geom_segment(
    data = plot_data,
    aes(y = var_name, yend = var_name, x = 0, xend = q_cutoff1),
    color = "#e47a81", linewidth = 2, alpha = 0.1
  ) +
  # --- Yellow segment: q_cutoff1 → q_cutoff2 ---
  geom_segment(
    data = plot_data,
    aes(y = var_name, yend = var_name, x = q_cutoff1, xend = q_cutoff2),
    color = "#ffd966", linewidth = 2, alpha = 0.3
  ) +
  # --- Green segment: q_cutoff2 → 1 ---
  geom_segment(
    data = plot_data,
    aes(y = var_name, yend = var_name, x = q_cutoff2, xend = 1),
    color = "#8ec18e", linewidth = 2, alpha = 0.3
  ) +
  # --- Frontier dashed line at x = 1 ---
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  # --- Comparison country dots (hollow circles) ---
  geom_point(
    data = plot_data,
    aes(y = var_name, x = dtf, text = text),
    shape = 21, size = 2, color = "gray30", fill = "white", alpha = 0.5
  ) +
  # --- Base country dot (filled, coloured by status) ---
  geom_point(
    data = plot_data |> filter(country_name == base_country),
    aes(y = var_name, x = dtf, fill = status),
    shape = 21, size = 4, color = "black"
  ) +
  scale_fill_manual(values = status_colors, drop = FALSE) +
  scale_y_discrete(labels = \(x) str_wrap(x, width = 35)) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    axis.ticks       = element_blank(),
    axis.text        = element_text(color = "black"),
    axis.text.y      = element_text(size = 10),
    axis.text.x      = element_text(size = 11),
    legend.box       = "vertical",
    plot.caption     = element_text(size = 8, hjust = 0),
    plot.caption.position = "plot"
  ) +
  labs(
    title = paste0("Public Finance Institutions — ", base_country),
    y     = "",
    x     = "Closeness to frontier",
    fill  = NULL
  )

ggsave(
  plot = pfm_plot,
  filename = here("eg-pfm-analysis", "pfm_equatorial_guinea_vs_afwc.png")
)

