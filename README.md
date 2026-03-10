# cliarapp

An R Shiny dashboard for the **Country-Level Institutional Assessment and Reform (CLIAR)** benchmarking tool, developed by the World Bank. 

It enables policymakers, researchers, and stakeholders to compare institutional indicator performance across countries using closeness-to-frontier scores.

---

## Getting Started

### Prerequisites

- R ≥ 4.5
- The `cliaretl` R package (World Bank package providing processed datasets)

### Running the App

```r
renv::restore() #install all package dependencies

shiny::runApp()
```

---

## Directory Structure

```
cliarapp/
├── global.R               # Package loading, data ingestion, shared objects
├── ui.R                   # Full dashboard UI layout (bs4Dash)
├── server.R               # Server-side logic, reactives, outputs
├── report.Rmd             # Word report template rendered on demand
├── coverage-report.Rmd    # Coverage diagnostics report
├── usage_tracker.qmd      # Quarto dashboard for Connect usage telemetry
│
├── auxiliary/             # Helper functions and plotting logic
│   ├── plots.R            # Core ggplot2/plotly chart functions
│   ├── vars-control.R     # Variable definitions and groupings
│   ├── fun_check_data.R   # Checks for missing data by country/indicator
│   ├── fun_family_data.R  # Filters data by indicator family
│   ├── fun_quantiles.R    # Computes quantile benchmarks
│   ├── fun_plot_prep.R    # Plot layout and formatting helpers
│   ├── fun_download_prep.R# Prepares data for Excel/CSV/DTA download
│   ├── fun_extract_var.R  # Extracts variable vectors from datasets
│   ├── fun_remove_avg.R   # Removes aggregate average columns
│   ├── fun_missing_var.R  # Identifies indicators with missing coverage
│   ├── fun_low_variance.R # Flags low-variance indicators
│   ├── fun_loadInputs.R   # Loads saved user input selections
│   ├── fun_publications.R # Renders publication links
│   ├── guides.R           # Cicerone interactive user guide definitions
│   ├── useBs4Dash.R       # bs4Dash compatibility shims
│   ├── clean_plotly_legend.R # Cleans plotly legend labels
│   ├── fixfacets.R        # Fixes facet spacing in ggplot2 outputs
│   └── dynamic_benchmarking.R # Dynamic (time-varying) benchmark logic
│
├── modules/
│   └── mod_publications.R # Shiny module for the publications tab
│
├── data/                  # Proprietary data files (not in version control)
│   └── README.md          # Instructions for obtaining the data
│
├── source/                # Upstream data processing scripts
│   ├── data_coverage_processing.R
│   └── funs_coverage.R
│
├── www/                   # Static web assets
│   ├── styles.css         # Custom CSS
│   └── publications/      # Publication PDFs/links
│
├── rsconnect/             # Posit Connect deployment manifests
└── renv/                  # renv lockfile and library for reproducibility
```

---

## Dashboard Tabs

| Tab | Description |
|-----|-------------|
| **Country Benchmarking** | Closeness-to-frontier scores for a base country vs. comparator countries or groups |
| **Cross-Country Comparison** | Side-by-side indicator-level bar charts across selected countries |
| **Bivariate Correlation** | Scatter plots of two indicators with optional GDP overlay |
| **World Map** | Choropleth map of indicator values or CTF scores |
| **Time Trends** | Year-on-year changes in raw indicator values |
| **Data** | Browsable data table with Excel/CSV/Stata download |
| **Methodology & User Guide** | Methodology documentation and FAQs |

---

---

## Data

The data used for this Shiny dashboard is provided through the `cliaretl` package. It provides:
- `closeness_to_frontier_static` — static CTF scores
- `closeness_to_frontier_dynamic` — time-varying CTF scores
- `db_variables_final` — indicator metadata
- `wb_country_groups` / `wb_country_list` — country and group reference tables

---

## Deployment

The app is deployed to Posit Connect. For more information on how to deploy it, please see [here](https://rstudio.github.io/rsconnect/reference/deployApp.html).