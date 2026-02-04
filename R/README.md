# CLIAR App - Code Organization

This document describes the refactored file structure of the CLIAR Benchmarking Dashboard.

## Folder Structure

```
cliarapp/
├── R/
│   ├── _ui/                    # UI component functions
│   │   ├── ui_home.R          # Home/landing page tab
│   │   └── ui_helpers.R       # Reusable UI helper functions
│   │
│   ├── _server/               # Server logic functions
│   │   ├── server_inputs.R    # Input save/load functionality (placeholder)
│   │   └── server_downloads.R # Download handlers (placeholder)
│   │
│   ├── data_processing/       # Data transformation functions
│   │   ├── fun_quantiles.R
│   │   ├── fun_family_data.R
│   │   ├── fun_missing_var.R
│   │   ├── fun_low_variance.R
│   │   ├── fun_check_data.R
│   │   ├── fun_download_prep.R
│   │   └── funs_coverage.R
│   │
│   ├── plotting/              # Plotting and visualization functions
│   │   ├── plots.R
│   │   ├── dynamic_benchmarking.R
│   │   ├── fixfacets.R
│   │   ├── clean_plotly_legend.R
│   │   └── fun_plot_prep.R
│   │
│   ├── utils/                 # Utility functions
│   │   ├── fun_extract_var.R
│   │   ├── fun_remove_avg.R
│   │   ├── fun_loadInputs.R
│   │   ├── fun_publications.R
│   │   ├── useBs4Dash.R
│   │   └── guides.R
│   │
│   └── modules/               # Shiny modules
│       └── mod_publications.R
│
├── ui.R                       # Main UI file (sources R/_ui/ components)
├── server.R                   # Main server file (sources R/_server/ components)
├── global.R                   # Global setup (sources all R/ subfolders)
└── app.R                      # Application entry point

```

## Refactoring Approach

### Phase 1: File Reorganization (COMPLETED ✅)
- Moved files from `auxiliary/` and `source/` into organized `R/` subfolders
- Updated all `source()` paths in `global.R` to point to new locations
- Created new folder structure: `data_processing/`, `plotting/`, `utils/`, `modules/`

### Phase 2: UI/Server Organization (CURRENT - Option A)
- Created `R/_ui/` and `R/_server/` directories
- **Approach**: Keep main `ui.R` and `server.R` mostly intact
- Extract only reusable/helper components into separate files
- Benefits:
  - Lower risk of breaking existing functionality
  - Easier to test incrementally
  - Still improves code organization

### What's Been Extracted

#### UI Components (R/_ui/):
1. **ui_home.R** - Complete home/landing page tab wrapped in `ui_home_tab()` function
2. **ui_helpers.R** - Reusable helper functions:
   - `download_button_style()` - Consistent button styling
   - `create_info_helper()` - Helper text with icons

#### Server Components (R/_server/):
1. **server_inputs.R** - Placeholder for input management logic
2. **server_downloads.R** - Placeholder for download handler logic

### Usage

All helper files are automatically sourced at the top of `ui.R` and `server.R`:

```r
# In ui.R
source(here("R", "_ui", "ui_helpers.R"))
source(here("R", "_ui", "ui_home.R"))

# In server.R
source(here("R", "_server", "server_inputs.R"))
source(here("R", "_server", "server_downloads.R"))
```

The home tab now uses the extracted function:
```r
tabItems(
  ui_home_tab(),  # Replaced inline code with function call
  ...
)
```

## Next Steps (Optional Future Work)

If you want to extract more code in the future:

1. **Extract more UI tabs**: Create `ui_benchmark.R`, `ui_comparison.R`, etc.
2. **Extract server logic by tab**: Create `server_benchmark.R`, `server_comparison.R`, etc.
3. **Create shared reactive modules**: Extract common reactive patterns
4. **Consolidate similar functions**: Group related functions together

## Testing

After refactoring:
1. Test that the app launches without errors
2. Verify all tabs load correctly
3. Test key functionality (benchmarking, downloads, plots)
4. Check that saved inputs still work

## Convention

- **UI files**: Contain functions that return `tabItem()` or UI components
- **Server files**: Contain logic that would go inside `server <- function(input, output, session)`
- **Keep it simple**: Don't over-extract; leave complex inline code in place if it's only used once
- **Function names**: Use descriptive names like `ui_[tabname]_tab()` or `server_[feature]_logic()`
