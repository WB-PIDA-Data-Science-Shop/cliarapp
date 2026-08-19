# Server ################################################################################
#
# CLIAR Benchmarking Dashboard - Server Logic Script (server.R)
#
# As of this refactor, the bulk of reactive logic lives inside the tab
# modules (modules/mod_*.R). This top-level server function is now
# responsible only for: app-wide setup (helpers, guided tour init),
# instantiating shared cross-tab state, calling each module's server
# function with the data/lookup objects and shared state it needs, and the
# handful of download handlers belonging to the purely informative
# Methodology & User Guide tab (static file copies — no analysis).

server <- function(input, output, session) {

  observe_helpers()

  ## Guided tour (Home page) ---------------------------------------------------
  observeEvent(input$start, {
    guide_landing_page$init()$start()
  })

  ## Shared cross-tab state ------------------------------------------------------
  ## Keeps the base-country picker synced across Cross-Country Comparison,
  ## Bivariate Correlation, and Time Trends, matching the original app's
  ## three-way observer chain.
  display_country <- reactiveVal(NULL)

  ## Module wiring ----------------------------------------------------------------
  benchmark_state <- mod_benchmark_server(
    "benchmark",
    countries = countries,
    flags_with_countries = flags_with_countries,
    group_list = group_list,
    variable_list = variable_list,
    country_list = country_list,
    global_data = global_data,
    global_data_dyn = global_data_dyn,
    db_variables = db_variables,
    variable_names = variable_names,
    ctf_long = ctf_long,
    vars_all = vars_all,
    vars_family = vars_family,
    family_names = family_names,
    family_order = family_order,
    definitions = definitions,
    plot_height = plot_height,
    plotly_remove_buttons = plotly_remove_buttons
  )

  mod_country_compare_server(
    "country",
    shared = benchmark_state,
    display_country = display_country,
    countries = countries,
    group_list = group_list,
    global_data = global_data,
    raw_data = raw_data,
    variable_names = variable_names,
    db_variables = db_variables,
    plotly_remove_buttons = plotly_remove_buttons,
    plot_height = plot_height
  )

  mod_scatter_server(
    "scatter",
    shared = benchmark_state,
    display_country = display_country,
    countries = countries,
    group_list = group_list,
    country_list = country_list,
    global_data = global_data,
    variable_names = variable_names,
    db_variables = db_variables,
    plotly_remove_buttons = plotly_remove_buttons,
    plot_height = plot_height
  )

  mod_trends_server(
    "trends",
    shared = benchmark_state,
    display_country = display_country,
    countries = countries,
    group_list = group_list,
    raw_data = raw_data,
    country_list = country_list,
    db_variables = db_variables,
    plot_height = plot_height
  )

  mod_world_map_server(
    "world_map",
    shared = benchmark_state,
    variable_names = variable_names,
    db_variables = db_variables,
    spatial_data = spatial_data,
    plotly_remove_buttons = plotly_remove_buttons,
    plot_height = plot_height
  )

  mod_data_server(
    "data",
    shared = benchmark_state,
    countries = countries,
    flags_with_countries = flags_with_countries,
    group_list = group_list,
    country_list = country_list,
    definitions = definitions,
    variable_names = variable_names,
    db_variables = db_variables,
    global_data = global_data,
    global_data_dyn = global_data_dyn,
    raw_data = raw_data,
    ctf_long = ctf_long,
    all_groups = all_groups
  )

  publicationsServer("publications")

  ## Methodology & User Guide tab — static downloads only (informative tab) --------
  output$download_user_guide <- downloadHandler(
    filename = "CLIAR_User_Guide.docx",
    content = function(file) {
      existing_file_path <- paste(here(), "www", "dashboard_userguide_outline_v5.2.docx", sep = "/")
      file.copy(existing_file_path, file)
    }
  )

  output$download_indicators <- downloadHandler(
    filename = "CLIAR Indicators.csv",
    content = function(file) {
      write_csv(
        db_variables %>%
          select(indicator = var_name, family = family_name, description, description_short, source),
        file, na = ""
      )
    }
  )

  output$download_metho <- downloadHandler(
    filename = "CLIAR Benchmarking.pdf",
    content = function(file) file.copy("www/CLIAR Benchmarking.pdf", file)
  )
}
