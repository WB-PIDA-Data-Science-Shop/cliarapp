mod_data_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "data",

    bs4Card(
      title = "Data download",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      collapsible = FALSE,

      fluidRow(
        column(width = 2.4, downloadButton(ns("down_clust_ctf_stat"), "CTF Static (Cluster-level aggregates only)",
                                            style = "width:100%; background-color: #204d74; color: white")),
        column(width = 2.4, downloadButton(ns("down_all_ctf_stat"), "CTF Static (All indicators)",
                                            style = "width:100%; background-color: #204d74; color: white")),
        column(width = 2.4, downloadButton(ns("down_clust_ctf_dyn"), "CTF Dynamic (Cluster-level aggregates only)",
                                            style = "width:100%; background-color: #204d74; color: white")),
        column(width = 2.4, downloadButton(ns("down_all_ctf_dyn"), "CTF Dynamic (All indicators)",
                                            style = "width:100%; background-color: #204d74; color: white")),
        column(width = 2.4, downloadButton(ns("down_original"), "Original indicators",
                                            style = "width:100%; background-color: #204d74; color: white")),
        column(width = 2.4, downloadButton(ns("down_db_var"), "Data Dictionary",
                                            style = "width:100%; background-color: #204d74; color: white"))
      )
    ),

    bs4Card(
      title = "Pre-Download Base & Comparison Country Selection",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      collapsed = TRUE,

      fluidRow(
        column(
          width = 6,
          style = "padding-left: 24px",
          pickerInput(
            ns("country_dwnld"),
            label = helper(
              shiny_tag = tags$span("Base country:", style = "font-size: 28px; color: #051f3f;"),
              type = "inline", icon = "circle-question", title = "Base country",
              content = c("Choose the base country of interest. (For some analysis, you can select more than one.) This menu can also be accessed in the Country Benchmarking tab"),
              buttonLabel = "Close", fade = TRUE, size = "s"
            ),
            choices = countries,
            choicesOpt = list(content = flags_with_countries, style = rep(length(flags_with_countries))),
            selected = NULL,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, `live-search` = TRUE)
          )
        )
      ),
      fluidRow(style = "height: 5px;"),

      shiny::fluidRow(
        column(
          width = 6,
          pickerInput(
            ns("groups_dwnld"),
            label = helper(
              shiny_tag = "Select comparison groups",
              type = "inline", icon = "circle-question", title = "Pre-defined groups",
              content = c("There are multiple ways to select the comparator countries. Here you can select one (or more) pre-defined group(s) (either as a comparator group itself or as a shortcut for selecting individual countries). When selecting more than one, it is the union (i.e., sum) of the groups that will be analyzed.This menu can also be accessed in the Country Benchmarking tab"),
              buttonLabel = "Close", fade = TRUE, size = "s"
            ),
            choices = group_list,
            selected = NULL,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, `live-search` = TRUE)
          )
        ),
        column(
          id = ns("show_countries_column_dwnld"),
          width = 3,
          style = "display: flex; align-items: center; justify-content: center;",
          shinyWidgets::materialSwitch(
            inputId = ns("show_countries_dwnld"),
            label = helper(
              shiny_tag = tags$b("Show list of countries"),
              type = "inline", icon = "circle-question", title = "List of countries",
              content = c("Here you can add and remove individual comparator countries. If you have already selected one or more the pre-defined groups, those countries will appear as selected, and you can manually add or remove."),
              buttonLabel = "Close", fade = TRUE, size = "s"
            ),
            value = FALSE,
            status = "success"
          )
        ),

        shiny::conditionalPanel(
          "input.show_countries_dwnld == true", ns = ns,
          fluidRow(style = "height: 15px;"),
          fluidRow(
            column(
              width = 12,
              checkboxGroupButtons(
                inputId = ns("countries_dwnld"),
                individual = TRUE,
                label = NULL,
                choices = countries,
                selected = "countries",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"))
              )
            )
          )
        )
      )
    ),

    bs4Card(
      title = "Interactive Data Access & Custom Download",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      collapsible = FALSE,

      fluidRow(
        column(
          width = 8,
          dataTableOutput(ns("benchmark_datatable")) |> shinycssloaders::withSpinner(color = "#051f3f", type = 8)
        ),
        column(
          width = 4,
          bs4Card(
            title = "Select information to display",
            status = "success",
            width = 12,
            collapsible = FALSE,

            pickerInput(
              ns("countries_data"),
              label = "Select countries to include",
              choices = c("All", "Base country only", "Base + comparison countries"),
              selected = "All",
              options = list(
                "All" = list(disabled = FALSE),
                "Base country only" = list(disabled = TRUE),
                "Base + comparison countries" = list(disabled = TRUE)
              )
            ),

            pickerInput(
              ns("vars"),
              label = "Select institutional families to include",
              choices = definitions |> pull(Family),
              selected = definitions |> pull(Family),
              multiple = TRUE,
              options = list(`actions-box` = TRUE)
            ),

            radioGroupButtons(
              ns("data_source"),
              label = "Select a data source",
              choices = c("Closeness to frontier (Static)", "Closeness to frontier (Dynamic)", "Original indicators"),
              selected = "Closeness to frontier (Static)",
              direction = "vertical",
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"))
            ),

            div(
              style = "display: flex; flex-direction: column; gap: 6px; align-items: flex-start;",
              helper(
                shiny_tag = tags$b("Descriptive Columns"),
                type = "inline", icon = "circle-question", title = "Descriptive Names",
                content = c("Here you can select whether you want abrreviated or full names for each of the columns in the downloaded data."),
                buttonLabel = "Close", fade = TRUE, size = "s"
              ),
              shinyWidgets::materialSwitch(inputId = ns("descriptions_dwnld"), label = NULL, value = FALSE, status = "success")
            ),

            shinyjs::hidden(
              radioGroupButtons(
                ns("data_value"),
                label = "Select information to show",
                choices = c("Value"),
                selected = "Value",
                direction = "vertical",
                justified = TRUE,
                checkIcon = list(yes = icon("ok", lib = "glyphicon"))
              )
            ),

            p(tags$b("Download data")),

            downloadButton(ns("download_global_rds"), ".rds", style = "width:100%; background-color: #204d74; color: white"),
            downloadButton(ns("download_global_csv"), ".csv", style = "width:100%; background-color: #204d74; color: white"),
            downloadButton(ns("download_global_dta"), ".dta", style = "width:100%; background-color: #204d74; color: white")
          )
        )
      )
    )
  )
}

mod_data_server <- function(id,
                             shared,             # list returned by mod_benchmark_server()
                             countries,
                             flags_with_countries,
                             group_list,
                             country_list,
                             definitions,
                             variable_names,
                             db_variables,
                             global_data,
                             global_data_dyn,
                             raw_data,
                             ctf_long,
                             all_groups) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Sync _dwnld pickers with Benchmarking tab (one-way) ---------------------------
    observeEvent(shared$live_country(), {
      updatePickerInput(session, "country_dwnld", selected = shared$live_country())
    })

    observeEvent(shared$groups(), {
      updatePickerInput(session, "groups_dwnld", selected = shared$groups())
    })

    observeEvent(shared$countries(), {
      updateCheckboxGroupButtons(session, "countries_dwnld", choices = countries,
                                  checkIcon = list(yes = icon("ok", lib = "glyphicon", style = "color: #00000")),
                                  selected = shared$countries())
    })

    ## Enable country-selection radio choices once "Apply selection" has run --------
    observeEvent(shared$base_country(), {
      updatePickerInput(
        session, "countries_data",
        choices = c("All", "Base country only", "Base + comparison countries"),
        options = list(
          "All" = list(disabled = FALSE),
          "Base country only" = list(disabled = FALSE),
          "Base + comparison countries" = list(disabled = FALSE)
        )
      )
    })

    ## groups_dwnld <-> countries_dwnld agreement -------------------------------------
    observeEvent(input$groups_dwnld, {
      selected_dgroups <- input$groups_dwnld
      selected_dcountry <- input$country_dwnld

      if (is.null(selected_dgroups)) {
        selected <- NULL
      } else {
        selected <- country_list %>% filter(group %in% selected_dgroups) %>% select(country_name) %>% unique()
        if (!is.null(selected_dcountry)) selected <- selected %>% filter(country_name != selected_dcountry)
        selected <- selected %>% pluck(1)
      }

      updateCheckboxGroupButtons(session, "countries_dwnld", label = NULL, choices = countries,
                                  checkIcon = list(yes = icon("ok", lib = "glyphicon", style = "color: #e94152")),
                                  selected = selected)
    }, ignoreNULL = FALSE)

    ## Pre-download dataset construction ----------------------------------------------
    pre_download_data <- reactive({
      data <- switch(
        input$data_source,
        "Closeness to frontier (Static)" = global_data,
        "Closeness to frontier (Dynamic)" = global_data_dyn,
        "Original indicators" = raw_data %>% select(-ends_with("_avg"))
      )

      groups <- all_groups

      selected_countries <- switch(
        input$countries_data,
        "All" = countries,
        "Base country only" = input$country_dwnld,
        "Base + comparison countries" = c(input$country_dwnld, input$countries_dwnld)
      )

      vars <- variable_names %>%
        filter(family_name %in% input$vars, var_level == "indicator") %>%
        pull(variable)

      vars_table <- switch(
        input$data_source,
        "Closeness to frontier (Static)" = c("country_name", "country_code", "country_group", "income_group", "region", vars),
        "Closeness to frontier (Dynamic)" = c("country_name", "country_code", "country_group", "income_group", "region", "year", vars),
        names(data)
      )
      vars_table <- unname(vars_table)

      data <- data %>%
        filter(country_name %in% c(selected_countries, groups)) %>%
        ungroup() %>%
        mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
        select(any_of(vars_table))

      if (input$data_value == "Rank") {
        data <- data %>%
          filter(country_group == 0) %>%
          mutate(across(6:ncol(.), ~ rank(desc(.), ties.method = "min")))
      }

      data
    })

    ## Interactive table ---------------------------------------------------------------
    output$benchmark_datatable <- DT::renderDataTable(
      server = FALSE,
      datatable(
        pre_download_data() %>%
          setnames(., as.character(db_variables$variable), as.character(db_variables$variable), skip_absent = TRUE),
        rownames = FALSE,
        extensions = c("FixedColumns"),
        filter = "none",
        options = list(
          scrollX = TRUE, scrollY = "550px", pageLength = 25, autoWidth = TRUE,
          dom = "lftipr", fixedColumns = list(leftColumns = 1, rightColumns = 0)
        )
      )
    )

    ## Global format downloads ----------------------------------------------------------
    output$download_global_rds <- downloadHandler(
      filename = function() paste0("CLIAR ", input$data_source, " data.rds"),
      content = function(file) {
        show_modal_spinner(color = "#17a2b8", text = "Loading Data")
        on.exit(remove_modal_spinner())
        write_rds(rds_prep(pre_download_data(), input$descriptions_dwnld), file)
      }
    )

    output$download_global_csv <- downloadHandler(
      filename = function() paste0("CLIAR ", input$data_source, " data.csv"),
      content = function(file) {
        show_modal_spinner(color = "#17a2b8", text = "Loading Data")
        on.exit(remove_modal_spinner())
        write_csv(csv_prep(pre_download_data(), input$descriptions_dwnld), file, na = "")
      }
    )

    output$download_global_dta <- downloadHandler(
      filename = function() paste0("CLIAR ", input$data_source, " data.dta"),
      content = function(file) {
        show_modal_spinner(color = "#17a2b8", text = "Loading Data")
        on.exit(remove_modal_spinner())
        write_dta(dta_prep(pre_download_data(), input$descriptions_dwnld), file)
      }
    )

    ## Pre-packaged CSV downloads --------------------------------------------------------
    output$down_clust_ctf_stat <- downloadHandler(
      filename = function() "CTF Static (Cluster-level aggregates only) data.csv",
      content = function(file) {
        show_modal_spinner(color = "#17a2b8", text = "Loading Data")
        on.exit(remove_modal_spinner())
        write_csv(ctf_long, file, na = "")
      }
    )

    output$down_all_ctf_stat <- downloadHandler(
      filename = function() "CLIAR CTF Static (All indicators) data.csv",
      content = function(file) {
        show_modal_spinner(color = "#17a2b8", text = "Loading Data")
        on.exit(remove_modal_spinner())
        write_csv(global_data, file, na = "")
      }
    )

    output$down_clust_ctf_dyn <- downloadHandler(
      filename = function() "CTF Dynamic (Cluster-level aggregates only) data.csv",
      content = function(file) {
        show_modal_spinner(color = "#17a2b8", text = "Loading Data")
        on.exit(remove_modal_spinner())
        write_csv(global_data_dyn %>% select(1:6, (ncol(.) - 6):ncol(.)), file, na = "")
      }
    )

    output$down_all_ctf_dyn <- downloadHandler(
      filename = function() "CLIAR CTF Dynamic (All indicators) data.csv",
      content = function(file) {
        show_modal_spinner(color = "#17a2b8", text = "Loading Data")
        on.exit(remove_modal_spinner())
        write_csv(global_data_dyn, file, na = "")
      }
    )

    output$down_original <- downloadHandler(
      filename = function() "CLIAR Original indicators data.csv",
      content = function(file) {
        show_modal_spinner(color = "#17a2b8", text = "Loading Data")
        on.exit(remove_modal_spinner())
        write_csv(raw_data %>% select(-ends_with("_avg")), file, na = "")
      }
    )

    output$down_db_var <- downloadHandler(
      filename = function() "CLIAR Data Dictionary.csv",
      content = function(file) {
        show_modal_spinner(color = "#17a2b8", text = "Loading Data")
        on.exit(remove_modal_spinner())
        write_csv(db_variables, file, na = "")
      }
    )
  })
}
