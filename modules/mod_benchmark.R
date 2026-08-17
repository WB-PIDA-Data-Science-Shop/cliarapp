mod_benchmark_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "benchmark",
    useShinyjs(),

    fluidRow(
      column(
        width = 6,
        style = "padding-left: 24px",
        pickerInput(
          ns("country"),
          label = helper(
            shiny_tag = tags$span("Base country:", style = "font-size: 28px; color: #051f3f;"),
            type = "inline",
            icon = "circle-question",
            title = "Base country",
            content = c("Choose the base country of interest. (For some analysis, you can select more than one.)"),
            buttonLabel = "Close",
            fade = TRUE,
            size = "s"
          ),
          choices = countries,
          choicesOpt = list(
            content = flags_with_countries,
            style = rep(length(flags_with_countries))
          ),
          selected = NULL,
          multiple = TRUE,
          options = list(`actions-box` = TRUE, `live-search` = TRUE)
        )
      ),
      column(
        width = 3,
        tags$span("Guided tour", style = "font-size: 1rem; color: #051f3f; font-weight:bold"),
        fluidRow(
          shinyWidgets::actionBttn(
            inputId = ns("start_guide_bench"),
            label = "Start",
            icon = shiny::icon("gear"),
            style = "jelly",
            color = "primary",
            size = "sm"
          )
        )
      ),
      column(
        id = ns("input_buttons"),
        width = 3,
        fluidRow(
          column(
            width = 8,
            helper(
              shiny_tag = tags$span("Selection of Countries", style = "font-size: 1rem; color: #051f3f; font-weight:bold"),
              type = "inline",
              icon = "circle-question",
              title = "Saving and loading Selection of Countries",
              content = c(
                "You can save your selected inputs to return to at a future time: click \u201cSave Selection of Countries\u201d button to download a .rds file to your computer with that information. When you return to the dashboard, you can click \u201cLoad Selection of Countries\u201d button and then \u201cBrowse\u201d to select this same .rds file. Loading this .rds file will re-populate all of the selections that you previously made."
              ),
              buttonLabel = "Close",
              fade = TRUE,
              size = "s"
            )
          ),
          column(4)
        ),
        fluidRow(
          buttons_func(id = ns("load_inputs"), lab = "Load"),
          shinyjs::disabled(downloadButton(ns("save_inputs"), "Save"))
        )
      )
    ),

    fluidRow(style = "height: 5px;"),

    ### Comparator countries card
    bs4Card(
      title = "Comparator countries",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,

      shiny::fluidRow(
        column(
          width = 6,
          pickerInput(
            ns("groups"),
            label = helper(
              shiny_tag = "Select comparison groups",
              type = "inline",
              icon = "circle-question",
              title = "Pre-defined groups",
              content = c(
                "There are multiple ways to select the comparator countries. Here you can select one (or more) pre-defined group(s) (either as a comparator group itself or as a shortcut for selecting individual countries). When selecting more than one, it is the union (i.e., sum) of the groups that will be analyzed."
              ),
              buttonLabel = "Close",
              fade = TRUE,
              size = "s"
            ),
            choices = group_list,
            selected = NULL,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, `live-search` = TRUE)
          )
        ),
        column(
          id = ns("show_countries_column"),
          width = 3,
          style = "display: flex; flex-direction: column; gap: 6px; align-items: flex-start;",
          helper(
            shiny_tag = tags$b("Show list of countries"),
            type = "inline",
            icon = "circle-question",
            title = "List of countries",
            content = c("Here you can add and remove individual comparator countries. If you have already selected one or more the pre-defined groups, those countries will appear as selected, and you can manually add or remove."),
            buttonLabel = "Close",
            fade = TRUE,
            size = "s"
          ),
          shinyWidgets::materialSwitch(inputId = ns("show_countries"), label = NULL, value = FALSE, status = "success")
        ),
        column(
          id = ns("custom_grps_column"),
          width = 3,
          style = "display: flex; flex-direction: column; gap: 6px; align-items: flex-start;",
          helper(
            shiny_tag = tags$b("Create custom groups"),
            type = "inline",
            icon = "circle-question",
            title = "Custom groups",
            content = paste0(
              "Alternative, you may create up to three custom groups of countries. This feature will additionally display in the Benchmarking graphs the median estimates of each custom group.",
              "<br><br><b>Note:</b> Currently custom groups are not allowed when displaying ranks instead of values, when ranking from best to worst, or when doing the dynamic benchmark."
            ),
            buttonLabel = "Close",
            fade = TRUE,
            size = "s"
          ),
          shinyWidgets::materialSwitch(inputId = ns("create_custom_grps"), label = NULL, value = FALSE, status = "success")
        )
      ),

      shiny::conditionalPanel(
        condition = "input.create_custom_grps == true", ns = ns,
        fluidRow(
          column(
            width = 12,
            shinyWidgets::materialSwitch(
              inputId = ns("show_custom_grps"),
              label = tags$b("Show custom groups"),
              status = "success",
              value = TRUE
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            numericInput(ns("custom_grps_count"), label = "Number of groups", value = 1, min = 1, max = 3, step = 1)
          ),
          column(
            width = 3,
            style = "display: flex; align-items: center; justify-content: center;",
            shinyWidgets::actionBttn(
              inputId = ns("save_custom_grps"),
              label = "Save custom groups",
              icon = shiny::icon("save"),
              style = "jelly",
              color = "primary",
              size = "sm"
            )
          ),
          column(
            width = 12,
            conditionalPanel("input.custom_grps_count >= 1", ns = ns, uiOutput(ns("custom_grps")))
          )
        )
      ),

      #### Countries list
      shiny::conditionalPanel(
        condition = "input.show_countries == true", ns = ns,
        fluidRow(style = "height: 15px;"),
        fluidRow(
          column(
            width = 12,
            checkboxGroupButtons(
              inputId = ns("countries"),
              individual = TRUE,
              label = NULL,
              choices = countries,
              selected = NULL,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"))
            )
          )
        )
      )
    ),

    ### Benchmarking options card
    bs4Card(
      title = "Benchmarking options",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      fluidRow(
        column(
          width = 6,
          pickerInput(
            inputId = ns("threshold"),
            label = helper(
              shiny_tag = tags$b("Benchmarking Thresholds"),
              type = "inline",
              icon = "circle-question",
              title = "Benchmarking Thresholds",
              content = c("The default benchmarking thresholds for weak, emerging and strong institutions are 25th and 50th percentiles. You can also select the \u201cTerciles\u201d option, which uses 33rd and 66th percentiles as thresholds instead."),
              buttonLabel = "Close",
              fade = TRUE,
              size = "s"
            ),
            choices = c("Default", "Terciles")
          )
        ),
        column(width = 1),
        column(
          width = 5,
          div(
            id = ns("benchmark_dots_div"),
            prettyCheckbox(
              inputId = ns("benchmark_dots"),
              label = helper(
                shiny_tag = tags$b("Show comparison countries"),
                type = "inline",
                icon = "circle-question",
                title = "Show comparison countries",
                content = c("Select this option to show the comparison countries as white circles on the plots. You may hover over each circle to see the country name. Note that individual countries are represented by circles in the first example below. This shows the distribution of values for the comparison group."),
                buttonLabel = "Close",
                fade = TRUE,
                size = "s"
              ),
              value = FALSE,
              icon = icon("check"),
              status = "success"
            )
          ),
          div(
            id = ns("rank_div"),
            prettyCheckbox(
              inputId = ns("rank"),
              label = helper(
                shiny_tag = tags$b("Show rank instead of value"),
                type = "inline",
                icon = "circle-question",
                title = "Show rank instead of value",
                content = c("Select this option to change the x-axis of the static benchmarking plot to display rankings instead of the CTF value."),
                buttonLabel = "Close",
                fade = TRUE,
                size = "s"
              ),
              value = FALSE,
              icon = icon("check"),
              status = "success"
            )
          ),
          div(
            id = ns("preset_order_div"),
            prettyCheckbox(
              inputId = ns("preset_order"),
              label = helper(
                shiny_tag = tags$b("Rank indicators from best to worst"),
                type = "inline",
                icon = "circle-question",
                title = "Rank indicators from best to worst",
                content = c("Select this option to change the ordering of the variables on the vertical axis of the figure. Ranking from best to worst will place the indicator for which the base country has the highest value first and the indicator with the lowest value last."),
                buttonLabel = "Close",
                fade = TRUE,
                size = "s"
              ),
              value = FALSE,
              icon = icon("check"),
              status = "success"
            )
          )
        )
      )
    ),

    ### Outputs card
    bs4Card(
      title = "Outputs",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      fluidRow(
        column(
          width = 6,
          pickerInput(
            ns("family"),
            label = helper(
              shiny_tag = tags$b("Select institutional cluster"),
              type = "inline",
              icon = "circle-question",
              title = "Institutional cluster",
              content = c("Choose the institutional cluster you would like to display. The overview displays the aggregate results at the institutional-cluster level. When selecting a specific institutional-cluster, the individual indicators/components will be displayed."),
              buttonLabel = "Close",
              fade = TRUE,
              size = "s"
            ),
            choices = c("Overview", names(variable_list)),
            selected = NULL
          )
        ),
        # Please do not delete the below button, though not being displayed. It has downstream impact on other process.
        column(
          width = 1,
          pickerInput(
            inputId = ns("benchmark_median"),
            label = "Show group median",
            choices = append("Comparison countries", group_list),
            selected = NULL,
            multiple = TRUE,
            options = list(`live-search` = TRUE, "max-options" = 3)
          )
        ),
        column(
          width = 4,
          style = "display: flex; align-items: center; justify-content: center;",
          fluidRow(
            column(width = 10, uiOutput(ns("select_button"))),
            column(
              width = 1,
              helper(
                shiny_tag = NULL,
                type = "inline",
                icon = "circle-question",
                title = "Apply",
                content = c("Click on this box to (re-)run the analysis and (re-)load the resulting graphs. Note that this has to be done for every new selection or option, including a different institutional cluster. This option is enabled when the base country and at least 10 comparison countries are selected."),
                buttonLabel = "Close",
                fade = TRUE,
                size = "s"
              )
            ),
            column(1)
          )
        )
      ),
      fluidRow(
        column(
          width = 2,
          helper(
            shiny_tag = tags$b("Downloads"),
            type = "inline",
            icon = "circle-question",
            title = "Pre-populated reports and data",
            content = c(
              "Download pre-populated Word or Power Point documents with the results. Note that you may select the \u201cAdvanced Report\u201d box to receive more detailed information - including all dynamic graphs. Select the help button next to the checkbox to learn more. Click the download \u201cData\u201d button to download a CSV file that contains the data needed to recreate the benchmarking graphs."
            ),
            buttonLabel = "Close",
            fade = TRUE,
            size = "s"
          )
        )
      ),
      fluidRow(
        column(
          width = 9,
          fluidRow(
            id = ns("download_reports"),
            column(width = 3, shinyjs::disabled(downloadButton(ns("report"), "Editable report", style = "width:100%; background-color: #204d74; color: white"))),
            column(width = 3, shinyjs::disabled(downloadButton(ns("advreport"), "Advanced Report", style = "width:100%; background-color: #204d74; color: white"))),
            column(width = 3, shinyjs::disabled(downloadButton(ns("pptreport"), "PPT report", style = "width:100%; background-color: #204d74; color: white"))),
            column(id = ns("download_data_opt"), width = 3, shinyjs::disabled(downloadButton(ns("download_data_1"), "Data", style = "width:100%; background-color: #204d74; color: white")))
          )
        ),
        column(width = 3, shinyjs::disabled(downloadButton(ns("download_Coverage"), "Coverage report", style = "width:100%; background-color: #204d74; color: white")))
      )
    ),

    ### Static Benchmarks
    bs4Card(
      title = "Static Benchmarks",
      collapsible = TRUE,
      width = 12,
      conditionalPanel(
        "input.select !== 0", ns = ns,
        fluidRow(
          column(
            width = 12,
            plotlyOutput(ns("plot"), height = paste0(2.25 * plot_height, "px")) %>%
              shinycssloaders::withSpinner(color = "#051f3f", type = 8)
          )
        ),
        fluidRow(
          shinyWidgets::materialSwitch(inputId = ns("show_plot_notes"), label = "Show notes", status = "success", value = FALSE)
        ),
        conditionalPanel(
          "input.show_plot_notes !== false", ns = ns,
          fluidRow(column(width = 12, htmlOutput(ns("plot_notes"))))
        )
      )
    ),

    ### Dynamic Benchmarks
    bs4Card(
      width = 12,
      solidHeader = FALSE,
      gradientColor = "primary",
      title = "Dynamic Benchmarks",
      collapsible = TRUE,
      tags$style(paste0("
        #", ns("dynamic_benchmark_plot"), " {
          height: 100%;
          overflow-y: scroll;
        }
      ")),
      conditionalPanel(
        "input.select !== 0 && output.plot!=null", ns = ns,
        fluidRow(
          column(
            width = 12,
            plotlyOutput(ns("dynamic_benchmark_plot"), height = paste0(plot_height * 4, "px")) %>%
              shinycssloaders::withSpinner(color = "#051f3f", type = 8)
          )
        )
      )
    ),

    bs4Card(
      title = "Indicator definitions",
      collapsible = TRUE,
      collapsed = TRUE,
      status = "secondary",
      solidHeader = TRUE,
      width = 12,
      tableOutput(ns("definition"))
    )
  )
}

mod_benchmark_server <- function(id,
                                  countries,
                                  flags_with_countries,
                                  group_list,
                                  variable_list,
                                  country_list,
                                  global_data,
                                  global_data_dyn,
                                  db_variables,
                                  variable_names,
                                  ctf_long,
                                  vars_all,
                                  vars_family,
                                  family_names,
                                  family_order,
                                  definitions,
                                  plot_height,
                                  plotly_remove_buttons) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Setup ------------------------------------------------------------------
    shinyjs::disable("preset_order")
    shinyjs::hide("benchmark_median")

    observeEvent(input$start_guide_bench, {
      guide_benchmark$init()$start()
    })

    ## Base country -------------------------------------------------------------
    base_country <- eventReactive(input$select, input$country, ignoreNULL = FALSE)

    ## Load inputs --------------------------------------------------------------
    observeEvent(input$load_inputs, {
      shiny::showModal(
        modalDialog(
          title = htmltools::tags$span(htmltools::tags$strong("Please upload an input file")),
          tagList(
            shiny::fluidRow(shiny::fileInput(inputId = ns("input_file"), label = "", accept = ".rds")),
            shiny::fluidRow(buttons_func(ns("submit"), "Submit")),
            shiny::fluidRow(style = "height:15px;")
          ),
          easyClose = FALSE
        )
      )
    })

    observeEvent(input$rank, {
      if (input$rank == FALSE) shinyjs::disable("preset_order") else shinyjs::enable("preset_order")
    })

    core_fields <- c(
      "country", "groups", "family", "benchmark_median", "benchmark_dots", "rank",
      "threshold", "worst_to_best_order", "comparison_countries", "create_custom_groups"
    )

    saved_inputs_df <- shiny::eventReactive(input$submit, {
      file <- input$input_file
      req(file)
      saved_inputs_df <- readRDS(file$datapath)
      if (all(!core_fields %in% names(saved_inputs_df))) saved_inputs_df <- NULL
      saved_inputs_df
    })

    shiny::observeEvent(input$submit, {
      if (all(!core_fields %in% names(saved_inputs_df()))) {
        toast_messages_func("error", "Invalid file")
      }

      if (all(core_fields %in% names(saved_inputs_df()))) {
        waiter::waiter_show(html = shiny::tagList(waiter::spin_ring(), shiny::h4("Fetching data ...")))

        shinyWidgets::updatePickerInput(session, "country", selected = saved_inputs_df()$country)
        shinyWidgets::updatePickerInput(session, "groups", selected = unlist(strsplit(saved_inputs_df()$groups, ";")))
        shinyWidgets::updatePickerInput(session, "family", selected = saved_inputs_df()$family)
        shinyWidgets::updatePickerInput(session, "benchmark_median", selected = unlist(strsplit(saved_inputs_df()$benchmark_median, ";")))
        shinyWidgets::updatePrettyCheckbox(session, "benchmark_dots", value = saved_inputs_df()$benchmark_dots)
        shinyWidgets::updatePrettyCheckbox(session, "rank", value = saved_inputs_df()$rank)
        shinyWidgets::updatePickerInput(session, "threshold", selected = saved_inputs_df()$threshold)
        shinyWidgets::updatePrettyCheckbox(session, "preset_order", value = saved_inputs_df()$worst_to_best_order)

        removeModal()
        waiter::waiter_hide()

        shinyjs::show("save_inputs")
        shinyjs::disable("save_inputs")
        shinyjs::disable("download_data_1")
      }
    })

    ## Apply-selection side effects ---------------------------------------------
    observeEvent(input$select, {
      updatePickerInput(
        session, "countries_data",
        choices = c("All", "Base country only", "Base + comparison countries"),
        options = list(
          "All" = list(disabled = FALSE),
          "Base country only" = list(disabled = FALSE),
          "Base + comparison countries" = list(disabled = FALSE)
        )
      )

      toggleState("report", condition = input$select, shinyjs::disable("report"))
      toggleState("advreport", condition = input$select, shinyjs::disable("advreport"))
      toggleState("pptreport", condition = input$select, shinyjs::disable("pptreport"))
      toggleState("download_Coverage", condition = input$select, shinyjs::disable("download_Coverage"))
      toggleState("download_missing", condition = input$select, shinyjs::disable("download_missing"))
      toggleState("download_data_1", condition = input$select, shinyjs::disable("download_data_1"))
    }, ignoreNULL = TRUE)

    ## Custom groups --------------------------------------------------------------
    custom_group_fields_reactive <- reactive({
      n_fields <- input$custom_grps_count
      ui_fields <- c()

      lapply(1:n_fields, function(i) {
        custom_names <- isolate(input[[paste("custom_grps_names", i, sep = "_")]])
        custom_countries <- isolate(input[[paste("custom_grps_countries", i, sep = "_")]])

        ui_fields[[i]] <- shiny::fluidRow(
          width = 6,
          shiny::column(
            width = 6,
            shiny::textInput(ns(paste("custom_grps_names", i, sep = "_")), label = paste("Insert the name of group ", i), value = custom_names)
          ),
          shiny::column(
            width = 6,
            shinyWidgets::pickerInput(
              inputId = ns(paste("custom_grps_countries", i, sep = "_")),
              label = paste("Select countries that fall into group ", i),
              choices = c("", countries[!countries %in% input$country]),
              selected = custom_countries,
              multiple = TRUE,
              options = list(`actions-box` = TRUE, `live-search` = TRUE)
            )
          )
        )
      })
    })

    output$custom_grps <- renderUI({ custom_group_fields_reactive() })

    custom_grps_df <- shiny::eventReactive(input$save_custom_grps, {
      n_fields <- input$custom_grps_count
      if (n_fields > 0) {
        custom_grps_list <- lapply(1:n_fields, function(i) {
          grp_name <- as.character(input[[paste("custom_grps_names", i, sep = "_")]])
          country_selection <- as.vector(input[[paste("custom_grps_countries", i, sep = "_")]])
          if (!is.null(grp_name) & !is.null(country_selection)) {
            data.frame(Category = "Custom", Grp = grp_name, Countries = country_selection)
          } else NULL
        })
        custom_grps_df <- dplyr::bind_rows(custom_grps_list)
      } else {
        custom_grps_df <- NULL
      }
      if (!is.null(custom_grps_df) && nrow(custom_grps_df) == 0) custom_grps_df <- NULL
      custom_grps_df
    })

    shiny::observeEvent(input$save_custom_grps, {
      if (is.null(custom_grps_df())) {
        shinyWidgets::updatePrettyCheckbox(session, "create_custom_grps", value = FALSE)
      }
    })

    shiny::observeEvent(input$create_custom_grps, {
      if (input$create_custom_grps == TRUE) {
        shinyWidgets::updateMaterialSwitch(session, "show_countries", value = FALSE)
        shinyjs::disable(id = "show_countries")
      } else {
        shinyjs::enable(id = "show_countries")
      }
    })

    shiny::observeEvent(input$show_custom_grps, {
      if (input$show_custom_grps == TRUE) {
        shinyjs::show(id = "custom_grps_count"); shinyjs::show(id = "custom_grps"); shinyjs::show(id = "save_custom_grps")
      } else {
        shinyjs::hide(id = "custom_grps_count"); shinyjs::hide(id = "custom_grps"); shinyjs::hide(id = "save_custom_grps")
      }
    })

    shiny::observeEvent(input$save_custom_grps, {
      if (any(custom_grps_df()$Grp %in% unlist(group_list))) {
        dup_grp_names <- unique(custom_grps_df()$Grp[custom_grps_df()$Grp %in% unlist(group_list)])
        shiny::showModal(modalDialog(shiny::tagList(
          shiny::tags$p("The following list includes group name(s) that already exist(s) within the original group list. Please modify the group name(s) to continue."),
          shiny::tags$p(paste(as.character(dup_grp_names), collapse = " , "))
        )))
      } else {
        shinyWidgets::updateMaterialSwitch(session, "show_custom_grps", value = FALSE)

        Custom <- list(unique(custom_grps_df()$Grp))
        names(Custom) <- if (length(unique(custom_grps_df()$Grp)) == 1) unique(custom_grps_df()$Grp) else "Custom"

        shinyWidgets::updatePickerInput(session, "groups", choices = as.list(append(group_list, Custom)),
                                         selected = unique(c(input$groups, unique(custom_grps_df()$Grp))))
        shinyWidgets::updatePickerInput(session, "benchmark_median",
                                         choices = append("Comparison countries", append(group_list, Custom)),
                                         selected = unique(c(input$benchmark_median, custom_grps_df()$Grp))[1:3],
                                         options = list(`live-search` = TRUE, "max-options" = 3))
      }
    })

    shiny::observeEvent(input$create_custom_grps, {
      if (input$create_custom_grps == FALSE) {
        shinyWidgets::updatePickerInput(session, "groups", choices = group_list,
                                         selected = input$groups[!input$groups %in% unique(custom_grps_df()$Grp)])
        shinyWidgets::updatePickerInput(session, "benchmark_median", choices = append("Comparison countries", group_list),
                                         selected = input$benchmark_median[!input$benchmark_median %in% unique(custom_grps_df()$Grp)],
                                         options = list(`live-search` = TRUE, "max-options" = 3))
        updateCheckboxGroupButtons(session, "countries", label = NULL, choices = countries,
                                    checkIcon = list(yes = icon("ok", lib = "glyphicon", style = "color: #e94152")),
                                    selected = input$countries[!input$countries %in% unique(custom_grps_df()$Countries)])
      }
    })

    ## Comparison countries selector ---------------------------------------------
    observeEvent(input$groups, {
      selected_groups <- input$groups
      selected_country <- input$country

      if (is.null(selected_groups)) {
        selected <- NULL
      } else {
        selected <- country_list %>% filter(group %in% selected_groups) %>% select(country_name) %>% unique()
        if (!is.null(selected_country)) selected <- selected %>% filter(country_name != selected_country)
        selected <- selected %>% pluck(1)
      }

      updateCheckboxGroupButtons(session, "countries", label = NULL, choices = countries,
                                  checkIcon = list(yes = icon("ok", lib = "glyphicon", style = "color: #e94152")),
                                  selected = selected)
    }, ignoreNULL = FALSE)

    observeEvent(list(input$groups, input$save_custom_grps), {
      if (!is.null(custom_grps_df())) {
        custom_grp_countries <- custom_grps_df()$Countries[custom_grps_df()$Grp %in% input$groups]
        preselected_grp_countries <- country_list %>% filter(group %in% input$groups) %>% pull(country_name)

        selected_c <- if (length(preselected_grp_countries) > 0) {
          unique(c(custom_grp_countries, preselected_grp_countries))
        } else {
          unique(custom_grp_countries)
        }

        updateCheckboxGroupButtons(session, "countries", label = NULL, choices = countries,
                                    checkIcon = list(yes = icon("ok", lib = "glyphicon", style = "color: #e94152")),
                                    selected = selected_c)
      }
    })

    ## Apply-selection validation button -------------------------------------------
    output$select_button <- renderUI({
      if ((length(input$countries) + length(input$groups)) >= 10 && length(input$country) >= 1) {
        actionButton(ns("select"), "Apply selection", icon = icon("check"), status = "success", width = "100%",
                     shinyjs::enable("save_inputs"))
      } else {
        actionButton(ns("select"), "Select a base country and at least 10 comparison countries to apply selection",
                     icon = icon("triangle-exclamation"), status = "warning", width = "100%",
                     shinyjs::disable("report"), shinyjs::disable("advreport"), shinyjs::disable("pptreport"),
                     shinyjs::disable("download_missing"), shinyjs::disable("download_Coverage"),
                     shinyjs::disable("download_data_1"), shinyjs::disable("save_inputs"))
      }
    })

    observeEvent(input$countries, {
      toggleState("select", condition = length(input$countries) >= 10, shinyjs::disable("report"))
      toggleState("select", condition = length(input$countries) >= 10, shinyjs::disable("advreport"))
      toggleState("select", condition = length(input$countries) >= 10, shinyjs::disable("pptreport"))
      toggleState("select", condition = length(input$countries) >= 10, shinyjs::disable("download_missing"))
      toggleState("select", condition = length(input$countries) >= 10, shinyjs::disable("download_data_1"))
    }, ignoreNULL = FALSE)

    ## Reactive analysis objects ------------------------------------------------
    vars <- eventReactive(input$select, {
      if (input$family == "Overview") vars_all
      else variable_names %>% filter(family_name == input$family) %>% pull(variable) %>% unique()
    })

    note_compare <- eventReactive(input$select, {
      group_list_countries <- country_list %>% filter(group %in% input$groups) %>% pull(country_name)
      custom_df_countries <- NULL
      if (input$create_custom_grps == TRUE) {
        custom_df_countries <- custom_grps_df()$Countries[custom_grps_df()$Grp %in% input$groups &
                                                              custom_grps_df()$Countries %in% input$countries]
      }
      if (is.null(input$groups)) {
        input$countries
      } else if (all(unique(input$countries) %in% unique(c(group_list_countries, custom_df_countries)))) {
        input$groups
      } else {
        input$countries
      }
    })

    low_variance_indicators <- eventReactive(input$select, {
      global_data %>% low_variance(base_country(), country_list, input$countries, vars(), variable_names)
    })

    low_variance_indicators_dyn <- eventReactive(input$select, {
      global_data_dyn %>% low_variance_dyn(base_country(), country_list, input$countries, vars(), variable_names)
    })

    data_avg <- eventReactive(input$select, {
      static_avg_data <- global_data %>% select(-matches("_avg"))
      vars_static_avg_data <- names(static_avg_data)[6:length(static_avg_data)]
      static_avg <- compute_family_average(static_avg_data, vars_static_avg_data, "static", db_variables, base_country(), input$countries)
      static_avg <- static_avg %>% select(-matches("NA"))
      static_avg_data <- static_avg_data %>% left_join(static_avg, by = "country_code")
      static_avg_data %>% def_quantiles(base_country(), country_list, input$countries, vars_all, variable_names, input$threshold)
    })

    benchmark_data <- eventReactive(input$select, {
      global_data %>% def_quantiles(base_country(), country_list, input$countries, vars_all, variable_names, input$threshold)
    })

    data_dyn_avg <- eventReactive(input$select, {
      dynamic_avg_data <- global_data_dyn %>% select(-matches("_avg")) %>% filter(year %% 2 == 0)
      vars_dynamic_avg_data <- names(dynamic_avg_data)[6:length(dynamic_avg_data)]
      dynamic_avg <- compute_family_average(dynamic_avg_data, vars_dynamic_avg_data, "dynamic", db_variables, base_country(), input$countries)
      dynamic_avg <- dynamic_avg %>% select(-matches("NA")) %>% select(-matches("vars_other_avg"))
      dynamic_avg_data <- global_data_dyn %>% select(-matches("_avg")) %>% left_join(dynamic_avg, by = c("country_code", "year"))
      dynamic_avg_data %>% def_quantiles_dyn(base_country(), country_list, input$countries, vars_all, variable_names, input$threshold)
    })

    data_dyn <- eventReactive(input$select, {
      global_data_dyn %>% def_quantiles_dyn(base_country(), country_list, input$countries, vars_all, variable_names, input$threshold)
    })

    data_family <- eventReactive(input$select, {
      family_data(global_data, base_country(), variable_names, input$countries) %>%
        def_quantiles(base_country(), country_list, input$countries, vars_family, family_names, input$threshold)
    })

    data_family_dyn <- eventReactive(input$select, {
      family_data_dyn(global_data_dyn, base_country(), variable_names) %>%
        def_quantiles_dyn(base_country(), country_list, input$countries, vars_family, family_names, input$threshold)
    })

    na_indicators <- eventReactive(input$select, {
      global_data %>% ungroup() %>% filter(country_name == input$country) %>% select(where(is.na)) %>% names()
    })

    observeEvent(input$country, {
      sel_family <- if (nrow(saved_inputs_df()) > 0 && input$load_inputs == 1) saved_inputs_df()$family else NULL

      valid_vars <- ctf_long %>%
        filter(country_name == input$country, !is.na(value)) %>%
        select(family_name) %>% unique() %>% unlist() %>% unname()

      updatePickerInput(session, "family",
                         choices = c("Overview", intersect(names(variable_list), valid_vars)),
                         selected = sel_family)
    }, ignoreNULL = FALSE)

    data_family_median <- eventReactive(input$select, {
      family_data(global_data, base_country(), variable_names)
    })

    ## Static plot -----------------------------------------------------------------
    custom_df <- shiny::eventReactive(input$select, {
      if (input$create_custom_grps == TRUE) {
        custom_grps_df()[custom_grps_df()$Grp %in% input$benchmark_median & custom_grps_df()$Countries %in% input$countries, ]
      } else {
        NULL
      }
    })

    output$plot <- renderPlotly({
      tryCatch({
        if (length(input$countries) >= 10) {
          input$select
          isolate(
            if (input$family == "Overview") {
              data_family() %>%
                left_join(family_order, by = c("var_name" = "family_name")) %>%
                arrange(family_order, country_name) %>%
                static_plot(base_country(), input$family, input$rank, dots = input$benchmark_dots,
                            group_median = input$benchmark_median, custom_df = custom_df(),
                            threshold = input$threshold, preset_order = input$preset_order) %>%
                interactive_plot(input$family, plotly_remove_buttons, "static")
            } else {
              data_avg() %>%
                filter(variable %in% vars()) %>%
                static_plot(base_country(), input$family, input$rank, dots = input$benchmark_dots,
                            group_median = input$benchmark_median, custom_df = custom_df(),
                            threshold = input$threshold, preset_order = input$preset_order) %>%
                interactive_plot(input$family, plotly_remove_buttons, "static")
            }
          )
        }
      }, error = function(e) {
        showNotification("Data is missing for the selected base country or countries for the given indicator. Please try a different selection.", "", type = "message", duration = 30)
        return()
      })
    }) %>%
      bindCache(input$country, input$groups, input$family, input$benchmark_median, input$rank,
                input$benchmark_dots, input$preset_order, input$create_custom_grps,
                input$show_dynamic_plot, input$threshold, input$countries) %>%
      bindEvent(input$select)

    output$plot_notes <- renderUI({
      if (length(input$countries) >= 10) {
        input$select
        isolate(
          if (input$family == "Overview") {
            missing_variables <- global_data %>% missing_var(base_country(), country_list, input$countries, vars_all, variable_names)
            low_variance_variables <- low_variance_indicators() %>% data.frame() %>% rename("variable" = ".") %>%
              left_join(variable_names %>% select(variable, var_name), by = "variable") %>% .$var_name
            missing_variables <- c(missing_variables, low_variance_variables)

            plot_notes_function(base_country(), note_compare(), input$family, missing_variables, "static", custom_df = custom_df())
          } else {
            missing_variables <- global_data %>% missing_var(base_country(), country_list, input$countries, vars(), variable_names)
            low_variance_variables <- low_variance_indicators() %>% data.frame() %>% rename("variable" = ".") %>%
              left_join(variable_names %>% select(variable, var_name), by = "variable") %>% .$var_name
            missing_variables <- c(missing_variables, low_variance_variables)
            missing_variables <- missing_variables[!grepl("_avg", missing_variables)]

            plot_notes_function(base_country(), note_compare(), input$family, missing_variables, "static", custom_df = custom_df())
          }
        )
      }
    })

    ## Dynamic plot ------------------------------------------------------------
    shiny::observeEvent(
      list(input$country, input$groups, input$family, input$rank, input$benchmark_dots,
           input$create_custom_grps, input$threshold, input$preset_order, input$countries),
      {
        if (length(input$country) == 1) {
          shinyWidgets::updateMaterialSwitch(session, "show_dynamic_plot", value = FALSE)
        }
      }
    )

    output$dynamic_benchmark_plot <- renderPlotly({
      tryCatch({
        validate(need(length(input$country) == 1, "Dynamic Benchmarking is available only when One base Country is selected"))
        validate(need(!(input$family %in% family_order$family_name[family_order$Benchmark_dynamic_indicator == "No"]),
                      "No Dynamic Benchmarking Plot available for this family."))

        if (length(input$countries) >= 10 && length(input$country) == 1) {
          isolate(
            if (input$family == "Overview") {
              data_dyn_avg() %>%
                filter(str_detect(variable, "_avg")) %>%
                left_join(family_order, by = "family_name") %>%
                filter(Benchmark_dynamic_family_aggregate != "No") %>%
                static_plot_dyn(base_country(), input$family, input$rank, dots = input$benchmark_dots,
                                 group_median = input$benchmark_median, custom_df = custom_df(),
                                 threshold = input$threshold, preset_order = input$preset_order) %>%
                interactive_plot(input$family, plotly_remove_buttons, "dynamic")
            } else {
              plot_data <- data_dyn_avg()

              plot_data_1 <- plot_data %>%
                filter(str_detect(variable, "_avg")) %>%
                left_join(family_order, by = "family_name") %>%
                filter(Benchmark_dynamic_family_aggregate != "No")

              plot_data_2 <- plot_data %>%
                filter(!str_detect(variable, "_avg")) %>%
                left_join(family_order, by = "family_name")

              plot_data <- bind_rows(plot_data_1, plot_data_2)

              plot_data %>%
                filter(variable %in% vars()) %>%
                static_plot_dyn(base_country(), input$family, input$rank, dots = input$benchmark_dots,
                                 group_median = input$benchmark_median, custom_df = custom_df(),
                                 threshold = input$threshold, preset_order = input$preset_order) %>%
                interactive_plot(input$family, plotly_remove_buttons, "dynamic")
            }
          )
        }
      }, error = function(e) {
        showNotification("Data is insufficient for the selected base country. No Dynamic Plot was generated", "", type = "message", duration = 10)
        return()
      })
    }) %>%
      bindCache(input$country, input$groups, input$family, input$benchmark_median, input$rank,
                input$benchmark_dots, input$preset_order, input$create_custom_grps,
                input$show_dynamic_plot, input$threshold, input$countries) %>%
      bindEvent(input$select)

    ## Indicator definitions table --------------------------------------------------
    output$definition <- renderTable({
      shiny::req(input$family)

      variables <- db_variables %>%
        filter(var_level == "indicator" & benchmarked_ctf == "Yes" & family_var != "vars_other")

      if (input$family != "Overview") {
        variables <- variables %>% filter(family_name == input$family)
      }

      variables %>% select(Indicator = var_name, Family = family_name, Description = description, Source = source)
    })

    ## Reports -----------------------------------------------------------------
    output$report <- downloadHandler(
      filename = reactive(paste0("CLIAR-benchmarking-", base_country(), ".docx")),
      content = function(file) {
        show_modal_spinner(color = "#17a2b8", text = "Compiling report")
        on.exit(remove_modal_spinner())

        tmp_dir <- tempdir()
        tempReport <- file.path(tmp_dir, "report.Rmd")
        file.copy("www/", tmp_dir, recursive = TRUE)
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

        params <- list(
          base_country = base_country(), comparison_countries = input$countries, data = data_avg(),
          wb_country_list = country_list, family_data = data_family(), data_dyn = data_dyn(),
          data_dyn_avg = data_dyn_avg(), family_data_dyn = data_family_dyn(), rank = input$rank,
          definitions = definitions, variable_names = variable_names, dots = input$benchmark_dots,
          group_median = input$benchmark_median, threshold = input$threshold, family_order = family_order,
          global_data = global_data, download_opt = FALSE, compiled_indicators = raw_data, db_variables = db_variables
        )

        rmarkdown::render(tempReport, output_file = file, params = params,
                           envir = new.env(parent = globalenv()), knit_root_dir = getwd())
      }
    )

    output$advreport <- downloadHandler(
      filename = reactive(paste0("CLIAR-benchmarking-Advanced-Report-", base_country(), ".docx")),
      content = function(file) {
        show_modal_spinner(color = "#17a2b8", text = "Compiling report")
        on.exit(remove_modal_spinner())

        tmp_dir <- tempdir()
        tempReport <- file.path(tmp_dir, "report.Rmd")
        file.copy("www/", tmp_dir, recursive = TRUE)
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

        params <- list(
          base_country = base_country(), comparison_countries = input$countries, data = data_avg(),
          wb_country_list = country_list, family_data = data_family(), data_dyn = data_dyn(),
          data_dyn_avg = data_dyn_avg(), family_data_dyn = data_family_dyn(), rank = input$rank,
          definitions = definitions, variable_names = variable_names, dots = input$benchmark_dots,
          group_median = input$benchmark_median, threshold = input$threshold, family_order = family_order,
          global_data = global_data, download_opt = TRUE, compiled_indicators = raw_data, db_variables = db_variables
        )

        rmarkdown::render(tempReport, output_file = file, params = params,
                           envir = new.env(parent = globalenv()), knit_root_dir = getwd())
      }
    )

    output$download_Coverage <- downloadHandler(
      filename = reactive(paste0("Missing_data-", base_country(), ".docx")),
      content = function(file) {
        show_modal_spinner(color = "#17a2b8", text = "Compiling report")
        on.exit(remove_modal_spinner())

        tmp_dir <- tempdir()
        tempReport <- file.path(tmp_dir, "coverage-report.Rmd")
        file.copy("www/", tmp_dir, recursive = TRUE)
        file.copy("coverage-report.Rmd", tempReport, overwrite = TRUE)

        params <- list(
          ctf_static_long = ctf_long %>%
            left_join(db_variables %>% select(variable, var_name, family_var, family_name), by = "variable"),
          ctf_dynamic = year_ctf_dynamic,
          base_country = base_country()
        )

        rmarkdown::render(tempReport, output_file = file, params = params,
                           envir = new.env(parent = globalenv()), knit_root_dir = getwd())
      }
    )

    output$pptreport <- downloadHandler(
      filename = reactive(paste0("CLIAR-PPT-", base_country(), ".pptx")),
      content = function(file) {
        show_modal_spinner(color = "#17a2b8", text = "Compiling report")
        on.exit(remove_modal_spinner())

        ppt <- read_pptx("www/CLIAR_template.pptx")

        custom_df_ppt <- if (input$create_custom_grps == TRUE) {
          custom_grps_df()[custom_grps_df()$Grp %in% input$benchmark_median & custom_grps_df()$Countries %in% input$countries, ]
        } else NULL

        plot1 <- data_family() %>%
          left_join(family_order, by = c("var_name" = "family_name")) %>%
          arrange(country_name, family_order) %>%
          static_plot(base_country(), "Country overview", rank = input$rank, group_median = input$benchmark_median,
                      dots = input$benchmark_dots, custom_df = custom_df_ppt, title = FALSE,
                      threshold = input$threshold, report = TRUE)

        plot2 <- data_dyn_avg() %>%
          filter(str_detect(variable, "_avg")) %>%
          static_plot_dyn(base_country()[1], "Country overview", input$rank, dots = input$benchmark_dots,
                           group_median = input$benchmark_median, custom_df = custom_df_ppt,
                           threshold = input$threshold, title = FALSE)

        plot1 <- dml(ggobj = plot1)
        plot2 <- dml(ggobj = plot2)

        properties <- fp_text(color = "black", font.size = 20, bold = FALSE)
        text_1 <- ftext(paste0("Base Country : ", input$country), properties)
        text_2 <- ftext(paste0("Comparison Countries : ", paste(c(input$countries), collapse = ", ")), properties)

        ppt <- ppt %>%
          on_slide(index = 8) %>%
          ph_with(value = fpar(text_1), ph_location(left = 0.5, width = 12, top = 1.3, bg = "transparent")) %>%
          ph_with(value = fpar(text_2), ph_location(left = 0.5, width = 12, top = 1.8, bg = "transparent"))

        ppt <- ppt %>%
          on_slide(index = 9) %>%
          ph_with(value = plot1, location = ph_location(left = 1.5, top = 1.2, width = 10.04, height = 4.67, bg = "transparent"))

        slide_index <- 10

        family_n <- db_variables %>% distinct(family_name) %>% filter(!is.na(family_name)) %>% pull(family_name) %>% as.list()

        for (fam_n in family_order$family_name) {
          if (fam_n %in% family_n) {
            fam_variable_names <- variable_names %>% filter(family_name == fam_n) %>% pull(variable) %>% unique()

            plt_f <- data_avg() %>%
              filter(variable %in% fam_variable_names) %>%
              static_plot(base_country(), fam_n, input$rank, dots = input$benchmark_dots,
                          group_median = input$benchmark_median, custom_df = custom_df(),
                          threshold = input$threshold, preset_order = input$preset_order,
                          title = FALSE, report = TRUE)

            plt_f <- dml(ggobj = plt_f)

            ppt <- ppt %>%
              add_slide(master = "Custom Design") %>%
              on_slide(index = slide_index) %>%
              ph_with(value = fam_n, location = ph_location(left = 1, top = 0.4, width = 12)) %>%
              ph_with(value = plt_f, location = ph_location(left = 1.5, top = 1.2, width = 10.04, height = 4.67, bg = "transparent"))

            slide_index <- slide_index + 1
          }
        }

        ppt <- ppt %>%
          add_slide(master = "Custom Design") %>%
          on_slide(index = slide_index) %>%
          ph_with(value = "Dynamic Benchmarking : Overview", location = ph_location(left = 1, top = 0.4, width = 12)) %>%
          ph_with(value = plot2, location = ph_location(left = 1.5, top = 1.2, width = 10.04, height = 4.67, bg = "transparent"))

        print(ppt, file)
      }
    )

    ## Save/apply-selection inputs -----------------------------------------------
    cliar_inputs <- eventReactive(input$select, {
      cliar_inputs <- data.frame(
        country = input$country,
        groups = paste(c(input$groups), collapse = ";"),
        family = input$family,
        benchmark_median = paste(c(input$benchmark_median), collapse = ";"),
        benchmark_dots = input$benchmark_dots,
        rank = input$rank,
        threshold = input$threshold,
        worst_to_best_order = input$preset_order,
        comparison_countries = paste(c(input$countries), collapse = ";"),
        create_custom_groups = input$create_custom_grps
      )

      if (input$create_custom_grps == TRUE) {
        cliar_inputs$no_custom_grps <- input$custom_grps_count
        for (i in 1:input$custom_grps_count) {
          cliar_inputs[, paste("custom_grps_names", i, sep = "_")] <- input[[paste("custom_grps_names", i, sep = "_")]]
          cliar_inputs[, paste("custom_grps_countries", i, sep = "_")] <-
            paste(input[[paste("custom_grps_countries", i, sep = "_")]], collapse = ";")
        }
      }

      cliar_inputs
    })

    shiny::observeEvent(input$select, {
      shinyjs::show("download_data_1")
      shinyjs::enable("download_data_1")
    })

    output$save_inputs <- downloadHandler(
      filename = function() paste("cliar_inputs.rds"),
      content = function(file) saveRDS(cliar_inputs(), file)
    )

    observeEvent(input$family, {
      if (input$family == "SOE Corporate Governance" || input$family == "Labor and Social Protection Institutions") {
        shinyjs::hide("download_data_1")
      } else {
        shinyjs::show("download_data_1")
      }
    })

    download_data_1 <- eventReactive(input$select, {
      data1 <- data_family() %>% filter(country_name == base_country())
      data2 <- benchmark_data() %>% filter(country_name == base_country())
      data3 <- data_family_dyn() %>% filter(country_name == base_country())
      data4 <- data_dyn_avg() %>% filter(country_name == base_country()) %>% filter(variable != "wdi_nygdppcapppkd")

      list(data1 = data1, data2 = data2, data3 = data3, data4 = data4)
    })

    output$download_data_1 <- downloadHandler(
      filename = function() paste0("CTF-plot-data.xlsx"),
      content = function(file) {
        show_modal_spinner(color = "#17a2b8", text = "Compiling Data")
        on.exit(remove_modal_spinner())

        data <- download_data_1()

        wb <- createWorkbook()
        sheet1 <- addWorksheet(wb, "Static Overview")
        sheet2 <- addWorksheet(wb, "Static Family")
        sheet3 <- addWorksheet(wb, "Dynamic Overview")
        sheet4 <- addWorksheet(wb, "Dynamic Family")

        writeData(wb, sheet1, data$data1, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
        writeData(wb, sheet2, data$data2, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
        writeData(wb, sheet3, data$data3, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
        writeData(wb, sheet4, data$data4, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)

        saveWorkbook(wb, file)
      }
    )

    ## Return shared state for other modules --------------------------------------
    list(
      base_country     = base_country,          # gated by "Apply selection"
      live_country     = reactive(input$country), # live, ungated
      countries        = reactive(input$countries),
      groups           = reactive(input$groups),
      custom_groups_df = custom_grps_df
    )
  })
}
