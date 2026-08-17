mod_country_compare_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "country",

    bs4Card(
      title = "Select information to display",
      status = "success",
      solidHeader = TRUE,
      width = 12,

      fluidRow(
        column(
          width = 3,
          pickerInput(ns("country_bar"), label = "Select a base country",
                      choices = c("", countries), selected = NULL, multiple = FALSE)
        ),
        column(
          width = 3,
          pickerInput(
            inputId = ns("groups_bar"),
            label = "Select comparison groups",
            choices = group_list,
            selected = NULL,
            multiple = TRUE,
            options = list(`live-search` = TRUE, `actions-box` = TRUE)
          )
        ),
        column(
          width = 3,
          pickerInput(
            ns("vars_bar"),
            label = "Select indicator",
            choices = variable_list_benchmarked,
            selected = NULL,
            options = list(`actions-box` = TRUE, `live-search` = TRUE, "max-options" = 3),
            width = "100%"
          )
        ),
        column(
          width = 3,
          radioGroupButtons(
            ns("value_bar"),
            label = "Select data source",
            choices = c("Closeness to frontier" = "ctf", "Original indicator" = "raw"),
            justified = TRUE,
            selected = "ctf",
            checkIcon = list(yes = icon("ok", lib = "glyphicon"))
          )
        )
      )
    ),

    bs4Card(
      title = "Select individual comparison countries",
      width = 12,
      status = "success",
      collapsed = TRUE,

      checkboxGroupButtons(
        inputId = ns("countries_bar"),
        individual = TRUE,
        label = NULL,
        choices = countries,
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      )
    ),

    bs4Card(
      title = "Select Bar Graph Colors",
      status = "success",
      collapsed = TRUE,
      width = 12,

      fluidRow(
        column(width = 4, colourInput(ns("color_base_bar"), "Choose a base country color:", value = "#f29411")),
        column(width = 4, colourInput(ns("color_comp_bar"), "Choose a comparison country color:", value = "#080770")),
        column(width = 4, colourInput(ns("color_groups_bar"), "Choose a comparison group color:", value = "#808080"))
      )
    ),

    conditionalPanel(
      'input.country_bar !== "" && input.vars_bar != null', ns = ns,
      bs4Card(
        width = 12,
        solidHeader = FALSE,
        gradientColor = "primary",
        collapsible = FALSE,
        plotlyOutput(ns("bar_plot"), height = paste0(plot_height * 1.6, "px"))
      )
    )
  )
}

mod_country_compare_server <- function(id,
                                        shared,            # list returned by mod_benchmark_server()
                                        display_country,   # app-level reactiveVal shared with scatter & trends
                                        countries,
                                        group_list,
                                        global_data,
                                        raw_data,
                                        variable_names,
                                        db_variables,
                                        plotly_remove_buttons,
                                        plot_height) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Keep this tab's base-country picker in sync with the other tabs --------------
    observeEvent(display_country(), {
      req(!identical(input$country_bar, display_country()))
      updatePickerInput(session, "country_bar", selected = display_country())
    }, ignoreNULL = FALSE)

    observeEvent(input$country_bar, {
      display_country(input$country_bar)
    }, ignoreInit = TRUE)

    ## Also follow the Benchmarking tab's base country / groups when it changes -----
    observeEvent(shared$base_country(), {
      if (length(shared$base_country()) <= 1) {
        updatePickerInput(session, "country_bar", selected = shared$base_country())
      }
    })

    observeEvent(shared$groups(), {
      updatePickerInput(session, "groups_bar", selected = shared$groups())
    })

    observeEvent(shared$countries(), {
      updateCheckboxGroupButtons(session, "countries_bar", choices = countries,
                                  checkIcon = list(yes = icon("ok", lib = "glyphicon", style = "color: #00000")),
                                  selected = shared$countries())
    })

    ## Custom groups passthrough --------------------------------------------------
    custom_df_bar <- reactive({
      if (any(!input$groups_bar %in% unlist(group_list))) {
        shared$custom_groups_df()[shared$custom_groups_df()$Grp %in% input$groups_bar, ]
      } else {
        NULL
      }
    })

    ## Dataset selection (CTF vs raw) ----------------------------------------------
    bar_data <- reactive({
      if (input$value_bar == "ctf") {
        global_data
      } else {
        raw_data %>%
          select(-Year) %>%
          group_by(country_code, country_name, income_group, region) %>%
          fill(everything()) %>%
          slice(n())
      }
    })

    ## Comparison-country availability -----------------------------------------------
    filtered_countries_bar <- reactive({
      req(input$vars_bar)
      countries[!sapply(countries, function(country) check_data(bar_data(), country, input$vars_bar))]
    })

    observeEvent(input$vars_bar, {
      available_countries_bar <- na.omit(filtered_countries_bar())
      updateCheckboxGroupButtons(session, "countries_bar", choices = available_countries_bar,
                                  checkIcon = list(yes = icon("ok", lib = "glyphicon", style = "color: #00000")),
                                  selected = input$countries_bar)
    })

    ## Plot ------------------------------------------------------------------------
    output$bar_plot <- renderPlotly({
      validate(need(
        check_data(global_data, input$country_bar, input$vars_bar) == FALSE,
        "Country Comparison is not available for this Indicator for the selected base country"
      ))

      static_bar(
        bar_data(),
        input$country_bar,
        input$countries_bar,
        input$groups_bar,
        input$vars_bar,
        variable_names,
        custom_df_bar(),
        input$color_base_bar,
        input$color_comp_bar,
        input$color_groups_bar
      ) %>%
        interactive_bar(input$vars_bar, db_variables, plotly_remove_buttons)
    })

    ## Return this tab's picked indicator/country in case other modules need it later
    list(
      country_bar = reactive(input$country_bar),
      vars_bar    = reactive(input$vars_bar)
    )
  })
}
