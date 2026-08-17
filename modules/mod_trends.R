mod_trends_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "trends",

    box(
      width = 12,
      solidHeader = TRUE,
      title = "Select indicator to visualize",
      status = "success",
      collapsible = TRUE,

      fluidRow(
        column(
          width = 3,
          pickerInput(
            ns("country_trends"),
            label = "Select a base country",
            choices = c("", countries),
            selected = NULL,
            multiple = FALSE,
            options = list(`live-search` = TRUE)
          )
        ),
        column(
          width = 4,
          pickerInput(
            ns("group_trends"),
            label = "Select comparison groups",
            choices = group_list,
            selected = NULL,
            multiple = TRUE,
            options = list("max-options" = 5, `live-search` = TRUE)
          )
        ),
        column(
          width = 5,
          pickerInput(
            ns("vars_trends"),
            label = "Select indicator to visualize",
            choices = filtered_variable_list,
            selected = NULL,
            options = list(`live-search` = TRUE, title = "Click to select family or indicator"),
            width = "100%"
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
        inputId = ns("countries_trends"),
        individual = TRUE,
        label = NULL,
        choices = countries,
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      )
    ),

    bs4Card(
      title = "Select Time Trend Colors",
      status = "success",
      collapsed = TRUE,
      width = 12,

      fluidRow(
        column(width = 4, colourInput(ns("color_base_trends"), "Choose a base country color:", value = "#f29411")),
        column(width = 4, colourInput(ns("color_comp_trends"), "Choose a comparison country color:", value = "#080770")),
        column(width = 4, colourInput(ns("color_groups_trends"), "Choose a comparison group color:", value = "#808080"))
      )
    ),

    conditionalPanel(
      'input.vars_trends !== null && input.country_trends != ""', ns = ns,
      bs4Card(
        width = 12,
        solidHeader = FALSE,
        gradientColor = "primary",
        collapsible = FALSE,
        plotlyOutput(ns("time_series"), height = paste0(plot_height * 1.6, "px"))
      )
    )
  )
}

mod_trends_server <- function(id,
                               shared,
                               display_country,
                               countries,
                               group_list,
                               raw_data,
                               country_list,
                               db_variables,
                               plot_height) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Cross-tab base-country sync --------------------------------------------------
    observeEvent(display_country(), {
      req(!identical(input$country_trends, display_country()))
      updatePickerInput(session, "country_trends", selected = display_country())
    }, ignoreNULL = FALSE)

    observeEvent(input$country_trends, {
      display_country(input$country_trends)
    }, ignoreInit = TRUE)

    ## Follow Benchmarking tab's groups / countries -----------------------------------
    observeEvent(shared$groups(), {
      updatePickerInput(session, "group_trends", selected = shared$groups())
    })

    ## Custom groups -----------------------------------------------------------------
    custom_df_trend <- reactive({
      if (any(!input$group_trends %in% unlist(group_list))) {
        shared$custom_groups_df()[shared$custom_groups_df()$Grp %in% input$group_trends, ]
      } else {
        NULL
      }
    })

    ## Comparison-country availability, based on year coverage ------------------------
    filtered_countries_trends <- reactive({
      req(input$vars_trends, input$country_trends)

      fullvar <- db_variables %>%
        filter(var_name == input$vars_trends) %>%
        select(variable) %>%
        pull()

      filter_years <- raw_data %>%
        filter(country_name == input$country_trends, !is.na(get(fullvar))) %>%
        summarise(min = min(Year, na.rm = TRUE), max = max(Year, na.rm = TRUE))

      trends_start <- filter_years %>% pull(min)
      trends_end <- filter_years %>% pull(max)

      countries %>%
        .[!sapply(., function(country) trends_check_data(trends_start, trends_end, country, fullvar))]
    })

    observeEvent(list(input$country_trends, input$vars_trends), {
      available_countries_trends <- na.omit(filtered_countries_trends())
      updateCheckboxGroupButtons(
        session, "countries_trends",
        choices = available_countries_trends,
        checkIcon = list(yes = icon("ok", lib = "glyphicon", style = "color: #00000")),
        selected = intersect(input$countries_trends, available_countries_trends)
      )
    })

    ## Plot ----------------------------------------------------------------------------
    output$time_series <- renderPlotly({
      shiny::req(input$country_trends, input$vars_trends)

      validate(need(
        check_data(raw_data, input$country_trends, input$vars_trends) == FALSE,
        "Country Comparison is not available for this Indicator for the selected base country"
      ))

      if (input$vars_trends != "") {
        var <- db_variables %>% filter(var_name == input$vars_trends) %>% pull(variable)

        trends_plot(
          raw_data,
          var,
          input$vars_trends,
          input$country_trends,
          input$countries_trends,
          country_list,
          input$group_trends,
          db_variables,
          custom_df_trend(),
          input$color_base_trends,
          input$color_comp_trends,
          input$color_groups_trends
        )
      }
    })

    list(
      country_trends = reactive(input$country_trends)
    )
  })
}
