mod_world_map_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "world_map",

    box(
      width = 12,
      solidHeader = TRUE,
      title = "Select information to display",
      status = "success",
      collapsible = TRUE,

      fluidRow(
        column(
          width = 5,
          pickerInput(
            ns("vars_map"),
            label = "Select indicator",
            choices = variable_list,
            selected = NULL,
            options = list(`live-search` = TRUE, title = "Click to select family or indicator"),
            width = "100%"
          )
        ),
        column(
          width = 3,
          radioGroupButtons(
            ns("countries_map"),
            label = "Select countries to display",
            choices = c("All" = FALSE, "Base + comparison countries" = TRUE),
            justified = TRUE,
            selected = FALSE,
            checkIcon = list(yes = icon("ok", lib = "glyphicon"))
          )
        ),
        column(
          width = 4,
          radioGroupButtons(
            ns("value_map"),
            label = "Select data source",
            choices = c("Closeness to frontier" = "ctf", "Original indicator" = "raw"),
            justified = TRUE,
            selected = "ctf",
            checkIcon = list(yes = icon("ok", lib = "glyphicon"))
          )
        )
      )
    ),

    conditionalPanel(
      "input.vars_map !== ''", ns = ns,
      bs4Card(
        width = 12,
        solidHeader = FALSE,
        gradientColor = "primary",
        collapsible = FALSE,
        plotlyOutput(ns("map"), height = paste0(plot_height, "px")) %>%
          shinycssloaders::withSpinner(color = "#051f3f", type = 8)
      )
    )
  )
}

mod_world_map_server <- function(id,
                                  shared,
                                  variable_names,
                                  db_variables,
                                  spatial_data,
                                  plotly_remove_buttons,
                                  plot_height) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Disable raw-indicator toggle for averaged/aggregate indicators -----------------
    observeEvent(input$vars_map, {
      if (grepl("Average", input$vars_map)) {
        disable(selector = paste0("#", ns("value_map"), " button:eq(1)"))
      } else {
        enable(selector = paste0("#", ns("value_map"), " button:eq(1)"))
      }
    })

    ## Plot ----------------------------------------------------------------------------
    output$map <- renderPlotly({
      validate(need(
        check_spatial_data(spatial_data, input$vars_map) == FALSE,
        "Map is not available for this Indicator for the selected base country"
      ))

      if (input$vars_map != "") {
        var_selected <- variable_names %>% filter(var_name == input$vars_map) %>% pull(variable)

        static_map(
          input$value_map,
          var_selected,
          input$vars_map,
          input$countries_map,
          shared$base_country(),
          shared$countries()
        ) %>%
          interactive_map(var_selected, db_variables, plotly_remove_buttons, input$value_map)
      }
    })
  })
}
