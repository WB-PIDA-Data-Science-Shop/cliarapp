mod_scatter_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "scatter",

    bs4Card(
      title = "Select indicators to visualize",
      status = "success",
      solidHeader = TRUE,
      width = 12,

      fluidRow(
        column(
          width = 3,
          pickerInput(
            ns("country_scatter"),
            label = "Select a base country",
            choices = c("", countries),
            selected = NULL,
            multiple = FALSE,
            options = list(`actions-box` = TRUE, `live-search` = TRUE)
          )
        ),
        column(
          width = 3,
          pickerInput(
            ns("y_scatter"),
            label = "Select indicator for Y axis",
            choices = y_scatter_choices,
            selected = NULL,
            options = list(`live-search` = TRUE, title = "Click to select family or indicator"),
            width = "100%"
          )
        ),
        column(
          width = 3,
          pickerInput(
            ns("x_scatter"),
            label = "Select indicator for X axis",
            choices = NULL,
            selected = NULL,
            options = list(`live-search` = TRUE, title = "Click to select family or indicator"),
            width = "100%"
          )
        ),
        column(
          width = 3,
          pickerInput(
            ns("high_group"),
            label = "Highlight a group",
            choices = append("No highlight", group_list),
            selected = NULL,
            multiple = FALSE,
            options = list(`live-search` = TRUE, `actions-box` = TRUE)
          )
        )
      ),
      fluidRow(
        column(
          width = 3,
          shinyWidgets::materialSwitch(inputId = ns("linear_fit"), label = "Show linear fit line", value = FALSE, status = "success")
        ),
        column(width = 6),
        column(
          width = 2.4,
          shinyjs::hidden(downloadButton(ns("download_bivariate_data"), "Download Chart Data",
                                          style = "width:100%; background-color: #204d74; color: white"))
        )
      )
    ),

    bs4Card(
      title = "Select individual comparison countries",
      width = 12,
      status = "success",
      collapsed = TRUE,

      checkboxGroupButtons(
        inputId = ns("countries_scatter"),
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
        column(width = 4, colourInput(ns("color_base_scatter"), "Choose a base country color:", value = "#f29411")),
        column(width = 4, colourInput(ns("color_comp_scatter"), "Choose a comparison country color:", value = "#080770"))
      )
    ),

    conditionalPanel(
      'input.y_scatter !== ""', ns = ns,
      bs4Card(
        width = 12,
        solidHeader = FALSE,
        gradientColor = "primary",
        collapsible = FALSE,
        plotlyOutput(ns("scatter_plot"), height = paste0(plot_height * 1.6, "px"))
      )
    )
  )
}

mod_scatter_server <- function(id,
                                shared,
                                display_country,
                                countries,
                                group_list,
                                country_list,
                                global_data,
                                variable_names,
                                db_variables,
                                x_scatter_choices,
                                plotly_remove_buttons,
                                plot_height) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Cross-tab base-country sync --------------------------------------------------
    observeEvent(display_country(), {
      req(!identical(input$country_scatter, display_country()))
      updatePickerInput(session, "country_scatter", selected = display_country())
    }, ignoreNULL = FALSE)

    observeEvent(input$country_scatter, {
      display_country(input$country_scatter)
    }, ignoreInit = TRUE)

    ## X-axis choices depend on Y selection ------------------------------------------
    observeEvent(input$y_scatter, {
      shiny::req(input$y_scatter)
      updatePickerInput(session, inputId = "x_scatter", choices = x_scatter_choices(input$y_scatter))
    })

    ## Custom groups feeding the highlight picker -------------------------------------
    custom_df_scatter <- reactive({
      if (any(!input$high_group %in% unlist(group_list))) {
        shared$custom_groups_df()[shared$custom_groups_df()$Grp %in% input$high_group, ]
      } else {
        NULL
      }
    })

    high_group <- reactive({
      # render the plot if the highlight group is not selected (i.e., null)
      if (is.null(input$high_group) || input$high_group == "No highlight") {
        return(country_list[0, ] %>% select(group, country_name))
      }

      high_group_df <- country_list %>%
        filter(group %in% input$high_group) %>%
        select(group, country_name)

      if (!is.null(custom_df_scatter()) && any(input$high_group %in% custom_df_scatter()$Grp)) {
        custom_df_data <- custom_df_scatter() %>%
          filter(Grp %in% input$high_group) %>%
          select(Grp, Countries) %>%
          rename(group = Grp, country_name = Countries) %>%
          left_join(country_list %>% select(country_name), by = "country_name")

        high_group_df <- bind_rows(high_group_df, custom_df_data)
      }

      high_group_df
    })

    ## Comparison-country availability -----------------------------------------------
    filtered_countries_scatter <- reactive({
      req(input$y_scatter, input$x_scatter)
      countries %>%
        .[!sapply(., function(country) check_data(global_data, country, input$y_scatter, input$x_scatter))]
    })

    observeEvent(list(input$country_scatter, input$x_scatter), {
      available_countries_scatter <- na.omit(filtered_countries_scatter())
      updateCheckboxGroupButtons(
        session, "countries_scatter",
        choices = available_countries_scatter,
        checkIcon = list(yes = icon("ok", lib = "glyphicon", style = "color: #00000")),
        selected = intersect(input$countries_scatter, available_countries_scatter)
      )
    })

    ## Follow Benchmarking tab's comparison countries --------------------------------
    observeEvent(shared$countries(), {
      updateCheckboxGroupButtons(session, "countries_scatter", choices = countries,
                                  checkIcon = list(yes = icon("ok", lib = "glyphicon", style = "color: #e94152")),
                                  selected = shared$countries())
    })

    ## Plot --------------------------------------------------------------------------
    scatter_result <- reactive({
      shiny::req(input$y_scatter, input$x_scatter)
      static_scatter(
        global_data,
        input$country_scatter,
        input$countries_scatter,
        high_group(),
        input$y_scatter,
        input$x_scatter,
        variable_names,
        country_list,
        input$linear_fit,
        input$color_base_scatter,
        input$color_comp_scatter
      )
    })

    output$scatter_plot <- renderPlotly({
      shiny::req(input$y_scatter, input$x_scatter)

      validate(need(
        check_data(global_data, input$country_scatter, input$y_scatter, input$x_scatter) == FALSE,
        "Country Comparison is not available for this Indicator for the selected base country"
      ))

      scatter_result()$sc_plot %>%
        interactive_scatter(input$y_scatter, input$x_scatter, db_variables, high_group(), plotly_remove_buttons)
    })

    ## Download chart data -------------------------------------------------------------
    observe({
      inputs_not_blank <- input$country_scatter != "" && input$y_scatter != "" && input$x_scatter != ""
      condition <- inputs_not_blank &&
        check_data(global_data, input$country_scatter, input$y_scatter, input$x_scatter) == FALSE

      if (condition) shinyjs::show("download_bivariate_data") else shinyjs::hide("download_bivariate_data")
    })

    output$download_bivariate_data <- downloadHandler(
      filename = function() paste0("CLIAR Bivariate Analysis-", input$country_scatter, " - data.csv"),
      content = function(file) {
        show_modal_spinner(color = "#17a2b8", text = "Loading Data")
        on.exit(remove_modal_spinner())

        write_csv(scatter_result()$sc_data, file, na = "")
      }
    )

    list(
      country_scatter = reactive(input$country_scatter)
    )
  })
}
