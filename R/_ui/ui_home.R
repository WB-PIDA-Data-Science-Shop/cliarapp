# UI - Home Tab
# Landing page for the CLIAR Benchmarking Dashboard

ui_home_tab <- function() {
  tabItem(
    tabName = "home",
    
    bs4Card(
      width = 12,
      status = "navy",
      solidHeader = TRUE,
      title =
        span(
          img(src = "cliar.png", width = "80%")
        ),
      
      br(),            
      p("Welcome to the Country Level Institutional Assessment and Review (CLIAR) Benchmarking Dashboard!"), 
      
      p("The CLIAR Benchmarking Dashboard provides a standard quantitative methodology to summarize information from a large set of country-level institutional indicators."),
      
      p("For full details about the methodology behind the CLIAR Benchmarking, please find the Methodological paper in the Methodology tab. Users of this resource should cite this paper. Publications using the CLIAR data should include a citation of the CLIAR Dashboard as well as the original source(s) of the data used. Citation information for each component dataset is also included in the Methodology page."),
      h3("How to use this dashboard"),
      p("This dashboard enables its users to interact with the CLIAR benchmarking through the following tabs:"),
      tags$ul(
        tags$li(
          "The ",
          tags$b("Country Benchmarking"),
          "tab shows how one country compares to another group of countries in terms of closeness to frontier for each relevant indicator and institutional cluster.
                It works best with a relatively large group of comparator countries."
        ),
        tags$li(
          "The ",
          tags$b("Cross-Country Comparison "),
          "tab shows how one country compares to another group of countries for each relevant indicator.
                It works even with a few comparator countries."
        ),
        tags$li(
          "The",
          tags$b("Bivariate Correlation"),
          "tab shows correlations between the closeness to frontier scores for pairs of indicators"
        ),
        tags$li(
          "The ",
          tags$b("World Map"),
          "tab shows the closeness to frontier of a given indicator for all countries with available data."
        ),
        tags$li(
          "The ",
          tags$b("Time Trends"),
          "tab shows the evolution year by year of multiple indicators."
        ),
        tags$li(
          "The ",
          tags$b("Data"),
          "tab provides an interactive table containing the closeness to frontier data for all countries.
                It also allows users to download the data in different formats."
        ),
        tags$li(
          "The ",
          tags$b("Methodology & User Guide"),
          "tab includes metadata on the indicators, country groups and methods used in the analysis, and FAQs."
        ),
        tags$li(
          "The ",
          tags$b("Terms of Use and Disclaimers"),
          "tab provides more information about the terms of use and disclaimers, as well as citation information."
        ),
        tags$li(
          "The ",
          tags$b("FAQ"),
          "tab shows and answers the most frequently asked questions about CLIAR."
        ),
        tags$li(
          "The ",
          tags$b("Contact Us"),
          "tab allows users to directly contact us to CLIAR@worldbank.org"
        ),
        tags$li(
          "The ",
          tags$b("Source Code"),
          "tab takes users to our GitHub repository where they can access our source code."
        )
      ),
      p("Disclaimer :The findings, interpretations, and conclusions expressed in CLIAR are a product of staff of the World Bank, but do not necessarily reflect the views of the World Bank and its affiliated organizations, or those of the Executive Directors of the World Bank or the governments they represent. Moreover, country borders or names used and available in this dashboard do not necessarily reflect the World Bank Group's official position, and do not imply the expression of any opinion on the part of the World Bank, concerning the legal status of any country or territory or concerning the delimitation of frontiers or boundaries. The term country, used interchangeably with economy, does not imply political independence but refers to any territory for which authorities report separate social or economic statistics.")
      
    )
  )
}
