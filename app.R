source("global.R")

source("modules/mod_benchmark.R")
source("modules/mod_country_compare.R")
source("modules/mod_scatter.R")
source("modules/mod_trends.R")
source("modules/mod_world_map.R")
source("modules/mod_data.R")

source("ui.R")
source("server.R")

shinyApp(ui, server)
