deploy_cliarapp <- function(type = c("dev", "prod")){
  type_deployment <- match.arg(type)

  switch(
    type_deployment,
    dev = rsconnect::deployApp(
      appId = Sys.getenv("dev_guid")
    ),
    prod = rsconnect::deployApp(
      appId = Sys.getenv("prod_guid")
    )
  )
}