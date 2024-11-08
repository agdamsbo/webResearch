# Prep for shiny
# system2("cat ./R/index_from_raw.R ./R/plot_index.R ./R/read_file.R > ./R/functions.R")

project.aid::merge_scripts(list.files("R/",full.names = TRUE),dest = here::here("app/functions.R"))

# Typical shiny
shiny::runApp(appDir = here::here("app/"), launch.browser = TRUE)

project.aid::deploy_shiny(
  files = c("server.R", "ui.R"),
  account.name = "agdamsbo",
  name.app = "webResearch",
  name.token = "rsconnect_agdamsbo_token",
  name.secret = "rsconnect_agdamsbo_secret"
)
