Sys.setenv(lang = "en_US")
options(shiny.sanitize.errors = FALSE, scipen = 999, dplyr.summarise.inform = FALSE)#warn = -1, 
rm(list = ls())
#setwd('C:/Projects/wappapp/')
setwd('/srv/connect/apps/wappapp/')
files_to_exclude <- c('theme.css', 'wappapp.png', 'README.md', 'ui.R')

for (i in list.files(recursive = TRUE)) {
  if (!i %in% files_to_exclude & !stringr::str_detect(i, 'data\\/')) {
    source(i)
  }
}

ui <- tagList(dashboardPage(
  dashboardHeader(
    title = 'Warsaw Apartments Helper',
    titleWidth = 300,
    tags$li(a(href = 'https://www.linkedin.com/in/yauheni-semianiuk', icon("linkedin"), title = "", target = "_blank"), class = "dropdown"),
    tags$li(a(href = 'https://github.com/yauheni-se/wappapp', icon("github"), title = "", target = "_blank"), class = "dropdown")
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Forecast price', tabName = 'forecast', icon = icon('calculator'))
      # add here
    )
  ),
  dashboardBody(
    useShinyjs(),
    shinyDashboardThemes(theme = 'poor_mans_flatly'),
    tags$head(includeCSS('theme.css')),
    tags$head(tags$style(HTML('.content-wrapper {overflow: auto; background-color: #F5F5F5;}'))),
    tabItems(
      ForecastUI('forecast')
      # add here
    )
  ),
  skin = 'black'
))