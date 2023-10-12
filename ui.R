ui <- tagList(dashboardPage(
  dashboardHeader(
    title = 'Warsaw Apartments Helper',
    titleWidth = 300,
    tags$li(a(href = 'https://www.linkedin.com/in/yauheni-semianiuk', icon("linkedin"), title = "", target="_blank"), class = "dropdown"),
    tags$li(a(href = 'https://github.com/yauheni-se', icon("github"), title = "", target="_blank"), class = "dropdown")
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