Sys.setenv(lang = "en_US")
options(shiny.sanitize.errors = FALSE, scipen = 999, dplyr.summarise.inform = FALSE)#warn = -1, 
rm(list = ls())
#setwd('C:/Projects/wappapp/')
files_to_exclude <- c('app.R', 'theme.css')

for (i in list.files(recursive = TRUE)) {
  if (!i %in% files_to_exclude & !stringr::str_detect(i, 'data\\/')) {
    source(i)
  }
}

shinyApp(ui, server)