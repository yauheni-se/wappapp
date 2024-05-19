ComparisonUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = 'comparison',
    # Input ----
    box(
      width = 12,
      title = "Links to the otodom's offers",
      solidHeader = TRUE,
      status = 'primary',
      fluidRow(
        column(
          width = 4,
          textInput(ns('url1'), 'Link 1', value = "https://www.otodom.pl/pl/oferta/royal-wilanow-123-m-4-pokoje-ID4n4YE")
        ),
        column(
          width = 4,
          textInput(ns('url2'), 'Link 2', value = 'https://www.otodom.pl/pl/oferta/przytulne-mieszkanie-dla-rodziny-ID4nuUb')
        ),
        column(
          width = 4,
          textInput(ns('url3'), 'Link 3', value = 'https://www.otodom.pl/pl/oferta/jedyny-taki-apartament-2pok-gabinet-jak-nowy-ID4qsDF')
        )
      ),
      fluidRow(
        column(
          width = 2,
          offset = 5,
          actionBttn(ns("compare_apartments"), "Compare apartments", block = TRUE, color = 'warning', style = 'simple')
        )
      ),
      br(),
      fluidRow(
        id = ns('progress_bar_row'),
        column(
          width = 12,
          progressBar(ns('progress_bar'), value = 0, title = '', display_pct = TRUE, status = 'custom'),
          tags$style('.progress-bar-custom {background-color: #FEAB3A;')
        )
      )
    ),
    
    # Images ----
    box(
      width = 4,
      title = 'Images',
      solidHeader = TRUE,
      collapsible = TRUE,
      fluidRow(column(width = 12, htmlOutput(ns('image_1')))),
      br(),
      fluidRow(
        column(width = 6, actionBttn(ns('prev_1'), label = '<<<', style = 'simple', color = 'warning', block = TRUE)),
        column(width = 6, actionBttn(ns('next_1'), label = '>>>', style = 'simple', color = 'warning', block = TRUE))
      )
    ),
    box(
      width = 4,
      title = '',
      solidHeader = TRUE,
      collapsible = TRUE,
      fluidRow(column(width = 12, htmlOutput(ns('image_2')))),
      br(),
      fluidRow(
        column(width = 6, actionBttn(ns('prev_2'), label = '<<<', style = 'simple', color = 'warning', block = TRUE)),
        column(width = 6, actionBttn(ns('next_2'), label = '>>>', style = 'simple', color = 'warning', block = TRUE))
      )
    ),
    box(
      width = 4,
      title = '',
      solidHeader = TRUE,
      collapsible = TRUE,
      fluidRow(column(width = 12, htmlOutput(ns('image_3')))),
      br(),
      fluidRow(
        column(width = 6, actionBttn(ns('prev_3'), label = '<<<', style = 'simple', color = 'warning', block = TRUE)),
        column(width = 6, actionBttn(ns('next_3'), label = '>>>', style = 'simple', color = 'warning', block = TRUE))
      )
    ),
    
    # Main characteristics ----
    box(
      width = 12,
      title = 'Main characteristics',
      solidHeader = TRUE,
      collapsible = TRUE,
      fluidRow(
        column(width = 4, dataTableOutput(ns('table_main_1'))),
        column(width = 4, dataTableOutput(ns('table_main_2'))),
        column(width = 4, dataTableOutput(ns('table_main_3')))
      )
    ),
    # Advanced characteristics ----
    box(
      width = 12,
      title = 'Exteded characteristics',
      solidHeader = TRUE,
      collapsible = TRUE,
      fluidRow(
        column(width = 4, dataTableOutput(ns('table_additional_1'))),
        column(width = 4, dataTableOutput(ns('table_additional_2'))),
        column(width = 4, dataTableOutput(ns('table_additional_3')))
      )
    ),
    
    # Additional features ----
    box(
      width = 12,
      title = 'Features',
      solidHeader = TRUE,
      collapsible = TRUE,
      fluidRow(
        column(width = 4, dataTableOutput(ns('table_features_1'))),
        column(width = 4, dataTableOutput(ns('table_features_2'))),
        column(width = 4, dataTableOutput(ns('table_features_3')))
      )
    ),
    
    # Description ----
    box(
      width = 12,
      title = 'Offers description',
      solidHeader = TRUE,
      collapsible = TRUE,
      fluidRow(
        column(width = 4, dataTableOutput(ns('table_description_1'))),
        column(width = 4, dataTableOutput(ns('table_description_2'))),
        column(width = 4, dataTableOutput(ns('table_description_3')))
      )
    )
  )
}