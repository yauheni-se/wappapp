ForecastUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = 'forecast',
    box(
      width = 9,
      title = 'Property features',
      solidHeader = TRUE,
      status = 'primary',
      fluidRow(
        column(
          width=1,
          tags$h4(tags$u('Main')),
          prettyCheckbox(ns('is_to_renovation'), label = 'Needs renovation', value = 0L),
          prettyCheckbox(ns('is_market_primary'), label = 'Primary market', value = 1L),
          prettyCheckbox(ns('is_ownership_full'), label = 'Full ownership', value = 1L),
          tags$div(
            id = "expr-container",
            class = "js-irs-blue",
            sliderInput(ns('area'), label = 'Area', min = 20, max = 300, value = 55, step = 1, ticks = FALSE, post = 'm2')
          )
        ),
        column(
          width=1,
          tags$h4(tags$u('Interior')),
          prettyCheckbox(ns('is_furniture'), label = 'Furniture', value = 1L),
          prettyCheckbox(ns('is_kitchen_fursnished'), label = 'Furniture in kitchen', value = 0L),
          prettyCheckbox(ns('is_kitchen_separate'), label = 'Separated kitchen', value = 0L),
          prettyCheckbox(ns('is_air_cond'), label = 'Air conditioner', value = 1L),
          prettyCheckbox(ns('is_phone'), label = 'Phone', value = 0L),
          prettyCheckbox(ns('is_media'), label = 'Media', value = 0L),
        ),
        column(
          width=1,
          tags$h4(tags$u('Exterior')),
          prettyCheckbox(ns('is_balcony'), label = 'Balcony', value = 1L),
          prettyCheckbox(ns('is_terrace'), label = 'Terrace', value = 0L),
          prettyCheckbox(ns('is_garden'), label = 'Garden', value = 0L),
          prettyCheckbox(ns('is_basement'), label = 'Basement', value = 0L),
          prettyCheckbox(ns('is_garage'), label = 'Garage', value = 0L),
          prettyCheckbox(ns('is_utility_room'), label = 'Utility room', value = 0L),
        ),
        column(
          width=1,
          tags$h4(tags$u('Safety')),
          prettyCheckbox(ns('is_domophone'), label = 'Domophone', value = 0L),
          prettyCheckbox(ns('is_alarm'), label = 'Alarm system', value = 0L),
          prettyCheckbox(ns('is_security_windows'), label = 'Security windows', value = 0L),
          prettyCheckbox(ns('is_remote_service'), label = 'Remote service', value = 0L),
          prettyCheckbox(ns('is_closed_territory'), label = 'Closed territory', value = 0L),
          prettyCheckbox(ns('is_security'), label = 'Security', value = 0L),
        ),
        column(
          width=1,
          tags$h4(tags$u('Bulding')),
          prettyCheckbox(ns('is_elevator'), label = 'Elevator', value = 1L),
          prettyCheckbox(ns('is_heating_urban'), label = 'Urban heating', value = 1L),
          prettyCheckbox(ns('is_building_concrete'), label = 'Concrete building', value = 0L)
        ),
        column(
          width=2,
          tags$h4(tags$u('Additional')),
          tags$div(
            id = "expr-container", 
            pickerInput(ns('building_type'), label = 'Type of flat', choices = c('apartment', 'block', 'other'))
          ),
          tags$div(
            id = "expr-container", 
            pickerInput(ns('windows_type'), label = 'Windows material', choices = c('wooden', 'plastic', 'other'))
          ),
          tags$div(
            id = "expr-container", 
            pickerInput(ns('floor_no'), label = 'Floor', choices = c(as.character(0:9), '9+', 'unknown'), selected = 'unknown')
          ),
          tags$div(
            id = "expr-container", 
            pickerInput(ns('building_floors_num'), label = 'Number of floors', choices = c(as.character(1:9), '9+', 'unknown'), selected = 'unknown')
          ),
          tags$div(
            id = "expr-container",
            class = "js-irs-blue",
            sliderInput(ns('perc_change'), label = 'Average price growth since June-2023', min = 0, max = 100, value = 6, step = 1, ticks = FALSE, post = '%')
          )
        ),
        column(
          width = 5,
          tags$h4(tags$u('Location')),
          leafletOutput(ns("map"))
        )
      ),
      br(),
      fluidRow(
        column(
          width = 2,
          offset = 5,
          actionBttn(ns("forecast_price"), "Forecast price", block = TRUE, color = 'warning', style = 'simple')
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
    box(
      width = 3,
      title = '',
      solidHeader = TRUE,
      fluidRow(
        column(
          width = 12,
          valueBoxOutput(ns('price_box'), width = 12)
        ),
        column(
          width = 12,
          valueBoxOutput(ns('price_box_total'), width = 12)
        ),
        column(
          width = 12,
          valueBoxOutput(ns('price_comparison_box_total'), width = 12)
        ),
        column(
          width = 12,
          valueBoxOutput(ns('price_comparison_box'), width = 12)
        ),
      )
    ),
    box(
      width = 12,
      title = '',
      solidHeader = TRUE,
      fluidRow(
        column(
          width = 6,
          withSpinner(plotlyOutput(ns('boxplot_total')), color = '#2C3E50')
        ),
        column(
          width = 6,
          withSpinner(plotlyOutput(ns('boxplot_region')), color = '#2C3E50')
        )
      )
    ),
    box(
      width = 12,
      title = '',
      solidHeader = TRUE,
      fluidRow(
        column(
          width = 6,
          withSpinner(dataTableOutput(ns('object_dists')), color = '#2C3E50')
        ),
        column(
          width = 6,
          withSpinner(dataTableOutput(ns('object_counts')), color = '#2C3E50')
        )
      )
    )
  )
}

#'yellow'