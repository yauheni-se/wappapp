ForecastUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = 'forecast',
    # Input ----
    box(
      width = 12,
      title = 'Property features',
      solidHeader = TRUE,
      status = 'primary',
      
      fluidRow(
        column(
          width = 2,
          pickerInput(
            ns('main'), label = 'Main', multiple = TRUE,
            choices = c('Needs renovation', 'Primary market', 'Full ownership') %>% sort(),
            selected = c('Primary market', 'Full ownership')
          ),
          pickerInput(ns('building_type'), label = 'Type of flat', choices = c('apartment', 'block', 'other')),
          pickerInput(
            ns('interior'), label = 'Interior', multiple = TRUE,
            choices = c('Furniture', 'Furniture in kitchen', 'Separated kitchen', 'Air conditioner', 'Phone', 'Media') %>% sort(),
            selected = c('Furniture', 'Air conditioner')
          ),
          tags$div(
            id = "expr-container",
            class = "js-irs-blue",
            sliderInput(ns('area'), label = 'Apartment size', min = 20, max = 300, value = 55, step = 1, ticks = FALSE, post = 'm2')
          )
        ),
        
        column(
          width = 2,
          pickerInput(
            ns('exterior'), label = 'Exterior', multiple = TRUE,
            choices = c('Balcony', 'Terrace', 'Garden', 'Basement', 'Garage', 'Utility room') %>% sort(),
            selected = c('Balcony')
          ),
          pickerInput(
            ns('safety'), label = 'Safety', multiple = TRUE,
            choices = c('Domophone', 'Alarm system', 'Security windows', 'Remote service', 'Closed territory', 'Security') %>% sort(),
            selected = ''
          ),
          pickerInput(
            ns('bulding'), label = 'Bulding', multiple = TRUE,
            choices = c('Elevator', 'Urban heating', 'Concrete building') %>% sort(),
            selected = c('Elevator', 'Urban heating')
          ),
          br(),
          br(),
          br(),
          br(),
          br(),
          actionBttn(ns("forecast_price"), "Forecast price", block = TRUE, color = 'warning', style = 'simple')
        ),
        
        column(
          width = 2,
          pickerInput(ns('windows_type'), label = 'Windows material', choices = c('wooden', 'plastic', 'other')),
          pickerInput(ns('floor_no'), label = 'Floor', choices = c(as.character(0:9), '9+', 'unknown'), selected = 'unknown'),
          pickerInput(ns('building_floors_num'), label = 'Number of floors', choices = c(as.character(1:9), '9+', 'unknown'), selected = 'unknown'),
          tags$div(
            id = "expr-container",
            class = "js-irs-blue",
            sliderInput(ns('perc_change'), label = 'Average price growth since June-2023', min = 0, max = 100, value = 29, step = 1, ticks = FALSE, post = '%')
          )
        ),
        
        column(
          width = 6,
          leafletOutput(ns("map"))
        )
      ),
      
      br(),
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
    
    
    # Output ----
    tabBox(
      width = 12,
      # Apartment statistics ----
      tabPanel(
        "Selected apartment",
        br(),
        
        # value boxes
        fluidRow(
          column(
            width = 3,
            valueBoxOutput(ns('price_box'), width = 12)
          ),
          column(
            width = 3,
            valueBoxOutput(ns('price_box_total'), width = 12)
          ),
          column(
            width = 3,
            valueBoxOutput(ns('price_comparison_box_total'), width = 12)
          ),
          column(
            width = 3,
            valueBoxOutput(ns('price_comparison_box'), width = 12)
          )
        ),
        
        br(),
        br(),
        br(),
        br(),
        
        # flat stats
        fluidRow(
          column(
            width = 6,
            tags$h4('Distance between different objects and an apartment'),
            withSpinner(plotlyOutput(ns('dist_to_objects')), color = '#2C3E50'),
            style = "height:1000px;"
          ),
          column(
            width = 6,
            tags$h4('Count of different objects within 800 meters from an apartment'),
            withSpinner(plotlyOutput(ns('count_of_objects')), color = '#2C3E50'),
            style = "height:1000px;"
          )
        ),
        
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        
        fluidRow(
          column(
            width = 12,
            tags$h4('Objects within 800 meters from an apartment'),
            withSpinner(leafletOutput(ns('map_dist_to_objects'), height = 1000), color = '#2C3E50'),
          )
        ),
        
        br(),
        br(),
        br(),
        br()
        
      ),
      
      # Warsaw statistics ----
      tabPanel(
        "Warsaw",
        br(),
        
        fluidRow(
          column(
            width = 6,
            style = "height:1000px;",
            tags$h4('Price per square meter distribution'),
            withSpinner(plotlyOutput(ns('price_dist')), color = '#2C3E50'),
            
          ),
          column(
            width = 6,
            style = "height:1000px;",
            tags$h4("Apartment size distribution"),
            withSpinner(plotlyOutput(ns('area_dist')), color = '#2C3E50'),
          )
        ),
        
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        
        fluidRow(
          column(
            width = 6,
            style = "height:1000px;",
            tags$h4('Mean apartment price on the primary market'),
            withSpinner(leafletOutput(ns('mean_price_prim'), height = 1000), color = '#2C3E50'),
          ),
          column(
            width = 6,
            style = "height:1000px;",
            tags$h4("Mean apartment price on the secondary market"),
            withSpinner(leafletOutput(ns('mean_price_secd'), height = 1000), color = '#2C3E50'),
          )
        ),
        
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        
        fluidRow(
          column(
            width = 6,
            style = "height:1000px;",
            tags$h4("Price per square meter distribution by districts"),
            withSpinner(plotlyOutput(ns('price_hist_by_regs')), color = '#2C3E50')
          ),
          column(
            width = 6,
            style = "height:1000px;",
            tags$h4("Apartment size distribution by districts"),
            withSpinner(plotlyOutput(ns('area_hist_by_regs')), color = '#2C3E50')
          )
        ),
        
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        
        fluidRow(
          column(
            width = 6,
            style = "height:1000px;",
            tags$h4("Price per square meter distribution by districts"),
            withSpinner(plotlyOutput(ns('price_dist_by_regs')), color = '#2C3E50')
          ),
          column(
            width = 6,
            style = "height:1000px;",
            tags$h4("Apartment size distribution by districts"),
            withSpinner(plotlyOutput(ns('area_dist_by_regs')), color = '#2C3E50')
          )
        ),
        
        br(),
        br(),
        br(),
        br()
        
      )
    )
  )
}
