Forecast <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      shinyjs::hide('progress_bar_row')
      regions <- read_rds("data/regions.rds")
      regions$nazwa_dzie <- c(
        "Zoliborz", "Praga_Poludnie", "Mokotow", "Wola", "Wilanow", "Wesola", "Wawer", "Wlochy", "Ursynow", "Srodmiejscie",
        "Praga_Polnoc", "Ursus", "Targowek", "Rembertow", "Ochota", "Bielany", "Bialolenka", "Bemowo"
      )
      
      # initial map ----
      output$map <- renderLeaflet(
        regions %>% 
          leaflet(options = leafletOptions(attributionControl=FALSE)) %>% 
          addProviderTiles(providers$Stadia.OSMBright) %>% 
          addPolygons(
            stroke = 0.5,
            opacity = 1,
            fillColor = "#2C3E50",
            weight = 0.8,
            color = "black",
            smoothFactor = 0.8
          ) %>%
          addCircleMarkers(
            lng = 21.05,
            lat = 52.23,
            radius = 5,
            color = '#FEAB3A',
            stroke = FALSE,
            fillOpacity = 1
          ) %>% 
          setView(lng = 21.05, lat = 52.23, zoom = 10)
      )
      
      # update last coordinate every time the user clicks on map ----
      observeEvent(input$map_click, {
        sel_point <- as.data.frame(isolate(input$map_click))
        output$map <- renderLeaflet(
          regions %>% 
            leaflet(options = leafletOptions(attributionControl=FALSE)) %>% 
            addProviderTiles(providers$Stadia.OSMBright) %>% 
            addPolygons(
              stroke = 0.5,
              opacity = 1,
              fillColor = "#2C3E50",
              weight = 0.8,
              color = "black",
              smoothFactor = 0.8
            ) %>%
            addCircleMarkers(
              lng = sel_point$lng,
              lat = sel_point$lat,
              radius = 5,
              color = '#FEAB3A',
              stroke = FALSE,
              fillOpacity = 1
            ) %>% 
            setView(lng = isolate(input$map_center$lng), lat = isolate(input$map_center$lat), zoom = isolate(input$map_zoom))
        )
      })
      
      # calculate price ----
      observeEvent(input$forecast_price, {
        # presets ----
        shinyjs::show('progress_bar_row')
        
        last_click <- isolate(as.data.frame(input$map_click))
        if (nrow(last_click)==0) {last_click <- tibble(lng = 21.05, lat = 52.23)}
        
        objects <- data.table::fread("data/objects.csv") %>% as_tibble()
        river <- data.table::fread("data/river.csv") %>% as_tibble()
        cbd <- data.table::fread("data/cbd.csv") %>% as_tibble()
        df <- data.table::fread("data/df.csv") %>% as_tibble()
        best_model <- lightgbm::readRDS.lgb.Booster('data/best_model.rds')
        
        # check location ----
        updateProgressBar(session = session, id = 'progress_bar', value = 0, title = 'Assigning district to the selected location...')
        Sys.sleep(2)
        
        region_calc <- last_click %>% 
          dplyr::select(lat, lng) %>% 
          as.data.frame() %>% 
          sf::st_as_sf(coords = c(2, 1)) %>% 
          sf::st_set_crs(4326) %>% 
          sf::st_transform(2163) %>% 
          st_join(st_as_sf(regions) %>% st_set_crs(4326) %>% st_transform(2163)) %>% 
          as.data.frame() %>% 
          as_tibble() %>% 
          select(-geometry) %>% 
          pull()
        
        if (is.na(region_calc)) {
          show_toast(
            title = 'Location is outside Warsaw',
            type = 'error',
            position = 'top-end',
            timer = 9000
          )
        } else {
          # calculate flat price ----
          updateProgressBar(session = session, id = 'progress_bar', value = 5, title = 'Calculating distances to the nearby locations...')
          Sys.sleep(2)
          
          flat <- create_new_flat(
            objects = objects,
            river = river,
            cbd = cbd,
            lon = last_click$lng,
            lat = last_click$lat,
            area = isolate(input$area),
            is_kitchen_separate = isolate(input$is_kitchen_separate),
            is_closed_territory = isolate(input$is_closed_territory),
            is_domophone = isolate(input$is_domophone),
            is_security = isolate(input$is_security),
            is_furniture = isolate(input$is_furniture),
            is_air_cond = isolate(input$is_air_cond),
            is_balcony = isolate(input$is_balcony),
            is_basement = isolate(input$is_basement),
            is_garage = isolate(input$is_garage),
            is_elevator = isolate(input$is_elevator),
            is_phone = isolate(input$is_phone),
            is_security_windows = isolate(input$is_security_windows),
            is_terrace = isolate(input$is_terrace),
            is_utility_room = isolate(input$is_utility_room),
            is_alarm = isolate(input$is_alarm),
            is_garden = isolate(input$is_garden),
            is_remote_service = isolate(input$is_remote_service),
            is_first_time = 1L,
            is_kitchen_fursnished = isolate(input$is_kitchen_fursnished),
            is_media = isolate(input$is_media),
            is_market_primary = isolate(input$is_market_primary),
            is_building_concrete = isolate(input$is_building_concrete),
            is_heating_urban = isolate(input$is_heating_urban),
            is_to_renovation = isolate(input$is_to_renovation),
            is_ownership_full = isolate(input$is_ownership_full),
            region = region_calc,
            building_type = isolate(input$building_type),
            windows_type = isolate(input$windows_type),
            floor_no = isolate(input$floor_no),
            building_floors_num = isolate(input$building_floors_num)
          )
          flat <- flat %>% mutate(price_per_m = NA_real_) %>% dplyr::select(all_of(colnames(df)))
          
          # predict price ----
          updateProgressBar(session = session, id = 'progress_bar', value = 85, title = 'Predicting price...')
          Sys.sleep(2)
          
          varnames <- setdiff(colnames(df), c("price_per_m"))
          data_test_sparse <- Matrix::Matrix(as.matrix(flat[, varnames, with = FALSE]), sparse = TRUE)
          
          forecasted_price <- predict(best_model, data_test_sparse)
          correction_rate <- 1+(isolate(input$perc_change)/100)
          corrected_price <- round(exp(forecasted_price)*correction_rate, 0)
          
          # draw box plots ----
          updateProgressBar(session = session, id = 'progress_bar', value = 95, title = 'Preparing visualizations...')
          Sys.sleep(2)
          
          df_vis <- prepare_df_vis(df, correction_rate)
          
          output$boxplot_total <- renderPlotly(show_box_total(df_vis))
          output$boxplot_region <- renderPlotly(show_box_region(df_vis, region_calc))
          
          # draw value boxes ----
          output$price_comparison_box_total <- renderValueBox(show_price_comparison_box_total(df_vis, corrected_price))
          
          output$price_comparison_box <- renderValueBox(show_price_comparison_box(df_vis, corrected_price, region_calc))
          
          output$price_box <- renderValueBox({
            valueBox(
              value = paste0(format(corrected_price, big.mark = " "), ' zl'),
              subtitle = 'Price per sq.m.',
              icon = icon('pencil-alt'),
              color = 'blue'
            )
          })
          
          output$price_box_total <- renderValueBox({
            valueBox(
              value = paste0(format(corrected_price*isolate(input$area), big.mark = " "), ' zl'),
              subtitle = 'Total price',
              icon = icon('building'),
              color = 'blue'
            )
          })
          
          # list objects ----
          output$object_dists <- renderDataTable(clear_dists(flat))
          
          output$object_counts <- renderDataTable(clear_counts(flat))
          
        }
        # finalization ----
        updateProgressBar(session = session, id = 'progress_bar', value = 100, title = 'Finalization...')
        Sys.sleep(2)
        shinyjs::hide('progress_bar_row')
      })
      
    }
  )
}