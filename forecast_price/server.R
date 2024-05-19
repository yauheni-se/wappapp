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
          addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
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
            addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
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
      
      # click button for the first time ----
      shinyjs::click("forecast-forecast_price", asis = TRUE)
      
      # calculate price ----
      observeEvent(input$forecast_price, {
        # read data ----
        shinyjs::show('progress_bar_row')
        
        last_click <- isolate(as.data.frame(input$map_click))
        if (nrow(last_click)==0) {last_click <- tibble(lng = 21.05, lat = 52.23)}
        
        objects <- read_rds('data/objects.rds')#data.table::fread("data/objects.csv") %>% as_tibble()
        river <- data.table::fread("data/river.csv") %>% as_tibble()
        cbd <- data.table::fread("data/cbd.csv") %>% as_tibble()
        df <- data.table::fread("data/df.csv") %>% as_tibble()
        best_model <- lightgbm::readRDS.lgb.Booster('data/best_model.rds')
        df_orig <- read_rds("data/df_orig.rds") %>% 
          filter(area <= 17000) %>% 
          filter(!floor_no %in% c("cellar", "garret"))

        
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
            is_first_time = 1L,
            region = region_calc,
            # main
            is_market_primary = isolate(as.integer(!is.null(input$main) && "Primary market" %in% input$main)),
            is_ownership_full = isolate(as.integer(!is.null(input$main) && "Full ownership" %in% input$main)),
            is_to_renovation = isolate(as.integer(!is.null(input$main) && "Needs renovation" %in% input$main)),
            
            # type of flat
            building_type = isolate(input$building_type),
            
            # interior
            is_furniture = isolate(as.integer(!is.null(input$interior) && 'Furniture' %in% input$interior)),
            is_kitchen_fursnished = isolate(as.integer(!is.null(input$interior) && 'Furniture in kitchen' %in% input$interior)),
            is_kitchen_separate = isolate(as.integer(!is.null(input$interior) && 'Separated kitchen' %in% input$interior)),
            is_air_cond = isolate(as.integer(!is.null(input$interior) && 'Air conditioner' %in% input$interior)),
            is_phone = isolate(as.integer(!is.null(input$interior) && 'Phone' %in% input$interior)),
            is_media = isolate(as.integer(!is.null(input$interior) && 'Media' %in% input$interior)),
            
            # area
            area = isolate(input$area),
            
            # exterior
            is_balcony = isolate(as.integer(!is.null(input$exterior) && 'Balcony' %in% input$exterior)),
            is_terrace = isolate(as.integer(!is.null(input$exterior) && 'Terrace' %in% input$exterior)),
            is_garden = isolate(as.integer(!is.null(input$exterior) && 'Garden' %in% input$exterior)),
            is_basement = isolate(as.integer(!is.null(input$exterior) && 'Basement' %in% input$exterior)),
            is_garage = isolate(as.integer(!is.null(input$exterior) && 'Garage' %in% input$exterior)),
            is_utility_room = isolate(as.integer(!is.null(input$exterior) && 'Utility room' %in% input$exterior)),
 
            # safety
            is_domophone = isolate(as.integer(!is.null(input$safety) && 'Domophone' %in% input$safety)),
            is_alarm = isolate(as.integer(!is.null(input$safety) && 'Alarm system' %in% input$safety)),
            is_security_windows = isolate(as.integer(!is.null(input$safety) && 'Security windows' %in% input$safety)),
            is_remote_service = isolate(as.integer(!is.null(input$safety) && 'Remote service' %in% input$safety)),
            is_closed_territory = isolate(as.integer(!is.null(input$safety) && 'Closed territory' %in% input$safety)),
            is_security = isolate(as.integer(!is.null(input$safety) && 'Security' %in% input$safety)),
            
            # building
            is_elevator = isolate(as.integer(!is.null(input$building) && 'Elevator' %in% input$building)),
            is_building_concrete = isolate(as.integer(!is.null(input$building) && 'Urban heating' %in% input$building)),
            is_heating_urban = isolate(as.integer(!is.null(input$building) && 'Concrete building' %in% input$building)),
            
            # additional
            windows_type = isolate(input$windows_type),
            floor_no = isolate(input$floor_no),
            building_floors_num = isolate(input$building_floors_num)
          )
          flat <- flat %>% mutate(price_per_m = NA_real_) %>% dplyr::select(all_of(colnames(df)))
          
          # predict price ----
          updateProgressBar(session = session, id = 'progress_bar', value = 60, title = 'Predicting price...')
          Sys.sleep(2)
          
          varnames <- setdiff(colnames(df), c("price_per_m"))
          data_test_sparse <- Matrix::Matrix(as.matrix(flat[, varnames, with = FALSE]), sparse = TRUE)
          
          forecasted_price <- predict(best_model, data_test_sparse)
          correction_rate <- 1+(isolate(input$perc_change)/100)
          corrected_price <- round(exp(forecasted_price)*correction_rate, 0)
          
          # prepare df for visualizations ----
          updateProgressBar(session = session, id = 'progress_bar', value = 65, title = 'Preparing visualizations...')
          Sys.sleep(2)
          
          df_vis <- prepare_df_vis(df, correction_rate) %>% 
            mutate(
              reg = region,
              area = df_orig$area,
              region = df_orig$region,
              is_market_primary = df$is_market_primary
            )
          
          # draw value boxes ----
          # browser()
          
          output$price_comparison_box_total <- renderValueBox(show_price_comparison_box_total(df_vis, corrected_price))
          
          output$price_comparison_box <- renderValueBox(show_price_comparison_box(df_vis, corrected_price, region_calc))
          
          output$price_box <- renderValueBox({
            valueBox(
              value = paste0(format(corrected_price, big.mark = " "), ' zl'),
              subtitle = 'Predicted price per square meter',
              icon = icon('pencil-alt'),
              color = 'blue'
            )
          })
          
          output$price_box_total <- renderValueBox({
            valueBox(
              value = paste0(format(corrected_price*isolate(input$area), big.mark = " "), ' zl'),
              subtitle = 'Predicted total price',
              icon = icon('building'),
              color = 'blue'
            )
          })
          
          
          # draw flat statistics ----
          output$dist_to_objects <- renderPlotly(show_dist_to_objects(flat))
          output$count_of_objects <- renderPlotly(show_count_of_objects(flat))
          output$map_dist_to_objects <- renderLeaflet(create_map_within_800(objects, lon = isolate(last_click$lng), lat = isolate(last_click$lat)))
          
          # draw Warsaw statistics ----
          output$price_dist <- renderPlotly(show_dist(df_vis, 'y'))
          output$area_dist <- renderPlotly(show_dist(df_vis, 'area'))
          
          output$mean_price_prim <- renderLeaflet(show_mean_price(regions, df_vis, 1L))
          output$mean_price_secd <- renderLeaflet(show_mean_price(regions, df_vis, 0L))

          output$price_hist_by_regs <- renderPlotly(show_hist_by_regs(df_vis, 'y'))
          output$area_hist_by_regs <- renderPlotly(show_hist_by_regs(df_vis, 'area'))
          
          output$price_dist_by_regs <- renderPlotly(show_dist_by_regs(df_vis, 'y'))
          output$area_dist_by_regs <- renderPlotly(show_dist_by_regs(df_vis, 'area'))
          
        }
        # finalization ----
        updateProgressBar(session = session, id = 'progress_bar', value = 100, title = 'Finalization...')
        Sys.sleep(2)
        shinyjs::hide('progress_bar_row')
      })
      
    }
  )
}