Comparison <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # Presets ----
      ns <- session$ns
      shinyjs::hide('progress_bar_row')
      flats <<- list()
      prev_next_1 <<- 1
      prev_next_2 <<- 1
      prev_next_3 <<- 1
      lapply(1:3, function(i) {
        shinyjs::hide(paste0('prev_', i))
        shinyjs::hide(paste0('next_', i))
      })
      
      # Click button for the first time ----
      shinyjs::click("comparison-compare_apartments", asis = TRUE)
      
      
      # Compare apartments button ----
      observeEvent(input$compare_apartments, {
        # download pages ----
        shinyjs::show('progress_bar_row')
        
        flats <<- list()
        prev_next_1 <<- 1
        prev_next_2 <<- 1
        prev_next_3 <<- 1
        urls <- c(isolate(input$url1), isolate(input$url2), isolate(input$url3))
        
        updateProgressBar(session = session, id = 'progress_bar', value = 0, title = 'Downloading pages...')
        Sys.sleep(2)
        
        for (i in seq_along(urls)) {
          if (!str_detect(urls[i], 'https\\:\\/\\/www\\.otodom\\.pl\\/pl\\/oferta\\/') & urls[i] != '') {
            show_toast(
              title = paste0('Link ', i, ' is incorrect. Disabling output for this property.'),
              type = 'warning',
              position = 'top-end',
              timer = 9000
            )
            flats[[i]] <<- tibble()
          } else if (urls[i] == '') {
            flats[[i]] <<- tibble()
          } else {
            tmp_offer <- download_page(urls[i])
            if (!identical(tmp_offer, list())) {
              flats[[i]] <<- tmp_offer %>% clear_offer() %>% clear_flat()
            } else {
              show_toast(
                title = paste0('Link ', i, ' is incorrect or has expired. Disabling output for this property.'),
                type = 'warning',
                position = 'top-end',
                timer = 9000
              )
              flats[[i]] <<- tibble()
            }
          }
        }
        
        # check condition & show visuals if satisfied ----
        updateProgressBar(session = session, id = 'progress_bar', value = 50, title = 'Preparing visualizations...')
        Sys.sleep(2)
        
        lapply(seq_along(flats), function(i) {
          if (nrow(flats[[i]])!=0) {
            output[[paste0('table_main_', i)]] <- renderDataTable(flats[[i]] %>% show_main())
            output[[paste0('table_additional_', i)]] <- renderDataTable(flats[[i]] %>% show_additional())
            output[[paste0('table_features_', i)]] <- renderDataTable(flats[[i]] %>% show_features())
            output[[paste0('table_description_', i)]] <- renderDataTable(flats[[i]] %>% show_description())
            output[[paste0('image_', i)]] <- renderText({c('<img src="', flats[[i]]$images[[1]][1], '" width="710" height="600">')})
            shinyjs::show(paste0('prev_', i))
            shinyjs::show(paste0('next_', i))
          } else {
            output[[paste0('table_main_', i)]] <- renderDataTable(datatable(tibble()))
            output[[paste0('table_additional_', i)]] <- renderDataTable(datatable(tibble()))
            output[[paste0('table_features_', i)]] <- renderDataTable(datatable(tibble()))
            output[[paste0('table_description_', i)]] <- renderDataTable(datatable(tibble()))
            output[[paste0('image_', i)]] <- renderText({})
            shinyjs::hide(paste0('prev_', i))
            shinyjs::hide(paste0('next_', i))
          }
        })
        # finalization ----
        updateProgressBar(session = session, id = 'progress_bar', value = 100, title = 'Finalization...')
        Sys.sleep(2)
        shinyjs::hide('progress_bar_row')
      })
      # Image 1 ----
      observeEvent(input$prev_1, {
        if ( (length(flats) != 0) && (nrow(flats[[1]]) != 0) && ('images' %in% colnames(flats[[1]])) ) {
          prev_next_1 <<- prev_next_1-1
          if (prev_next_1 == 0) {prev_next_1 <<- length(flats[[1]]$images[[1]])}
          output$image_1 <- renderText({c('<img src="', flats[[1]]$images[[1]][prev_next_1], '" width="710" height="600">')})
        }
      })
      
      observeEvent(input$next_1, {
        if ( (length(flats) != 0) && (nrow(flats[[1]]) != 0) && ('images' %in% colnames(flats[[1]])) ) {
          prev_next_1 <<- prev_next_1+1
          if (prev_next_1 > length(flats[[1]]$images[[1]])) {prev_next_1 <<- 1}
          output$image_1 <- renderText({c('<img src="', flats[[1]]$images[[1]][prev_next_1], '" width="710" height="600">')})
        }
      })
      
      # Image 2 ----
      observeEvent(input$prev_2, {
        if ( (length(flats) != 0) && (nrow(flats[[2]]) != 0) && ('images' %in% colnames(flats[[2]])) ) {
          prev_next_2 <<- prev_next_2-1
          if (prev_next_2 == 0) {prev_next_2 <<- length(flats[[2]]$images[[1]])}
          output$image_2 <- renderText({c('<img src="', flats[[2]]$images[[1]][prev_next_2], '" width="710" height="600">')})
        }
      })
      
      observeEvent(input$next_2, {
        if ( (length(flats) != 0) && (nrow(flats[[2]]) != 0) && ('images' %in% colnames(flats[[2]])) ) {
          prev_next_2 <<- prev_next_2+1
          if (prev_next_2 > length(flats[[2]]$images[[1]])) {prev_next_2 <<- 1}
          output$image_2 <- renderText({c('<img src="', flats[[2]]$images[[1]][prev_next_2], '" width="710" height="600">')})
        }
      })
      
      # Image 3 ----
      observeEvent(input$prev_3, {
        if ( (length(flats) != 0) && (nrow(flats[[3]]) != 0) && ('images' %in% colnames(flats[[3]])) ) {
          prev_next_3 <<- prev_next_3-1
          if (prev_next_3 == 0) {prev_next_3 <<- length(flats[[3]]$images[[1]])}
          output$image_3 <- renderText({c('<img src="', flats[[3]]$images[[1]][prev_next_3], '" width="710" height="600">')})
        }
      })
      
      observeEvent(input$next_3, {
        if ( (length(flats) != 0) && (nrow(flats[[3]]) != 0) && ('images' %in% colnames(flats[[3]])) ) {
          prev_next_3 <<- prev_next_3+1
          if (prev_next_3 > length(flats[[3]]$images[[1]])) {prev_next_3 <<- 1}
          output$image_3 <- renderText({c('<img src="', flats[[3]]$images[[1]][prev_next_3], '" width="710" height="600">')})
        }
      })
    }
  )
}