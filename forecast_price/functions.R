# general ----
psum <- function(..., na.rm=TRUE) {rowSums(do.call(cbind, list(...)), na.rm=na.rm)}

# for data preparation ----
prepare_df_vis <- function(x, correction_rate) {
  x %>%
    select(starts_with('region_')) %>% 
    mutate(region_Wola = psum(
      region_Bemowo, region_Bialolenka, region_Bielany, region_Mokotow, region_Ochota, region_Praga_Poludnie, region_Praga_Polnoc,
      region_Rembertow, region_Targowek, region_Ursus, region_Ursynow, region_Wawer, region_Wesola, region_Wilanow, region_Wlochy,
      region_Zoliborz, region_Srodmiejscie)
    ) %>% 
    mutate(
      region_Wola = ifelse(region_Wola==0, 1L, 0L),
      id = row_number(),
      y = x$price_per_m,
    ) %>% 
    pivot_longer(cols = colnames(.)[str_detect(colnames(.), 'region')]) %>%
    mutate(
      region = str_remove(name, 'region_'),
      y = round(exp(y), 0)*correction_rate
    ) %>% 
    filter(value == 1) %>% 
    dplyr::select(-c(name, value))
}

rename_objects <- function(x) {
  case_when(
    x == "College university" ~  "College/university",
    x == "Public service" ~ "Police/fire department",
    x == "School kindergarden" ~ "School/kindergarden",
    x == "School kindergardens" ~ "Schools/kindergardens",
    x == "Road cycleway" ~ "Cycleway",
    x == "Road footway" ~ "Footway",
    x == "Road primary" ~ "Primary road",
    x == "Road secondary" ~ "Secondary road",
    x == "Road tertiary" ~ "Tertiary road",
    x == "Road service" ~ "Service road",
    x == "Road residential" ~ "Residential road",
    x == "Road track grade" ~ "Unpaved/gravel road",
    x == "Road trunk" ~ "Autostrade",
    x == "Cbd" ~ "Palace of Culture and Science (city center)",
    x == "Temple catholic" ~ "Catholic temple",
    x == "Temple other" ~ "Non-catholic temple",
    x == "Industrials" ~ "Industrial objects",
    TRUE ~ x
  )
}

clear_dists <- function(df) {
  df %>%
    select(starts_with('dist_')) %>% 
    pivot_longer(cols = colnames(.)) %>% 
    mutate(
      value = ifelse(name %in% c('dist_airport', 'dist_prison', 'dist_road_track_grade', 'dist_road_trunk', 'dist_river', 'dist_cbd'), value, exp(value)),
      name = str_remove(name, 'dist_') %>% str_replace_all('_', ' ') %>% str_to_sentence()
    ) %>% 
    mutate(
      name = rename_objects(name),
      value = round(value, -1)
    ) %>% 
    arrange(name) %>% 
    `colnames<-`(c('Object', 'Distance, m')) %>% 
    datatable(
      rownames = FALSE
    )
}

clear_counts <- function(df) {
  df %>%
    select(ends_with('_800')) %>% 
    pivot_longer(cols = colnames(.)) %>% 
    mutate(
      value = ifelse(name %in% c('bus_stops_800', 'industrials_800', 'playgrounds_800', 'sport_objects_800'), value, exp(value)),
      name = str_remove(name, '_800') %>% str_replace_all('_', ' ') %>% str_to_sentence()
    ) %>% 
    mutate(
      name = rename_objects(name),
      value = round(value, 0)
    ) %>% 
    arrange(name) %>% 
    `colnames<-`(c('Object', 'Number of objects within 10 minutes walk')) %>% 
    datatable(
      rownames = FALSE
    )
}

# for distances calculation and dataset (for the model) creation ----
calc_dist_river <- function(x, y, river) {
  ref_x <- river %>% pull(x)
  ref_y <- river %>% pull(y)
  mapply(calc_geo_dist, ref_x, ref_y, MoreArgs = list(x=x, y=y), USE.NAMES = FALSE) %>% min(na.rm = TRUE)
}

calc_geo_dist <- function(x_ref, y_ref, x, y) {distGeo(c(x, y), c(x_ref, y_ref))}

calc_dist_min <- function(x, y, o_type, objects) {
  ref_x <- objects %>% filter(object_type == o_type) %>% pull(x)
  ref_y <- objects %>% filter(object_type == o_type) %>% pull(y)
  mapply(calc_geo_dist, ref_x, ref_y, MoreArgs = list(x=x, y=y), USE.NAMES = FALSE) %>% min(na.rm = TRUE)
}

calc_dist_800 <- function(x, y, o_type, objects) {
  ref_x <- objects %>% filter(object_type == o_type) %>% pull(x)
  ref_y <- objects %>% filter(object_type == o_type) %>% pull(y)
  mapply(calc_geo_dist, ref_x, ref_y, MoreArgs = list(x=x, y=y), USE.NAMES = FALSE) %>% .[.<=800] %>% length()
}

calc_dist_cbd <- function(x, y, cbd) {
  ref_x <- cbd %>% pull(x)
  ref_y <- cbd %>% pull(y)
  mapply(calc_geo_dist, ref_x, ref_y, MoreArgs = list(x=x, y=y), USE.NAMES = FALSE) %>% min(na.rm = TRUE)
}

create_new_flat <- function(
    objects, river, cbd,
    lon, lat, area, is_kitchen_separate = 0L, is_closed_territory = 0L, is_domophone = 0L, is_security = 0L,
    is_furniture = 0L, is_air_cond = 0L, is_balcony = 1L, is_basement = 0L, is_garage = 0L, is_elevator = 0L,
    is_phone = 0L, is_security_windows = 0L, is_terrace = 0L, is_utility_room = 0L, is_alarm = 0L, is_garden = 0L,
    is_remote_service = 0L, is_first_time = 1L, is_kitchen_fursnished = 0L, is_media = 0L, is_market_primary = 1L,
    is_building_concrete = 0L, is_heating_urban = 1L, is_to_renovation = 0L, is_ownership_full = 1L,
    region = 'Wola', building_type = 'block', windows_type = 'plastic', floor_no = 'unknown', building_floors_num = 'unknown'
) {
  # Calculate distances ----
  flat <- tibble(x = lon, y = lat, area = area)
  
  object_types <- objects %>% filter(!str_detect(object_type, "tree")) %>% .$object_type %>% unique() %>% sort()
  
  for (j in object_types) {
    flat[[paste0("dist_", j)]] <- mapply(calc_dist_min, flat$x, flat$y, MoreArgs = list(o_type = j, objects = objects))
  }
  
  object_types <- objects %>% filter(!str_detect(object_type, "road_|tree")) %>% .$object_type %>% unique() %>% sort()
  for (j in object_types) {
    flat[[paste0(j, "s_800")]] <- mapply(calc_dist_800, flat$x, flat$y, MoreArgs = list(o_type = j, objects = objects))
  }
  
  flat <- flat %>% mutate(dist_river = mapply(calc_dist_river, .$x, .$y, MoreArgs = list(river = river)))
  flat <- flat %>% mutate(dist_cbd = mapply(calc_dist_cbd, .$x, .$y, MoreArgs = list(cbd = cbd)))
  
  # Filtering out variables ----
  excluded_vars <- c(
    "airports_800", "banks_800", "bars_800", "bike_rents_800", "bus_stations_800", "car_services_800", "college_universitys_800", "constructions_800",
    "culturals_800", "dormitorys_800", "garages_800", "gas_stations_800", "graveyards_800", "healthcare_institutions_800", "jewellers_800",
    "nightclubs_800", "parkings_800", "parks_800", "prisons_800", "public_services_800", "railway_stops_800", "shopping_malls_800", "subway_entrances_800",
    "swampss_800", "temple_catholics_800", "temple_others_800", "train_stops_800", "tram_stops_800", "dist_garage", "dist_railway_stop", "area",
    "dist_industrial", "dist_graveyard", "dist_gas_station", "dist_swamps", "dist_road_path", "dist_nightclub", "dist_bus_stop", "dist_playground",
    "dist_road_steps", "bike_parkings_800", "food_shops_800", "pharmacys_800", "shops_800", "attractions_800", "services_800", "dist_parking",
    "water_objects_800", "renthouses_800"
  )
  vars_to_log <- c(
    "beauty_shops_800", "fast_foods_800", "industrials_800", "offices_800", "public_institutions_800", "restaurants_800", "school_kindergardens_800",
    "dist_attraction", "dist_bank", "dist_bar", "dist_beauty_shop", "dist_bike_parking", "dist_bike_rent", "dist_college_university",
    "dist_cultural", "dist_dormitory", "dist_fast_food", "dist_food_shop", "dist_healthcare_institution", "dist_park", "dist_pharmacy", "dist_car_service",
    "dist_bus_station", "dist_construction", "dist_jeweller", "dist_public_institution", "dist_public_service", "dist_renthouse", "dist_restaurant",
    "dist_school_kindergarden", "dist_service", "dist_shop", "dist_shopping_mall", "dist_sport_object", "dist_subway_entrance", "dist_office", 
    "dist_temple_catholic", "dist_temple_other", "dist_tram_stop", "dist_water_object", "dist_road_cycleway", "dist_road_footway", "dist_road_primary",
    "dist_road_residential", "dist_road_secondary", "dist_road_service", "dist_road_tertiary", "dist_train_stop"
  )
  
  flat <- flat %>% 
    dplyr::select(-all_of(excluded_vars)) %>% 
    mutate(across(all_of(vars_to_log), function(x){log(x+1)}))
  # Adding 0-1 columns ----
  flat <- flat %>% 
    mutate(
      is_kitchen_separate = is_kitchen_separate,
      is_closed_territory = is_closed_territory,
      is_domophone = is_domophone,
      is_security = is_security,
      is_furniture = is_furniture,
      is_air_cond = is_air_cond,
      is_balcony = is_balcony,
      is_basement = is_basement,
      is_garage = is_garage,
      is_elevator = is_elevator,
      is_phone = is_phone,
      is_security_windows = is_security_windows,
      is_terrace = is_terrace,
      is_utility_room = is_utility_room,
      is_alarm = is_alarm,
      is_garden = is_garden,
      is_remote_service = is_remote_service,
      is_first_time = is_first_time,
      is_kitchen_fursnished = is_kitchen_fursnished,
      is_media = is_media,
      is_market_primary = is_market_primary,
      is_building_concrete = is_building_concrete,
      is_heating_urban = is_heating_urban,
      is_to_renovation = is_to_renovation,
      is_ownership_full = is_ownership_full
    )
  # Add other categorical columns ----
  flat <- flat %>% 
    mutate(
      rooms_num_1 = 0L,
      rooms_num_2 = 0L,
      rooms_num_3 = 0L,
      rooms_num_4 = 0L,
      `rooms_num_4+` = 0L,
      region_Bemowo = 0L,
      `region_Bialolenka` = 0L,
      region_Bielany = 0L,
      `region_Mokotow` = 0L,
      region_Ochota = 0L,
      `region_Praga_Poludnie` = 0L,
      `region_Praga_Polnoc` = 0L,
      `region_Rembertow` = 0L,
      `region_Targowek` = 0L,
      region_Ursus = 0L,
      `region_Ursynow` = 0L,
      region_Wawer = 0L,
      `region_Wesola` = 0L,
      `region_Wilanow` = 0L,
      `region_Wlochy` = 0L,
      `region_Srodmiejscie` = 0L,
      `region_Zoliborz` = 0L,
      building_type_apartment = 0L,
      building_type_block = 0L,
      building_type_other = 0L,
      windows_type_aluminium = 0L,
      windows_type_plastic = 0L,
      windows_type_wooden = 0L,
      floor_no_0 = 0L,
      floor_no_2 = 0L,
      floor_no_3 = 0L,
      floor_no_4 = 0L,
      floor_no_5 = 0L,
      floor_no_6 = 0L,
      floor_no_7 = 0L,
      floor_no_8 = 0L,
      floor_no_9 = 0L,
      `floor_no_9+` = 0L,
      floor_no_unknown = 0L,
      building_floors_num_1 = 0L,
      building_floors_num_2 = 0L,
      building_floors_num_3 = 0L,
      building_floors_num_5 = 0L,
      building_floors_num_6 = 0L,
      building_floors_num_7 = 0L,
      building_floors_num_8 = 0L,
      building_floors_num_9 = 0L,
      `building_floors_num_9+` = 0L,
      building_floors_num_unknown = 0L
    )
  flat[[paste0('region_', region)]] <- 1L
  flat[[paste0('building_type_', building_type)]] <- 1L
  flat[[paste0('windows_type_', windows_type)]] <- 1L
  flat[[paste0('floor_no_', floor_no)]] <- 1L
  flat[[paste0('building_floors_num_', building_floors_num)]] <- 1L
  
  # Return -----
  return(flat)
}

# for visualization ----
show_box_total <- function(df) {
  df %>% 
    plot_ly(
      y =~ y,
      type= 'box',
      name = 'price per sq. m.',
      boxmean = TRUE,
      color = I('#2C3E50')
    ) %>% 
    layout(
      title = 'Price per sq. m. distrbution in Warsaw, zl',
      yaxis = list(zeroline=FALSE, title = '', mirror = TRUE, linecolor = '#2C3E50'),
      xaxis = list(zeroline=FALSE, title = '', mirror = TRUE, linecolor = '#2C3E50'),
      paper_bgcolor = '#F5F5F5',
      plot_bgcolor = '#F5F5F5'
    )
}

show_box_region <- function(df, reg_sel) {
  df %>% 
    filter(region == reg_sel) %>% 
    plot_ly(
      y =~ y,
      type= 'box',
      name = 'price per sq. m.',
      boxmean = TRUE,
      color = I('#2C3E50')
    ) %>% 
    layout(
      title = glue::glue('Price per sq. m. distrbution in selected district ({str_replace(reg_sel, "_", " ")}), zl'),
      yaxis = list(zeroline=FALSE, title = '', mirror = TRUE, linecolor = '#2C3E50'),
      xaxis = list(zeroline=FALSE, title = '', mirror = TRUE, linecolor = '#2C3E50'),
      paper_bgcolor = '#F5F5F5',
      plot_bgcolor = '#F5F5F5'
    )
}

show_price_comparison_box_total <- function(df, corrected_price) {
  warsaw_perc <- round(length(df$y[df$y<corrected_price])/length(df$y)*100, 2)
  icon_warsaw <- ifelse(warsaw_perc > 60, 'balance-scale-left', ifelse(warsaw_perc < 40, 'balance-scale-right', 'balance-scale'))
  valueBox(
    value = paste0(warsaw_perc, '%'),
    subtitle = 'of flats in Warsaw costs less then the selected property',
    icon = icon(icon_warsaw),
    color = 'light-blue'
  )
}

show_price_comparison_box <- function(df, corrected_price, region_calc) {
  prices_restricted <- df %>% filter(region == region_calc) %>% .$y
  region_perc <- round(length(prices_restricted[prices_restricted<corrected_price])/length(prices_restricted)*100, 2)
  icon_region <- ifelse(region_perc > 60, 'balance-scale-left', ifelse(region_perc < 40, 'balance-scale-right', 'balance-scale'))
  valueBox(
    value = paste0(region_perc, '%'),
    subtitle = glue::glue('of flats in {str_replace(region_calc, "_", " ")} costs less then the selected property'),
    icon = icon(icon_region),
    color = 'light-blue'
  )
}