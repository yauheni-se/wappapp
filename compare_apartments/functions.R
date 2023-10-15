# for downloading otodom data and cleaning ----
clean_letters <- function(x) {x %>% str_split(",") %>% .[[1]] %>% str_trim() %>% unique() %>% .[.!=""]}

switch_owner <- function(x) {
  case_when(
    x == "full_ownership" ~ "full",
    x == "share" ~ "share",
    x == "co_operative_ownership" ~ "cooperative",
    x == "co_operative_ownership_with_a_land_and_mortgage_registe" ~ "cooperative_register",
    TRUE ~ NA_character_
  )
}

download_page <- function(url) {
  response <- GET(url, add_headers("User-Agent" = "Mozilla/5.0"))
  content <- content(response, "text")
  parsed_html <- read_html(content)
  desc <- parsed_html %>% html_node('script[type="application/json"]')
  json_data <- jsonlite::fromJSON(desc %>% html_text())
  if (!is.na(desc)) {
    json_data <- jsonlite::fromJSON(desc %>% html_text())
    if ("ad" %in% names(json_data$props$pageProps)) {
      offer_detailed <- json_data$props$pageProps$ad
    } else {
      offer_detailed <- list()
    }
  } else {
    offer_detailed <- list()
  }
  return(offer_detailed)
}

clear_offer <- function(offer_detailed) {
  keys_to_keep <- c(
    'developmentTitle', 'description', 'location', 'characteristics', 'features', 
    'featuresWithoutCategory', 'property', 'owner', 'images'
  )
  for (i in names(offer_detailed)) {
    if (!i %in% keys_to_keep) {offer_detailed[i] <- NULL}
  }
  
  flat <- tibble(
    variable = offer_detailed$characteristics$key,
    value = offer_detailed$characteristics$value
  ) %>% 
    pivot_wider(names_from = variable, values_from = value) %>% 
    mutate(
      lat = offer_detailed$location$coordinates$latitude,
      lon = offer_detailed$location$coordinates$longitude
    )
  if (!is.null(offer_detailed$location$address$district)) {
    flat$district <- offer_detailed$location$address$district$name
  } else {
    flat$district <- NA_character_
  }
  
  flat$features <- paste(offer_detailed$features, collapse = ', ')
  flat$features2 <- paste(offer_detailed$featuresWithoutCategory, collapse = ', ')
  flat$owner_type <- offer_detailed$owner$type
  flat$owner_name <- offer_detailed$owner$name
  flat$object_name <- offer_detailed$developmentTitle
  flat$pprice <- offer_detailed$price
  flat$p_parking <- paste(offer_detailed$p_parking, collapse = ', ')
  flat$p_rooms <- paste(offer_detailed$p_rooms, collapse = ', ')
  flat$p_bwindows <- paste(offer_detailed$p_bwindows, collapse = ', ')
  flat$phone <- offer_detailed$owner$phones[[1]]
  flat$street_name <- offer_detailed$location$address$street$name
  flat$street_number <- offer_detailed$location$address$street$number
  flat$city <- offer_detailed$location$address$city$name
  
  if (!is.null(offer_detailed$property)) {
    flat$p_kitchen <- offer_detailed$property$properties$kitchen
    flat$p_parking <- if(identical(offer_detailed$property$properties$parking, list())) {''} else {offer_detailed$property$properties$parking}
    flat$p_type <- offer_detailed$property$properties$type
    flat$p_btype <- offer_detailed$property$buildingProperties$type
    flat$p_bmaterial <- offer_detailed$property$buildingProperties$material
    flat$p_bwindows <- if(identical(offer_detailed$property$buildingProperties$windows, list())) {''} else {offer_detailed$property$buildingProperties$windows}
    flat$p_bheating <- offer_detailed$property$buildingProperties$heating
  } else {
    flat$p_kitchen <- ''
    flat$p_parking <- ''
    flat$p_rooms <- ''
    flat$p_type <- ''
    flat$p_windows <- ''
    flat$p_btype <- ''
    flat$p_bmaterial <- ''
    flat$p_bwindows <- ''
    flat$p_bheating <- ''
  }
  flat$description <- offer_detailed$description %>% read_html() %>%html_text()
  flat$images <- list(offer_detailed$images$large)
  return(flat)
}

clear_flat <- function(flat) {
  flat %>% 
    rename(area = m) %>%
    mutate(
      features_lst = list(clean_letters(features)),
      floor = str_remove(floor_no, "floor\\_"),
      ownership = switch_owner(building_ownership),
      street = ifelse(street_number == "", street_name, paste0(street_name, ", ", street_number))
    ) %>% 
    select(-c(floor_no, building_ownership, features, features2, street_name, street_number)) %>% 
    select(-any_of(c("p_bmaterial", "p_btype", "p_bwindows", "p_rooms", "p_parking")))
}

# for visualizations ----
rename_features <- function(x) {
  case_when(
    x == 'internet' ~ 'Internet',
    x == "drzwi / okna antywłamaniowe" ~ 'Security windows',
    x == 'teren zamknięty' ~ 'Closed territory',
    x == 'domofon / wideofon' ~ 'Domophone',
    x == 'monitoring / ochrona' ~ 'Security',
    x == 'zmywarka' ~ 'Dishwasher',
    x == 'lodówka' ~ 'Refrigerator',
    x == 'meble' ~ 'Furniture',
    x == 'piekarnik' ~ 'Oven',
    x == 'kuchenka' ~ 'Stove',
    x == 'telewizor' ~ 'TV',
    x == 'pralka' ~ 'Washing machine',
    x == 'klimatyzacja' ~ 'Air conditioning',
    x == 'balkon' ~ 'Balcony',
    x == 'garaż/miejsce parkingowe' ~ 'Garage',
    x == 'winda' ~ 'Elevator',
    x == 'piwnica' ~ 'Basement',
    x == 'rolety antywłamaniowe' ~ 'Security blinds',
    x == 'telewizja kablowa' ~ 'Cable TV',
    x == 'oddzielna kuchnia' ~ 'Separate kitchen',
    x == 'taras' ~ 'Terrace',
    x == 'pom. użytkowe' ~ 'Utility room',
    x == 'system alarmowy' ~ 'Alarm system',
    x == 'ogródek' ~ 'Garden',
    TRUE ~ x
  )
}

show_main <- function(df) {
  df %>% 
    select(any_of(c('price', 'area', 'price_per_m', 'floor', 'rooms_num', 'market', 'ownership', 'street', 'district', 'city'))) %>% 
    rename(
      `Price per sq.m.` = price_per_m,
      `Number of rooms` = rooms_num
    ) %>% 
    mutate(
      price = format(as.numeric(price), big.mark = " "),
      `Price per sq.m.` = format(as.numeric(`Price per sq.m.`), big.mark = " ")
    ) %>% 
    pivot_longer(cols = colnames(.)) %>% 
    mutate(
      name = str_to_sentence(name),
      value = str_to_title(value)
    ) %>% 
    rename(
      `  ` = name,
      ` ` = value
    ) %>% 
    datatable(
      rownames = FALSE,
      extensions = "Buttons",
      options = list(paging = FALSE, searching = FALSE, info = FALSE, dom = 'Bfrtip', buttons = c('copy', 'excel'))
    )
}

show_features <- function(flat) {
  tibble(`Additional features` = flat$features_lst[[1]]) %>% 
    mutate(`Additional features` = rename_features(`Additional features`)) %>% 
    arrange(`Additional features`) %>% 
    rename(` ` = `Additional features`) %>% 
    datatable(
      rownames = FALSE,
      extensions = "Buttons",
      options = list(paging = FALSE, searching = FALSE, info = FALSE, dom = 'Bfrtip', buttons = c('copy', 'excel'))
    )
}

show_additional <- function(flat) {
  flat %>% 
    select(
      `Type of flat` = any_of(c('building_type')),
      `Year built` = any_of(c('build_year')),
      `Name of the building` = any_of(c('object_name')),
      `Building material` = any_of(c('building_material')),
      `Windows material` = any_of(c('windows_type')),
      `Construction status` = any_of(c('construction_status')),
      `Owner type` = any_of(c('owner_type')),
      `Owner name` = any_of(c('owner_name')),
      `Owner phone` = any_of(c('phone')),
      `Longitude` = any_of(c('lon')),
      `Latitude` = any_of(c('lat'))
    ) %>% 
    mutate_if(is.numeric, round, 5) %>% 
    mutate_all(as.character) %>% 
    mutate(`Owner phone` = paste0("+", format(as.numeric(`Owner phone`), big.mark = " "))) %>% 
    pivot_longer(cols = colnames(.)) %>% 
    mutate(
      value = str_replace_all(value, "_", " ") %>% str_to_title()
    ) %>% 
    rename(
      `  ` = name,
      ` ` = value
    ) %>% 
    datatable(
      rownames = FALSE,
      extensions = "Buttons",
      options = list(paging = FALSE, searching = FALSE, info = FALSE, dom = 'Bfrtip', buttons = c('copy', 'excel'))
    )
}

show_description <- function(flat) {
  flat %>% 
    select(` ` = any_of(c('description'))) %>% 
    datatable(
      rownames = FALSE,
      extensions = "Buttons",
      options = list(paging = FALSE, searching = FALSE, info = FALSE, dom = 'Bfrtip', buttons = c('copy', 'excel'))
    )
}