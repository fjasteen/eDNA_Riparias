# =========================================================================
# INTERACTIEVE LEAFLET-KAARTEN PER SOORT (Gecorrigeerde Versie)
# =========================================================================

message("Genereren van interactieve soortspecifieke leaflet-kaarten...")

# ---- Packages ----
required_pkgs <- c("sf", "dplyr", "stringr", "leaflet", "htmlwidgets", "htmltools")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]
if(length(new_pkgs)) install.packages(new_pkgs)

library(sf)
library(dplyr)
library(stringr)
library(leaflet)
library(htmlwidgets)
library(htmltools)

# ---- Output folder ----
dir_leaflets <- "./data/output/maps/leaflets/"
if (!dir.exists(dir_leaflets)) dir.create(dir_leaflets, recursive = TRUE)

# ---- Helpers ----
label_cleaner <- function(x) stringr::str_to_sentence(gsub("_", " ", x))
safe_filename <- function(x) gsub("[^[:alnum:]_]+", "_", x)

# Zoek eerste bestaande kolomnaam uit een reeks kandidaten
get_col_or_na <- function(df, candidates) {
  hits <- candidates[candidates %in% names(df)]
  if (length(hits) == 0) return(rep(NA, nrow(df)))
  df[[hits[1]]]
}

# Jaar extraheren uit diverse datumvelden
extract_sampling_year <- function(df) {
  year_col <- c("year", "sample_year", "sampling_year", "eventyear", "eventdate", "date", "datum")
  hits <- year_col[year_col %in% names(df)]
  
  if (length(hits) == 0) return(rep(NA, nrow(df)))
  
  vals <- as.character(df[[hits[1]]])
  # Pak de eerste 4 cijfers (werkt voor YYYY en YYYY-MM-DD)
  suppressWarnings(as.integer(substr(vals, 1, 4)))
}

# HTML popup builder
make_popup_table <- function(...) {
  vals <- list(...)
  nm <- names(vals)
  
  rows <- vapply(seq_along(vals), function(i) {
    value <- vals[[i]]
    if (length(value) == 0 || is.na(value) || value == "") value <- "-"
    paste0(
      "<tr><th style='text-align:left; padding-right:8px; border-bottom:1px solid #eee;'>",
      htmlEscape(nm[i]),
      "</th><td style='border-bottom:1px solid #eee;'>",
      htmlEscape(as.character(value)),
      "</td></tr>"
    )
  }, FUN.VALUE = character(1))
  
  paste0(
    "<table style='border-collapse:collapse; font-family:sans-serif; font-size:12px;'>",
    paste(rows, collapse = ""),
    "</table>"
  )
}

# Kruis-icoon via SVG
make_cross_icon <- function(color = "black", size = 16) {
  svg <- sprintf(
    "<svg xmlns='http://www.w3.org/2000/svg' width='%s' height='%s' viewBox='0 0 24 24' fill='none' stroke='%s' stroke-width='4' stroke-linecap='round' stroke-linejoin='round'><line x1='18' y1='6' x2='6' y2='18'></line><line x1='6' y1='6' x2='18' y2='18'></line></svg>",
    size, size, color
  )
  icon_url <- paste0("data:image/svg+xml;utf8,", utils::URLencode(svg, reserved = TRUE))
  makeIcon(iconUrl = icon_url, iconWidth = size, iconHeight = size)
}

# CRS Transformatie helper
to_wgs84 <- function(x) {
  if (is.null(x) || !inherits(x, "sf")) return(NULL)
  if (is.na(sf::st_crs(x))) return(x)
  sf::st_transform(x, 4326)
}

# ---- Data voorbereiden ----
shp_riparias_ll <- to_wgs84(shp_riparias)
sf_phys_ll      <- to_wgs84(sf_phys)
cw_sf_ll        <- to_wgs84(cw_sf_filtered)
sf_edna_ll      <- to_wgs84(sf_edna_raw)
sf_gbif_ll      <- if(exists("sf_gbif")) to_wgs84(sf_gbif) else NULL

# ---- Loop per soort ----
for (sp in target_species) {
  
  message(paste("  -> Verwerken:", sp))
  
  # 1. Selecteer data
  # Presences
  sp_cw_pres <- cw_sf_ll %>% filter(!is.na(.data[[sp]]) & .data[[sp]] > 0)
  sp_cw_abs  <- cw_sf_ll %>% filter(is.na(.data[[sp]]) | .data[[sp]] == 0)
  
  sp_gbif <- NULL
  if (!is.null(sf_gbif_ll)) {
    sp_gbif <- sf_gbif_ll %>% filter(tolower(species) == tolower(sp))
  }
  
  # eDNA (detectie vs geen detectie)
  if (sp %in% names(sf_edna_ll)) {
    edna_subset <- sf_edna_ll %>%
      mutate(detected = ifelse(!is.na(.data[[sp]]) & .data[[sp]] %in% c(1, "1", TRUE, "Ja"), TRUE, FALSE))
  } else {
    message("     ! Geen eDNA data voor ", sp)
    next
  }
  
  sp_edna_pres <- edna_subset %>% filter(detected == TRUE)
  sp_edna_abs  <- edna_subset %>% filter(detected == FALSE)
  
  # 2. Popups genereren
  # eDNA popups
  if(nrow(edna_subset) > 0) {
    edna_subset <- edna_subset %>%
      mutate(popup = mapply(make_popup_table,
                            Locatiecode = get_col_or_na(., c("locatiecode", "locationcode", "sitecode")),
                            Locatie     = get_col_or_na(., c("locatie", "location", "naam")),
                            Filter      = get_col_or_na(., c("filter", "filter_id")),
                            Expected    = get_col_or_na(., c("expected", "verwacht")),
                            SIMPLIFY = TRUE, USE.NAMES = FALSE))
    sp_edna_pres <- edna_subset %>% filter(detected == TRUE)
    sp_edna_abs  <- edna_subset %>% filter(detected == FALSE)
  }
  
  # Presence popups (Jaartal)
  if(nrow(sp_cw_pres) > 0) {
    sp_cw_pres <- sp_cw_pres %>% mutate(popup = paste0("<b>Year:</b> ", extract_sampling_year(.)))
  }
  if(!is.null(sp_gbif) && nrow(sp_gbif) > 0) {
    sp_gbif <- sp_gbif %>% mutate(popup = paste0("<b>Year:</b> ", extract_sampling_year(.)))
  }
  
  # 3. Kaart opbouwen
  sp_color <- if(exists("species_colors") && sp %in% names(species_colors)) species_colors[[sp]] else "#1f78b4"
  
  m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Licht") %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OSM") %>%
    
    # Lagen
    addPolygons(data = shp_riparias_ll, color = "black", weight = 1, fill = FALSE, group = "Riparias contour") %>%
    addCircleMarkers(data = sp_cw_abs, radius = 3, color = "black", stroke = F, fillOpacity = 0.5, group = "Craywatch absence") %>%
    addCircleMarkers(data = sf_phys_ll, radius = 5, color = "black", weight = 1, fill = F, group = "Riparias sampling")
  
  if(nrow(sp_cw_pres) > 0) {
    m <- m %>% addCircleMarkers(data = sp_cw_pres, radius = 6, color = sp_color, fillColor = sp_color, 
                                fillOpacity = 0.8, popup = ~popup, group = "Craywatch presence")
  }
  if(!is.null(sp_gbif) && nrow(sp_gbif) > 0) {
    m <- m %>% addCircleMarkers(data = sp_gbif, radius = 6, color = sp_color, fillColor = sp_color, 
                                fillOpacity = 0.4, popup = ~popup, group = "GBIF presence")
  }
  
  m <- m %>%
    addMarkers(data = sp_edna_abs, icon = make_cross_icon("black"), popup = ~popup, group = "eDNA no detection") %>%
    addMarkers(data = sp_edna_pres, icon = make_cross_icon("red"), popup = ~popup, group = "eDNA detection") %>%
    
    # Controls & Legende
    addLayersControl(
      baseGroups = c("Licht", "OSM"),
      overlayGroups = c("Riparias contour", "Craywatch presence", "Craywatch absence", "GBIF presence", "eDNA detection", "eDNA no detection"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addLegend(position = "bottomright", colors = c(sp_color, "black", "red"), 
              labels = c(paste(label_cleaner(sp), "presence"), "No detection / Absence", "eDNA Positive"),
              title = "Legende")
  
  # 4. Bounding Box fix (voorkomt grijze kaarten)
  all_points <- list(sp_cw_pres, sp_cw_abs, sp_edna_pres, sp_edna_abs, shp_riparias_ll)
  valid_points <- all_points[sapply(all_points, function(x) !is.null(x) && nrow(x) > 0)]
  
  if(length(valid_points) > 0) {
    combined_bbox <- st_bbox(do.call(rbind, lapply(valid_points, function(x) st_as_sf(st_bbox(x) %>% st_as_sfc()))))
    m <- m %>% fitBounds(as.numeric(combined_bbox[1]), as.numeric(combined_bbox[2]), 
                         as.numeric(combined_bbox[3]), as.numeric(combined_bbox[4]))
  }
  
  # 5. Opslaan
  out_file <- file.path(dir_leaflets, paste0("leaflet_soort_", safe_filename(sp), ".html"))
  saveWidget(m, file = out_file, selfcontained = TRUE)
}

message("Gereed! De kaarten staan in: ", dir_leaflets)