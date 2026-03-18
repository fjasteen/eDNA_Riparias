# ============================================================
# Scriptnaam: 19_riparias_edna_craywatch_gbif_maps_simplified.R
# Doel:
# 1. Inlezen Craywatch + Riparias eDNA-resultaten + shapefile
# 2. GBIF-data genereren voor 8 soorten
# 3. Statistiektabellen maken
# 4. Totaalkaarten maken
# 5. Soortspecifieke kaarten maken
#
# Vereenvoudiging:
# - GEEN afzonderlijke Riparias_bemonsterde / Riparias_nietbemonsterde files
# - ENKEL Riparias_eDNA_results.xlsx gebruiken
# ============================================================

# ---------------------------
# 1. Packages
# ---------------------------
check_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

pkgs <- c(
  "tidyverse",
  "sf",
  "ggspatial",
  "lubridate",
  "readxl",
  "rgbif",
  "janitor"
)

invisible(lapply(pkgs, check_install))

library(tidyverse)
library(sf)
library(ggspatial)
library(lubridate)
library(readxl)
library(rgbif)
library(janitor)

sf_use_s2(FALSE)

# ---------------------------
# 2. Instellingen
# ---------------------------

# PADEN
dir_input  <- "./data/input"
dir_output <- "./data/output"
dir_maps   <- file.path(dir_output, "maps")
dir_tables <- file.path(dir_output, "tables")
dir_cache  <- file.path(dir_output, "cache_tiles")

if (!dir.exists(dir_maps))   dir.create(dir_maps, recursive = TRUE)
if (!dir.exists(dir_tables)) dir.create(dir_tables, recursive = TRUE)
if (!dir.exists(dir_cache))  dir.create(dir_cache, recursive = TRUE)

# BESTANDEN
file_cw           <- file.path(dir_input, "analyse_dataset.csv")
file_shape        <- file.path(dir_input, "riparias.shp")
file_eDNA_results <- file.path(dir_input, "Riparias_eDNA_results.xlsx")
file_gbif         <- file.path(dir_input, "gbif_processed.rds")

# GBIF opnieuw downloaden?
force_download_gbif <- FALSE

# projectie
target_crs <- 3812

# Doelsoorten
target_species <- c(
  "procambarus clarkii",
  "procambarus virginalis",
  "procambarus acutus",
  "faxonius limosus",
  "faxonius virilis",
  "pacifastacus leniusculus",
  "pontastacus leptodactylus",
  "astacus astacus"
)

species_colors <- c(
  "procambarus clarkii"       = "#E41A1C",
  "procambarus virginalis"    = "#377EB8",
  "procambarus acutus"        = "#4DAF4A",
  "faxonius limosus"          = "#984EA3",
  "faxonius virilis"          = "#FFD92F",
  "pacifastacus leniusculus"  = "#FF7F00",
  "pontastacus leptodactylus" = "#A65628",
  "astacus astacus"           = "#F781BF"
)

# ---------------------------
# 3. Hulpfuncties
# ---------------------------

clean_species_name <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim()
}

label_cleaner <- function(x) {
  stringr::str_to_sentence(gsub("_", " ", x))
}

safe_filename <- function(x) {
  x %>%
    gsub("[^[:alnum:]_ ]+", "", .) %>%
    gsub("\\s+", "_", .) %>%
    tolower()
}

coerce_presence <- function(x) {
  y <- as.character(x)
  y <- stringr::str_trim(y)
  y <- stringr::str_to_lower(y)
  
  dplyr::case_when(
    is.na(y) ~ NA_real_,
    y %in% c("1", "ja", "yes", "true", "aanwezig", "present", "positief", "positive") ~ 1,
    y %in% c("0", "nee", "no", "false", "afwezig", "absent", "negatief", "negative") ~ 0,
    suppressWarnings(!is.na(as.numeric(y))) ~ as.numeric(y),
    TRUE ~ NA_real_
  )
}

download_gbif_species <- function(species_name, country = "BE", page_limit = 200, max_records = 5000) {
  message("GBIF download: ", species_name)
  
  all_res <- list()
  offset <- 0
  
  repeat {
    res <- tryCatch(
      rgbif::occ_search(
        scientificName = species_name,
        country = country,
        hasCoordinate = TRUE,
        limit = page_limit,
        start = offset
      ),
      error = function(e) NULL
    )
    
    if (is.null(res) || is.null(res$data) || nrow(res$data) == 0) break
    
    all_res[[length(all_res) + 1]] <- res$data
    
    if (nrow(res$data) < page_limit) break
    
    offset <- offset + page_limit
    if (offset >= max_records) break
    Sys.sleep(0.2)
  }
  
  if (length(all_res) == 0) return(tibble())
  
  bind_rows(all_res) %>%
    mutate(species = clean_species_name(species_name))
}

generate_gbif_data <- function(target_species, file_gbif) {
  gbif_list <- lapply(target_species, download_gbif_species)
  gbif_raw <- bind_rows(gbif_list)
  
  if (nrow(gbif_raw) == 0) {
    warning("Geen GBIF-records opgehaald.")
    saveRDS(gbif_raw, file_gbif)
    return(gbif_raw)
  }
  
  gbif_clean <- gbif_raw %>%
    mutate(
      species = clean_species_name(species),
      Longitude = suppressWarnings(as.numeric(decimalLongitude)),
      Latitude  = suppressWarnings(as.numeric(decimalLatitude)),
      eventDate = suppressWarnings(as.Date(eventDate)),
      year      = suppressWarnings(as.integer(year)),
      month     = suppressWarnings(as.integer(month))
    ) %>%
    filter(
      !is.na(Longitude),
      !is.na(Latitude),
      species %in% target_species
    ) %>%
    distinct(species, Longitude, Latitude, eventDate, .keep_all = TRUE)
  
  saveRDS(gbif_clean, file_gbif)
  gbif_clean
}

my_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.box.background = element_rect(color = "white", fill = scales::alpha("white", 0.9)),
    legend.title = element_text(face = "bold")
  )

# ---------------------------
# 4. Bestanden checken
# ---------------------------
if (!file.exists(file_cw))           stop("Niet gevonden: ", file_cw)
if (!file.exists(file_shape))        stop("Niet gevonden: ", file_shape)
if (!file.exists(file_eDNA_results)) stop("Niet gevonden: ", file_eDNA_results)

# ---------------------------
# 5. Data inlezen
# ---------------------------
message("--- Stap 1: Data inlezen ---")

# A. Craywatch
df_cw <- readr::read_csv(file_cw, show_col_types = FALSE) %>%
  janitor::clean_names()

# B. Riparias eDNA bestand
df_eDNA_bem <- readxl::read_excel(file_eDNA_results, sheet = "Bemonsterde locaties") %>%
  janitor::clean_names()

df_eDNA_niet <- readxl::read_excel(file_eDNA_results, sheet = "Niet bemonsterde locaties") %>%
  janitor::clean_names()

# C. Shapefile
shp_riparias <- sf::st_read(file_shape, quiet = TRUE) %>%
  st_transform(target_crs)

# ---------------------------
# 6. Kolomnamen harmoniseren
# ---------------------------
message("--- Stap 2: Kolommen harmoniseren ---")

# Craywatch: specieskolommen naar exact target-speciesformaat
cw_names_old <- names(df_cw)
cw_names_new <- clean_species_name(cw_names_old)

for (i in seq_along(cw_names_old)) {
  if (cw_names_new[i] %in% target_species) {
    names(df_cw)[i] <- cw_names_new[i]
  }
}

# eDNA: specieskolommen naar exact target-speciesformaat
edna_names_old <- names(df_eDNA_bem)
edna_names_new <- clean_species_name(edna_names_old)

for (i in seq_along(edna_names_old)) {
  if (edna_names_new[i] %in% target_species) {
    names(df_eDNA_bem)[i] <- edna_names_new[i]
  }
}

# ---------------------------
# 7. GBIF-data genereren of inlezen
# ---------------------------
message("--- Stap 3: GBIF-data ---")

if (!file.exists(file_gbif) || isTRUE(force_download_gbif)) {
  df_gbif <- generate_gbif_data(target_species, file_gbif)
} else {
  df_gbif <- readRDS(file_gbif)
}

# ---------------------------
# 8. Craywatch verwerken
# ---------------------------
message("--- Stap 4: Craywatch verwerken ---")

# enkel Craywatch records
if ("dat_source" %in% names(df_cw)) {
  df_cw <- df_cw %>% filter(dat_source == "craywatch_data")
}

# soortkolommen numeric maken
for (sp in target_species) {
  if (sp %in% names(df_cw)) {
    df_cw[[sp]] <- suppressWarnings(as.numeric(df_cw[[sp]]))
  } else {
    # analyse_dataset.csv bevat geen astacus astacus -> toevoegen als 0
    df_cw[[sp]] <- 0
  }
}

# coördinaten
cw_sf_raw <- df_cw %>%
  mutate(
    longitude = suppressWarnings(as.numeric(longitude)),
    latitude  = suppressWarnings(as.numeric(latitude))
  ) %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(target_crs)

cw_sf_filtered <- st_filter(cw_sf_raw, shp_riparias)

# locatie-ID
if (!"locid" %in% names(cw_sf_filtered)) {
  if ("session_nr" %in% names(cw_sf_filtered)) {
    cw_sf_filtered <- cw_sf_filtered %>% mutate(locid = session_nr)
  } else {
    cw_sf_filtered <- cw_sf_filtered %>% mutate(locid = row_number())
  }
}

# jaar/date robuust
if (!"year" %in% names(cw_sf_filtered)) {
  cw_sf_filtered <- cw_sf_filtered %>% mutate(year = NA_integer_)
}
if (!"date" %in% names(cw_sf_filtered)) {
  cw_sf_filtered <- cw_sf_filtered %>% mutate(date = NA)
}

# ---------------------------
# 9. Riparias eDNA verwerken
# ---------------------------
message("--- Stap 5: Riparias eDNA verwerken ---")

# aanwezigheid per soort naar 0/1
for (sp in target_species) {
  if (sp %in% names(df_eDNA_bem)) {
    df_eDNA_bem[[sp]] <- coerce_presence(df_eDNA_bem[[sp]])
  } else {
    warning("eDNA-kolom ontbreekt voor soort: ", sp, " -> NA toegevoegd")
    df_eDNA_bem[[sp]] <- NA_real_
  }
}

# Vallen gelegd
df_eDNA_bem <- df_eDNA_bem %>%
  mutate(
    latitude = suppressWarnings(as.numeric(stringr::str_replace_all(as.character(latitude), ",", "."))),
    longitude = suppressWarnings(as.numeric(stringr::str_replace_all(as.character(longitude), ",", "."))),
    has_coords = !is.na(latitude) & !is.na(longitude),
    is_fysiek = stringr::str_detect(
      stringr::str_to_lower(as.character(vallen_gelegd)),
      "ja|yes|true|1"
    )
  )

# niet bemonsterde locaties: aparte sheet
total_niet_uitgevoerd <- nrow(df_eDNA_niet)

# spatial
sf_edna_raw <- df_eDNA_bem %>%
  filter(has_coords) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(target_crs) %>%
  st_filter(shp_riparias) %>%
  mutate(edna_id = row_number())

# fysieke staalnames = vallen gelegd
sf_phys <- sf_edna_raw %>% filter(is_fysiek)

# long formaat eDNA
edna_long <- sf_edna_raw %>%
  pivot_longer(
    cols = any_of(target_species),
    names_to = "species",
    values_to = "present"
  ) %>%
  mutate(
    species = clean_species_name(species),
    present = as.numeric(present)
  )

# eDNA totaal presence/absence
sf_edna_pres_total <- edna_long %>% filter(present == 1)

locs_met_edna_vangst <- unique(sf_edna_pres_total$edna_id)
sf_edna_abs_total <- sf_edna_raw %>% filter(!edna_id %in% locs_met_edna_vangst)

# ---------------------------
# 10. GBIF verwerken
# ---------------------------
message("--- Stap 6: GBIF verwerken ---")

sf_gbif <- NULL
sf_gbif_historic <- NULL

if (nrow(df_gbif) > 0) {
  names(df_gbif) <- janitor::make_clean_names(names(df_gbif))
  
  lon_gbif <- names(df_gbif)[names(df_gbif) %in% c("longitude", "decimal_longitude", "decimallongitude")]
  lat_gbif <- names(df_gbif)[names(df_gbif) %in% c("latitude", "decimal_latitude", "decimallatitude")]
  
  lon_gbif <- lon_gbif[1]
  lat_gbif <- lat_gbif[1]
  
  if (is.na(lon_gbif) || is.na(lat_gbif)) {
    stop("Longitude/Latitude kolommen niet gevonden in GBIF-data.")
  }
  
  gbif_prep <- df_gbif %>%
    mutate(
      species = clean_species_name(species),
      !!lon_gbif := suppressWarnings(as.numeric(.data[[lon_gbif]])),
      !!lat_gbif := suppressWarnings(as.numeric(.data[[lat_gbif]]))
    ) %>%
    filter(
      !is.na(.data[[lon_gbif]]),
      !is.na(.data[[lat_gbif]]),
      species %in% target_species
    )
  
  sf_gbif <- gbif_prep %>%
    st_as_sf(coords = c(lon_gbif, lat_gbif), crs = 4326, remove = FALSE) %>%
    st_transform(target_crs) %>%
    st_filter(shp_riparias)
  
  if ("event_date" %in% names(gbif_prep)) {
    sf_gbif_historic <- gbif_prep %>%
      mutate(datum = as.Date(event_date)) %>%
      filter(!is.na(datum) & datum < as.Date("2024-06-01")) %>%
      st_as_sf(coords = c(lon_gbif, lat_gbif), crs = 4326, remove = FALSE) %>%
      st_transform(target_crs) %>%
      st_filter(shp_riparias)
  } else if (all(c("year", "month") %in% names(gbif_prep))) {
    sf_gbif_historic <- gbif_prep %>%
      filter(year < 2024 | (year == 2024 & month < 6)) %>%
      st_as_sf(coords = c(lon_gbif, lat_gbif), crs = 4326, remove = FALSE) %>%
      st_transform(target_crs) %>%
      st_filter(shp_riparias)
  }
}

# ---------------------------
# 11. Craywatch total presence/absence
# ---------------------------
message("--- Stap 7: Craywatch total presence/absence ---")

cw_long <- cw_sf_filtered %>%
  select(any_of(c("locid", "session_nr")), any_of(target_species), geometry) %>%
  pivot_longer(
    cols = any_of(target_species),
    names_to = "species",
    values_to = "present"
  ) %>%
  mutate(
    species = clean_species_name(species),
    present = suppressWarnings(as.numeric(present))
  )

sf_cw_pres_total <- cw_long %>% filter(!is.na(present) & present > 0)

locs_met_vangst <- unique(sf_cw_pres_total$locid)
sf_cw_abs_total <- cw_sf_filtered %>% filter(!locid %in% locs_met_vangst)

# ---------------------------
# 12. Tabellen
# ---------------------------
message("--- Stap 8: Tabellen ---")

stats_table <- tibble(
  categorie = c(
    "Craywatch",
    "Riparias eDNA",
    "Riparias sampling",
    "GBIF Totaal",
    "GBIF < Juni 2024",
    "NIET UITGEVOERD"
  ),
  aantal = c(
    if ("session_nr" %in% names(cw_sf_filtered)) n_distinct(cw_sf_filtered$session_nr) else nrow(cw_sf_filtered),
    nrow(sf_edna_raw),
    nrow(sf_phys),
    if (!is.null(sf_gbif)) nrow(sf_gbif) else 0,
    if (!is.null(sf_gbif_historic)) nrow(sf_gbif_historic) else 0,
    total_niet_uitgevoerd
  )
)

readr::write_csv(stats_table, file.path(dir_tables, "overzicht_bemonsteringen_totaal.csv"))

table_species <- cw_sf_filtered %>%
  st_drop_geometry() %>%
  pivot_longer(cols = any_of(target_species), names_to = "species", values_to = "aantal") %>%
  group_by(species) %>%
  summarise(totaal_gevangen = sum(as.numeric(aantal), na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(totaal_gevangen))

readr::write_csv(table_species, file.path(dir_tables, "statistiek_aantal_per_soort.csv"))

if ("year" %in% names(cw_sf_filtered)) {
  effort_per_year <- cw_sf_filtered %>%
    st_drop_geometry() %>%
    group_by(year) %>%
    summarise(
      aantal_vrijwilligers = if ("vrijwillid" %in% names(.)) n_distinct(vrijwillid) else NA_integer_,
      aantal_sessies = if ("session_nr" %in% names(.)) n_distinct(session_nr) else n(),
      .groups = "drop"
    )
  
  catch_per_year <- cw_sf_filtered %>%
    st_drop_geometry() %>%
    mutate(total_catch = rowSums(across(any_of(target_species)), na.rm = TRUE)) %>%
    group_by(year) %>%
    summarise(aantal_kreeften = sum(total_catch, na.rm = TRUE), .groups = "drop")
  
  absences_per_year <- cw_sf_filtered %>%
    st_drop_geometry() %>%
    mutate(total_catch = rowSums(across(any_of(target_species)), na.rm = TRUE)) %>%
    filter(total_catch == 0) %>%
    group_by(year) %>%
    summarise(aantal_absences = n(), .groups = "drop")
  
  table_year <- effort_per_year %>%
    left_join(catch_per_year, by = "year") %>%
    left_join(absences_per_year, by = "year") %>%
    mutate(
      aantal_kreeften = replace_na(aantal_kreeften, 0),
      aantal_absences = replace_na(aantal_absences, 0),
      percentage_absence = round((aantal_absences / aantal_sessies) * 100, 1)
    )
  
  readr::write_csv(table_year, file.path(dir_tables, "statistiek_jaarlijks.csv"))
}

# ---------------------------
# 13. Kaart 1: Craywatch + Riparias
# ---------------------------
message("--- Stap 9: Kaart 1 ---")

p1 <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 11, cachedir = dir_cache, progress = "none") +
  geom_sf(data = shp_riparias, fill = NA, color = "black", size = 0.6) +
  geom_sf(data = sf_cw_abs_total, aes(shape = "absences"), color = "black", size = 2.8, alpha = 0.8) +
  geom_sf(data = sf_phys, aes(shape = "Riparias sampling"), color = "black", size = 4.8, stroke = 1.2) +
  geom_sf(data = sf_cw_pres_total, aes(color = species), size = 3) +
  geom_sf(data = sf_edna_abs_total, aes(shape = "eDNA"), color = "black", size = 3.2, stroke = 1.2) +
  geom_sf(data = sf_edna_pres_total, aes(shape = "eDNA", color = species), size = 3.2, stroke = 1.2) +
  scale_color_manual(
    values = species_colors,
    labels = label_cleaner,
    name = "Species",
    drop = FALSE,
    guide = guide_legend(override.aes = list(shape = 16, size = 3))
  ) +
  scale_shape_manual(
    name = "Method",
    values = c(
      "absences" = 16,
      "eDNA" = 4,
      "Riparias sampling" = 0
    ),
    guide = guide_legend(
      override.aes = list(
        color = c("black", "black", "black"),
        size = c(3, 3.2, 4.2),
        stroke = c(1, 1.2, 1.2)
      )
    )
  ) +
  labs(
    title = "eDNA Riparias & Craywatch",
    caption = "Background: OSM"
  ) +
  my_theme

ggsave(
  file.path(dir_maps, "kaart_1_riparias_craywatch.png"),
  p1, width = 30, height = 22, units = "cm", dpi = 300, bg = "white"
)

# ---------------------------
# 14. Kaart 2: Totaal + GBIF
# ---------------------------
message("--- Stap 10: Kaart 2 ---")

if (!is.null(sf_gbif) && nrow(sf_gbif) > 0) {
  p2 <- ggplot() +
    annotation_map_tile(type = "osm", zoom = 11, cachedir = dir_cache, progress = "none") +
    geom_sf(data = shp_riparias, fill = NA, color = "black", size = 0.6) +
    geom_sf(data = sf_cw_abs_total, aes(shape = "absences"), color = "black", size = 2.8, alpha = 0.8) +
    geom_sf(data = sf_phys, aes(shape = "Riparias sampling"), color = "black", size = 4.8, stroke = 1.2) +
    geom_sf(data = sf_gbif, aes(color = species), size = 3, alpha = 0.55) +
    geom_sf(data = sf_cw_pres_total, aes(color = species), size = 3) +
    geom_sf(data = sf_edna_abs_total, aes(shape = "eDNA"), color = "black", size = 3.2, stroke = 1.2) +
    geom_sf(data = sf_edna_pres_total, aes(shape = "eDNA", color = species), size = 3.2, stroke = 1.2) +
    scale_color_manual(
      values = species_colors,
      labels = label_cleaner,
      name = "Species",
      drop = FALSE,
      guide = guide_legend(override.aes = list(shape = 16, size = 3))
    ) +
    scale_shape_manual(
      name = "Method",
      values = c(
        "absences" = 16,
        "eDNA" = 4,
        "Riparias sampling" = 0
      ),
      guide = guide_legend(
        override.aes = list(
          color = c("black", "black", "black"),
          size = c(3, 3.2, 4.2),
          stroke = c(1, 1.2, 1.2)
        )
      )
    ) +
    labs(
      title = "Updated distribution map",
      caption = "Background: OSM"
    ) +
    my_theme
  
  ggsave(
    file.path(dir_maps, "kaart_2_totaal_gbif.png"),
    p2, width = 30, height = 22, units = "cm", dpi = 300, bg = "white"
  )
}

# ---------------------------
# 15. Kaart 3: Enkel GBIF historisch
# ---------------------------
message("--- Stap 11: Kaart 3 ---")

if (!is.null(sf_gbif_historic) && nrow(sf_gbif_historic) > 0) {
  p3 <- ggplot() +
    annotation_map_tile(type = "osm", zoom = 11, cachedir = dir_cache, progress = "none") +
    geom_sf(data = shp_riparias, fill = NA, color = "black", size = 0.6) +
    geom_sf(data = sf_gbif_historic, aes(color = species), size = 3, alpha = 0.8) +
    scale_color_manual(
      values = species_colors,
      labels = label_cleaner,
      name = "Species",
      drop = FALSE
    ) +
    labs(
      title = "GBIF - before 06/2024",
      caption = "Background: OSM"
    ) +
    my_theme
  
  ggsave(
    file.path(dir_maps, "kaart_3_gbif_historic.png"),
    p3, width = 30, height = 22, units = "cm", dpi = 300, bg = "white"
  )
}

# ---------------------------
# 16. Kaart 4: per soort
# ---------------------------
message("--- Stap 12: Soortspecifieke kaarten ---")

for (sp in target_species) {
  
  message("  -> ", sp)
  
  # soortspecifieke Craywatch presences en absences
  sp_cw_pres <- cw_sf_filtered %>%
    filter(!is.na(.data[[sp]]) & .data[[sp]] > 0)
  
  sp_cw_abs <- cw_sf_filtered %>%
    filter(is.na(.data[[sp]]) | .data[[sp]] == 0)
  
  # soortspecifieke GBIF presences
  sp_gbif <- NULL
  if (!is.null(sf_gbif) && nrow(sf_gbif) > 0) {
    sp_gbif <- sf_gbif %>% filter(species == sp)
  }
  
  # soortspecifieke eDNA presences en absences
  sp_edna <- edna_long %>% filter(species == sp)
  sp_edna_pres <- sp_edna %>% filter(present == 1)
  sp_edna_abs  <- sp_edna %>% filter(is.na(present) | present == 0)
  
  p_sp <- ggplot() +
    annotation_map_tile(type = "osm", zoom = 11, cachedir = dir_cache, progress = "none") +
    geom_sf(data = shp_riparias, fill = NA, color = "black", size = 0.6) +
    
    # soortspecifieke Craywatch absences
    geom_sf(
      data = sp_cw_abs,
      aes(shape = "absences"),
      color = "black",
      size = 2.8,
      alpha = 0.8
    ) +
    
    # Riparias sampling
    geom_sf(
      data = sf_phys,
      aes(shape = "Riparias sampling"),
      color = "black",
      size = 4.8,
      stroke = 1.2
    )
  
  if (!is.null(sp_gbif) && nrow(sp_gbif) > 0) {
    p_sp <- p_sp +
      geom_sf(
        data = sp_gbif,
        aes(color = species),
        size = 3,
        alpha = 0.55
      )
  }
  
  if (nrow(sp_cw_pres) > 0) {
    p_sp <- p_sp +
      geom_sf(
        data = sp_cw_pres,
        aes(color = sp),
        size = 3
      )
  }
  
  # eDNA als bovenste laag
  if (nrow(sp_edna_abs) > 0) {
    p_sp <- p_sp +
      geom_sf(
        data = sp_edna_abs,
        aes(shape = "eDNA"),
        color = "black",
        size = 3.4,
        stroke = 1.3
      )
  }
  
  if (nrow(sp_edna_pres) > 0) {
    p_sp <- p_sp +
      geom_sf(
        data = sp_edna_pres,
        aes(shape = "eDNA"),
        color = "red",
        size = 3.4,
        stroke = 1.3
      )
  }
  
  p_sp <- p_sp +
    scale_color_manual(
      values = c(setNames(species_colors[sp], sp)),
      labels = c(label_cleaner(sp)),
      name = "Species",
      guide = guide_legend(override.aes = list(shape = 16, size = 3))
    ) +
    scale_shape_manual(
      name = "Method",
      values = c(
        "absences" = 16,
        "eDNA" = 4,
        "Riparias sampling" = 0
      ),
      guide = guide_legend(
        override.aes = list(
          color = c("black", "black", "black"),
          size = c(3, 3.4, 4.2),
          stroke = c(1, 1.3, 1.1)
        )
      )
    ) +
    labs(
      title = paste("Distribution map:", label_cleaner(sp)),
      caption = "Background: OSM"
    ) +
    my_theme
  
  filename <- paste0("kaart_soort_", safe_filename(sp), ".png")
  
  ggsave(
    filename = file.path(dir_maps, filename),
    plot = p_sp,
    width = 30,
    height = 22,
    units = "cm",
    dpi = 300,
    bg = "white"
  )
}

message("Alle kaarten gegenereerd.")