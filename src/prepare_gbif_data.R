# ====================================================
# Scriptnaam: 02_process_gbif_riparias.R
# Doel: 
# 1. Inlezen ruwe GBIF data (uit intermediate folder).
# 2. Opschonen (kwaliteit & onzekerheid).
# 3. Ruimtelijke filter: Behoud ALLEEN punten binnen riparias.shp.
# 4. Opslaan als .rds voor gebruik in de kaart.
# ====================================================

# --- 1. Packages ---
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")

library(tidyverse)
library(sf)

# --- 2. Paden & Bestanden ---
# Input mappen (gebaseerd op je kart.R script)
dir_input <- "./data/input"
dir_gbif_intermediate <- "./data/intermediate/gbif" # Waar je ruwe download staat

# Bestandsnamen
# LET OP: Pas 'gbif_occurrences.csv' aan naar de exacte naam van je bestand in die map!
# Vaak heet dit 'occurrence.txt' als het rechtstreeks uit de zip komt.
file_gbif_raw <- file.path(dir_gbif_intermediate, "occurrence.txt") 

file_shape <- file.path(dir_input, "riparias.shp")
file_output_rds <- file.path(dir_input, "gbif_processed.rds") # Output voor je kaartscript

# Check of bestanden bestaan
if(!file.exists(file_gbif_raw)) stop(paste("GBIF bestand niet gevonden:", file_gbif_raw))
if(!file.exists(file_shape)) stop(paste("Shapefile niet gevonden:", file_shape))

# --- 3. Instellingen Filters (Vroeger in config.R) ---
# Hier kun je aanpassen hoe streng je wilt zijn
max_uncertainty_m <- 1000  # Maximaal 1000m onzekerheid
issues_to_discard <- c("ZERO_COORDINATE", "COORDINATE_OUT_OF_RANGE", "COORDINATE_INVALID")

# --- 4. Data Inlezen ---
message("--- Stap 1: Data Inlezen ---")

# A. Shapefile inlezen en naar Lambert72 (3812) zetten
shp_riparias <- st_read(file_shape, quiet = TRUE) %>%
  st_transform(3812)

message("Shapefile ingelezen.")

# B. GBIF Data inlezen
# show_col_types = FALSE om de console schoon te houden
gbif_raw <- read_delim(file_gbif_raw, delim = "\t", show_col_types = FALSE) 
# TIP: Als read_delim niet werkt (omdat het een csv is met komma's), gebruik dan:
# gbif_raw <- read_csv(file_gbif_raw, show_col_types = FALSE)

message(paste("Totaal aantal ruwe GBIF records:", nrow(gbif_raw)))


# --- 5. Opschonen & Filteren ---
message("--- Stap 2: Kwaliteitsfilters toepassen ---")

# We checken eerst of de kolommen bestaan (GBIF namen variëren soms)
# Standaard GBIF download gebruikt vaak: "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters"
# Soms heten ze "Latitude", "Longitude". We maken ze uniform.

# Hernoem kolommen indien nodig (zodat ze matchen met je logica)
if("decimalLatitude" %in% names(gbif_raw)) {
  gbif_raw <- gbif_raw %>% rename(Latitude = decimalLatitude, Longitude = decimalLongitude)
}

gbif_clean <- gbif_raw %>%
  # 1. Verwijder rijen zonder coordinaten
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  
  # 2. Unieke records (soms zitten er dubbels in downloads)
  distinct(occurrenceID, .keep_all = TRUE) %>%
  
  # 3. Filter op issues (als de kolom 'issue' bestaat)
  filter(if("issue" %in% names(.)) !issue %in% issues_to_discard else TRUE) %>%
  
  # 4. Filter op onzekerheid (als de kolom bestaat)
  filter(
    if("coordinateUncertaintyInMeters" %in% names(.)) {
      coordinateUncertaintyInMeters <= max_uncertainty_m | is.na(coordinateUncertaintyInMeters)
    } else { TRUE }
  )

message(paste("Aantal records na kwaliteitsfilters:", nrow(gbif_clean)))


# --- 6. Ruimtelijke Filter (Riparias Shape) ---
message("--- Stap 3: Ruimtelijke Filter (Binnen Riparias.shp) ---")

# Converteren naar SF object (WGS84 -> 4326)
gbif_sf <- gbif_clean %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(3812) # Transformeren naar Lambert72 om te matchen met shapefile

# De daadwerkelijke "Knip" operatie
# st_filter behoudt alleen punten die fysiek in de polygonen liggen
gbif_final_sf <- st_filter(gbif_sf, shp_riparias)

# Terug omzetten naar dataframe (zonder geometry kolom, voor makkelijke opslag)
# Of als SF object opslaan (handiger voor je kaart script). 
# Omdat je kaart script verwacht dat het een dataframe/tibble is die we opnieuw converten,
# doen we st_drop_geometry, MAAR we zorgen dat Lat/Lon kolommen er nog zijn (remove=FALSE hierboven).
gbif_final_df <- gbif_final_sf %>%
  st_drop_geometry()

message(paste("Aantal records BINNEN Riparias gebied:", nrow(gbif_final_df)))


# --- 7. Opslaan ---
message("--- Stap 4: Opslaan ---")

saveRDS(gbif_final_df, file_output_rds)

message(paste("Klaar! Bestand opgeslagen als:", file_output_rds))