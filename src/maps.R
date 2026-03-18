# ====================================================
# Scriptnaam: 19_riparias_totaal_analyse_v2.R
# Doel: 
# 1. Integratie data & Ruimtelijke filter.
# 2. Statistiek tabel.
# 3. Kaart 1: Craywatch + Riparias (Met complete legende).
# 4. Kaart 2: Totaal + GBIF (Alles gekleurd).
# 5. Kaart 3: Enkel GBIF (Datum < 1 juni 2024).
# ====================================================

# --- 1. Packages & Instellingen ---
check_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
check_install("tidyverse")
check_install("sf")
check_install("ggspatial") 
check_install("kableExtra")
check_install("lubridate") # Nodig voor datum filters

library(tidyverse)
library(sf)
library(ggspatial)
library(kableExtra)
library(lubridate)

# PADEN
dir_input <- "./data/input"
dir_output <- "./data/output"
dir_maps <- file.path(dir_output, "maps")
dir_tables <- file.path(dir_output, "tables")

# Maak output mappen
if(!dir.exists(dir_maps)) dir.create(dir_maps, recursive = TRUE)
if(!dir.exists(dir_tables)) dir.create(dir_tables, recursive = TRUE)

# BESTANDSNAMEN
file_cw <- file.path(dir_input, "analyse_dataset.csv")
file_rip_bem <- file.path(dir_input, "Riparias_bemonsterde.csv")
file_rip_niet <- file.path(dir_input, "Riparias_nietbemonsterde.csv")
file_shape <- file.path(dir_input, "riparias.shp") 
file_gbif <- file.path(dir_input, "gbif_processed.rds")

# Check kritieke bestanden
if(!file.exists(file_cw)) stop(paste("Niet gevonden:", file_cw))
if(!file.exists(file_rip_bem)) stop(paste("Niet gevonden:", file_rip_bem))
if(!file.exists(file_shape)) stop(paste("Niet gevonden:", file_shape))
if(!file.exists(file_gbif)) warning(paste("GBIF bestand niet gevonden:", file_gbif))

# SpeciesEN & KLEUREN
target_species <- c("procambarus clarkii", "procambarus virginalis", "procambarus acutus", 
                    "faxonius limosus", "pacifastacus leniusculus", "faxonius virilis", 
                    "pontastacus leptodactylus")

species_colors <- c("procambarus clarkii" = "#E41A1C", "procambarus virginalis" = "#377EB8", 
                    "procambarus acutus" = "#4DAF4A", "faxonius limosus" = "#984EA3", 
                    "pacifastacus leniusculus" = "#FF7F00", "faxonius virilis" = "#FFFF33", 
                    "pontastacus leptodactylus" = "#A65628")

# --- 2. Data Inlezen ---
message("--- Stap 1: Data Inlezen ---")

# A. Craywatch
df_cw <- read_csv(file_cw, show_col_types = FALSE) %>%
  filter(dat.source == "craywatch_data")

# B. Riparias
df_rip_bem <- read_delim(file_rip_bem, show_col_types = FALSE, delim=";")
df_rip_niet <- read_delim(file_rip_niet, show_col_types = FALSE, delim=";")

# C. Shapefile
shp_riparias <- st_read(file_shape, quiet = TRUE) %>%
  st_transform(3812)

# --- 3. Data Verwerking & Filteren ---
message("--- Stap 2: Data Verwerken ---")

# 3.1 Craywatch Data
cw_sf_raw <- df_cw %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(3812)

cw_sf_filtered <- st_filter(cw_sf_raw, shp_riparias)

# 3.2 Riparias Data
df_rip_clean <- df_rip_bem %>%
  mutate(
    has_coords = !is.na(Latitude) & !is.na(Longitude),
    is_fysiek = str_detect(str_to_lower(as.character(`Vallen gelegd`)), "ja") 
  )

# Tellen
total_niet_uitgevoerd <- nrow(df_rip_niet) + sum(!df_rip_clean$has_coords, na.rm = TRUE)

# Filter Riparias
rip_sf_filtered <- df_rip_clean %>%
  filter(has_coords) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(3812) %>%
  st_filter(shp_riparias)

# Splitsen: eDNA vs Fysiek
sf_edna <- rip_sf_filtered
sf_phys <- rip_sf_filtered %>% filter(is_fysiek == TRUE)

# 3.3 GBIF Data (Voorbereiden)
sf_gbif <- NULL
sf_gbif_historic <- NULL # Voor kaart 3

if(file.exists(file_gbif)) {
  message("GBIF data inlezen...")
  df_gbif <- readRDS(file_gbif)
  
  if("species" %in% names(df_gbif)) {
    # Basis verwerking
    gbif_prep <- df_gbif %>%
      filter(!is.na(Longitude) & !is.na(Latitude)) %>%
      mutate(species = str_to_lower(species)) %>% 
      filter(species %in% target_species) 
    
    # Dataset 1: Alles (voor kaart 2)
    sf_gbif <- gbif_prep %>%
      st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
      st_transform(3812)
    
    # Dataset 2: Enkel < Juni 2024 (voor kaart 3)
    # We proberen 'eventDate', anders 'year'/'month'
    if("eventDate" %in% names(gbif_prep)) {
      sf_gbif_historic <- gbif_prep %>%
        mutate(datum = as.Date(eventDate)) %>%
        filter(datum < as.Date("2024-06-01")) %>%
        st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
        st_transform(3812)
    } else {
      # Fallback als eventDate ontbreekt
      sf_gbif_historic <- gbif_prep %>%
        filter(year < 2024 | (year == 2024 & month < 6)) %>%
        st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
        st_transform(3812)
    }
    
    message(paste("GBIF totaal:", nrow(sf_gbif)))
    message(paste("GBIF historisch (< juni 2024):", nrow(sf_gbif_historic)))
    
  } else {
    warning("Geen 'species' kolom gevonden in GBIF data!")
  }
}

# --- 4. Tabel Genereren ---
message("--- Stap 3: Tabel Maken ---")
# (Code ongewijzigd, tabel opslaan)
stats_table <- tibble(
  Categorie = c("Craywatch", "Riparias eDNA", "Riparias sampling", "GBIF Totaal", "GBIF < Juni 2024", "NIET UITGEVOERD"),
  Aantal = c(n_distinct(cw_sf_filtered$session_nr), nrow(sf_edna), nrow(sf_phys), 
             if(!is.null(sf_gbif)) nrow(sf_gbif) else 0,
             if(!is.null(sf_gbif_historic)) nrow(sf_gbif_historic) else 0,
             total_niet_uitgevoerd)
)
write_csv(stats_table, file.path(dir_tables, "overzicht_bemonsteringen_totaal.csv"))

# --- 5. Kaart Functies ---
# Om herhaling te voorkomen en de legende consistent te houden

# Voorbereiding Craywatch Absences/Presences
cw_long <- cw_sf_filtered %>%
  select(locID, any_of(target_species)) %>% 
  pivot_longer(cols = any_of(target_species), names_to = "species", values_to = "present")
sf_cw_pres <- cw_long %>% filter(present == 1)
locs_met_vangst <- unique(sf_cw_pres$locID)
sf_cw_abs <- cw_sf_filtered %>% filter(!locID %in% locs_met_vangst)

label_cleaner <- function(x) str_to_sentence(gsub("_", " ", x))

# THEMA DEFINITIE
my_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.box.background = element_rect(color="white", fill=alpha("white", 0.9)),
    legend.title = element_text(face="bold")
  )

message("--- Stap 4: Kaarten Genereren ---")

# =========================================================================
# KAART 1: Craywatch + Riparias (Met uitgebreide legende)
# =========================================================================
p1 <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 11, cachedir = "./cache_tiles", progress = "none") +
  geom_sf(data = shp_riparias, fill = NA, color = "black", size = 0.6) +
  
  # 1. Absences (Zwarte bollen in legende)
  geom_sf(data = sf_cw_abs, aes(shape = "absences"), color = "white", size = 3) + 
  geom_sf(data = sf_cw_abs, aes(shape = "absences"), color = "grey20", size = 3, alpha = 0.8) +
  
  # 2. Riparias eDNA (Zwarte kruisjes in legende)
  geom_sf(data = sf_edna, aes(shape = "eDNA"), color = "black", size = 3, stroke = 1.2) +
  
  # 3. Riparias Fysiek (Blauwe vierkantjes in legende)
  geom_sf(data = sf_phys, aes(shape = "Riparias sampling"), color = "blue", size = 4.5, stroke = 1.2) +
  
  # 4. Presences (Gekleurde bollen)
  geom_sf(data = sf_cw_pres, aes(color = species), size = 3) +
  
  # LEGENDE DEFINITIE
  scale_color_manual(values = species_colors, labels = label_cleaner, name = "Species", drop = FALSE) +
  
  # Hier definieren we de vormen voor de "Type" legende
  scale_shape_manual(
    name = "Method",
    values = c("absences" = 16, "eDNA" = 4, "Riparias sampling" = 0),
    # Hier forceren we de kleuren in de legende (anders worden ze allemaal zwart)
    guide = guide_legend(override.aes = list(
      color = c("black", "blue", "grey20"), # Volgorde: eDNA(B), Fysiek(Blue), Niet(Grey) -> Alfabetisch!
      # Check alfabetische volgorde van labels: 1. eDNA..., 2. Fysieke..., 3. Niet...
      # Dus: Black, Blue, Grey20
      size = c(3, 4, 3),
      stroke = c(1.2, 1.2, 1)
    ))
  ) +
  
  labs(title = "eDNA Riparias & Craywatch", caption = "Background: OSM") +
  my_theme

ggsave(file.path(dir_maps, "kaart_1_riparias_craywatch.png"), p1, width = 30, height = 22, units = "cm", dpi = 300, bg = "white")

# =========================================================================
# KAART 2: Totaal + GBIF (Alles)
# =========================================================================
if(!is.null(sf_gbif)) {
  p2 <- ggplot() +
    annotation_map_tile(type = "osm", zoom = 11, cachedir = "./cache_tiles", progress = "none") +
    geom_sf(data = shp_riparias, fill = NA, color = "black", size = 0.6) +
    
    # Absences & Riparias (Zelfde als boven)
    geom_sf(data = sf_cw_abs, aes(shape = "absences"), color = "white", size = 3) + 
    geom_sf(data = sf_cw_abs, aes(shape = "absences"), color = "grey20", size = 3) +
    geom_sf(data = sf_edna, aes(shape = "eDNA"), color = "black", size = 3, stroke = 1.2) +
    geom_sf(data = sf_phys, aes(shape = "Riparias sampling"), color = "blue", size = 4.5, stroke = 1.2) +
    
    # GBIF Presences (Als gekleurde bollen, onderop)
    geom_sf(data = sf_gbif, aes(color = species), size = 3, alpha = 0.6) +
    # Craywatch Presences (Bovenop)
    geom_sf(data = sf_cw_pres, aes(color = species), size = 3) +
    
    scale_color_manual(values = species_colors, labels = label_cleaner, name = "Species", drop = FALSE) +
    scale_shape_manual(
      name = "Method",
      values = c("absences" = 16, "eDNA" = 4, "Riparias sampling" = 0),
      guide = guide_legend(override.aes = list(color = c("black", "blue", "grey20")))
    ) +
    
    labs(title = "Updated distribution map", caption = "Background: OSM") +
    my_theme
  
  ggsave(file.path(dir_maps, "kaart_2_totaal_gbif.png"), p2, width = 30, height = 22, units = "cm", dpi = 300, bg = "white")
}

# =========================================================================
# KAART 3: Enkel GBIF (Voor Juni 2024)
# =========================================================================
if(!is.null(sf_gbif_historic)) {
  p3 <- ggplot() +
    annotation_map_tile(type = "osm", zoom = 11, cachedir = "./cache_tiles", progress = "none") +
    geom_sf(data = shp_riparias, fill = NA, color = "black", size = 0.6) +
    
    # Enkel GBIF punten
    geom_sf(data = sf_gbif_historic, aes(color = species), size = 3, alpha = 0.8) +
    
    scale_color_manual(values = species_colors, labels = label_cleaner, name = "Species", drop = FALSE) +
    
    labs(
      title = "GBIF - before 06/2024", 
      caption = "Background: OSM"
    ) +
    my_theme
  
  ggsave(file.path(dir_maps, "kaart_3_gbif_historic.png"), p3, width = 30, height = 22, units = "cm", dpi = 300, bg = "white")
  message("Kaart 3 (GBIF Historisch) opgeslagen.")
}

message("Alle kaarten gegenereerd!")

# --- 3b. Extra Statistieken Genereren (Inclusief Absences) ---
message("--- Stap 3b: Extra Statistieken (Aantallen, Jaren & Absences) ---")

# 1. Data voorbereiden (Long format)
stats_df <- cw_sf_filtered %>% 
  st_drop_geometry() %>%
  pivot_longer(cols = any_of(target_species), names_to = "Species", values_to = "Aantal")

# ---------------------------------------------------------
# TABEL A: Aantal kreeften per Species
# ---------------------------------------------------------
table_species <- stats_df %>%
  group_by(Species) %>%
  summarise(Totaal_Gevangen = sum(Aantal, na.rm = TRUE)) %>%
  arrange(desc(Totaal_Gevangen))

# Totaalregel toevoegen
total_catch_all <- sum(table_species$Totaal_Gevangen)
table_species <- table_species %>%
  add_row(Species = "TOTAAL (Alle Speciesen)", Totaal_Gevangen = total_catch_all)

write_csv(table_species, file.path(dir_tables, "statistiek_aantal_per_Species.csv"))
print(kable(table_species, caption = "Totaal aantal gevangen rivierkreeften per Species"))

# ---------------------------------------------------------
# TABEL B: Statistieken per Jaar (Uitgebreid met Absences)
# ---------------------------------------------------------

# 1. Vangsten (totaal aantal dieren) per jaar
catch_per_year <- stats_df %>%
  group_by(year) %>%
  summarise(Aantal_Kreeften = sum(Aantal, na.rm = TRUE))

# 2. Inspanning (Vrijwilligers & Sessies) per jaar
effort_per_year <- cw_sf_filtered %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarise(
    Aantal_Vrijwilligers = n_distinct(vrijwillID),
    Aantal_Sessies = n_distinct(session_nr)
  )

# 3. Absences (Nulvangsten) per jaar tellen
# We kijken per rij (sessie) of de som van de target-Speciesen 0 is
absences_per_year <- cw_sf_filtered %>%
  st_drop_geometry() %>%
  mutate(total_catch = rowSums(across(any_of(target_species)), na.rm = TRUE)) %>%
  filter(total_catch == 0) %>% # Behoud alleen de nulvangsten
  group_by(year) %>%
  summarise(Aantal_Absences = n())

# 4. Alles samenvoegen
table_year <- effort_per_year %>%
  left_join(catch_per_year, by = "year") %>%
  left_join(absences_per_year, by = "year") %>%
  mutate(
    # Vervang NA door 0 (voor jaren zonder absences of vangsten)
    Aantal_Absences = replace_na(Aantal_Absences, 0),
    Aantal_Kreeften = replace_na(Aantal_Kreeften, 0),
    # Optioneel: Percentage absences berekenen
    Percentage_Absence = round((Aantal_Absences / Aantal_Sessies) * 100, 1)
  )

# Opslaan en printen
write_csv(table_year, file.path(dir_tables, "statistiek_jaarlijks.csv"))
print(kable(table_year, caption = "Jaarlijkse statistieken (Inclusief Absences)"))