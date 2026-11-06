# ==============================================================================
# PROGETTO TIROCINIO: Spopolamento e Transizione Energetica
# SCRIPT 1: Importazione, Pre-processing, Join Spaziale e WebGIS (Leaflet)
# ==============================================================================

# --- STEP 1: CARICAMENTO LIBRERIE ---

library(sf)
library(tidyverse)
library(readxl)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)
library(rmapshaper) 
library(writexl)

# --- STEP 2: IMPORTAZIONE DEI DATI ---

# 2.1 Layer geografici (Shapefile)
comuni <- st_read("dati_gis/comuni_2025.shp")
eolico <- st_read("dati_gis/eo_pot_comunale_2019.shp")
fotovoltaico <- st_read("dati_gis/fv_impianti_comune_2021.shp")
idroelettrico <- st_read("dati_gis/po_esistenti_comunali_2021.shp")

# 2.2 Importiamo i dati demografici
demografia <- read_excel("dati_gis/Dataset Comuni - Spopolamento e Servizi Italia.xlsx")

# --- STEP 3: PULIZIA CODICI ISTAT ---
comuni <- comuni %>% mutate(PRO_COM = str_pad(as.character(PRO_COM), width = 6, side = "left", pad = "0"))
demografia <- demografia %>% mutate(PRO_COM = str_pad(as.character(PRO_COM), width = 6, side = "left", pad = "0"))
idroelettrico <- idroelettrico %>% mutate(PRO_COM = str_pad(as.character(PRO_COM), width = 6, side = "left", pad = "0"))
fotovoltaico <- fotovoltaico %>% mutate(PRO_COM = str_pad(as.character(PRO_COM), width = 6, side = "left", pad = "0"))
eolico <- eolico %>% mutate(PRO_COM = str_pad(as.character(PRO_COM), width = 6, side = "left", pad = "0"))

# --- STEP 4: JOIN DEMOGRAFIA E INDICATORE DI SPOPOLAMENTO ---
comuni_con_dati <- comuni %>%
  left_join(demografia, by = "PRO_COM")

comuni_analisi <- comuni_con_dati %>%
  mutate(
    # 1. Calcolo variazione percentuale (2001-2021)
    var_pop_perc = ((pop_2021 - pop_2001) / pop_2001) * 100,
    
    # 2. Classificazione demografica
    categoria_spopolamento = case_when(
      var_pop_perc < -20 ~ "Alto spopolamento",
      var_pop_perc >= -20 & var_pop_perc < -5 ~ "Medio spopolamento",
      var_pop_perc >= -5 ~ "Stabile o in crescita",
      TRUE ~ NA_character_ 
    ),
    
    # 3. Trasformazione in factor ordinato
    categoria_spopolamento = factor(
      categoria_spopolamento,
      levels = c("Alto spopolamento", "Medio spopolamento", "Stabile o in crescita")
    )
  )

cat("Step 4 completato: Categorie di spopolamento calcolate.\n")

# --- STEP 5: ANALISI SPAZIALE ---

# 5.1 Riproiezione (Metri - UTM 32N)
comuni_utm <- st_transform(comuni_analisi, crs = 32632)
eo_attivi <- st_transform(eolico, crs = 32632) %>% filter(VALORE > 0)
fv_attivi <- st_transform(fotovoltaico, crs = 32632) %>% filter(VALORE > 0)
id_attivi <- st_transform(idroelettrico, crs = 32632) %>% filter(POT_TUR_MW > 0 | POT_PO_MW > 0)

# 5.2 Centroidi comunali
comuni_centri_utm <- st_centroid(comuni_utm)

# 5.3 Calcolo indici vicinanza
idx_eo <- st_nearest_feature(comuni_centri_utm, eo_attivi)
idx_fv <- st_nearest_feature(comuni_centri_utm, fv_attivi)
idx_id <- st_nearest_feature(comuni_centri_utm, id_attivi)

# 5.4 Creazione colonne finali in KM e BINARIE (Fasce 5, 10, 20 km)
comuni_utm <- comuni_utm %>%
  mutate(
    # 1. Calcolo distanze direttamente in km (Variabili Continue)
    dist_eolico_km = as.numeric(st_distance(comuni_centri_utm, eo_attivi[idx_eo, ], by_element = TRUE)) / 1000,
    dist_fv_km     = as.numeric(st_distance(comuni_centri_utm, fv_attivi[idx_fv, ], by_element = TRUE)) / 1000,
    dist_idro_km   = as.numeric(st_distance(comuni_centri_utm, id_attivi[idx_id, ], by_element = TRUE)) / 1000,
    
    # 2. Coperture EOLICO (Binario 0/1)
    cop_eo_5km  = ifelse(dist_eolico_km <= 5, 1, 0),
    cop_eo_10km = ifelse(dist_eolico_km <= 10, 1, 0),
    cop_eo_20km = ifelse(dist_eolico_km <= 20, 1, 0),
    
    # 3. Coperture FOTOVOLTAICO (Binario 0/1)
    cop_fv_5km  = ifelse(dist_fv_km <= 5, 1, 0),
    cop_fv_10km = ifelse(dist_fv_km <= 10, 1, 0),
    cop_fv_20km = ifelse(dist_fv_km <= 20, 1, 0),
    
    # 4. Coperture IDROELETTRICO (Binario 0/1)
    cop_id_5km  = ifelse(dist_idro_km <= 5, 1, 0),
    cop_id_10km = ifelse(dist_idro_km <= 10, 1, 0),
    cop_id_20km = ifelse(dist_idro_km <= 20, 1, 0)
  )

summary(comuni_utm %>% select(starts_with("cop_")))

# --- STEP 6: COSTRUZIONE DEI BUFFER E UNIONE (DISSOLVE) ---

# 6.1 EOLICO 
eo_punti <- st_centroid(eo_attivi)
buffer_eolico_5km  <- st_union(st_buffer(eo_punti, dist = 5000))
buffer_eolico_10km <- st_union(st_buffer(eo_punti, dist = 10000))
buffer_eolico_20km <- st_union(st_buffer(eo_punti, dist = 20000))

# 6.2 FOTOVOLTAICO 
fv_punti <- st_centroid(fv_attivi)
buffer_fv_5km  <- st_union(st_buffer(fv_punti, dist = 5000))
buffer_fv_10km <- st_union(st_buffer(fv_punti, dist = 10000))
buffer_fv_20km <- st_union(st_buffer(fv_punti, dist = 20000))

# 6.3 IDROELETTRICO
id_punti <- st_centroid(id_attivi)
buffer_idro_5km  <- st_union(st_buffer(id_punti, dist = 5000))
buffer_idro_10km <- st_union(st_buffer(id_punti, dist = 10000))
buffer_idro_20km <- st_union(st_buffer(id_punti, dist = 20000))

# --- STEP 7: AGGREGAZIONE POTENZE E JOIN FINALE AL COMUNE ---

eo_comunale <- eolico %>%
  st_drop_geometry() %>%
  filter(VALORE > 0) %>%
  group_by(PRO_COM) %>%
  summarise(n_impianti_eo = n(), pot_eolico_mw = sum(VALORE, na.rm = TRUE))

fv_comunale <- fotovoltaico %>%
  st_drop_geometry() %>%
  filter(VALORE > 0) %>%
  group_by(PRO_COM) %>%
  summarise(n_impianti_fv = n(), pot_fv_mw = sum(VALORE, na.rm = TRUE))

id_comunale <- idroelettrico %>%
  st_drop_geometry() %>%
  filter(POT_TUR_MW > 0 | POT_PO_MW > 0) %>% 
  group_by(PRO_COM) %>%
  summarise(n_impianti_idro = n(), pot_idro_mw = sum(POT_TUR_MW + POT_PO_MW, na.rm = TRUE))

# Join finale 
comuni_finali <- comuni_utm %>%
  left_join(eo_comunale, by = "PRO_COM") %>%
  left_join(fv_comunale, by = "PRO_COM") %>%
  left_join(id_comunale, by = "PRO_COM") %>%
  mutate(across(c(n_impianti_eo, pot_eolico_mw, n_impianti_fv, pot_fv_mw, n_impianti_idro, pot_idro_mw), 
                ~replace_na(., 0)))

# --- STEP 8: PREPARAZIONE DATASET FINALI (CLEAN) ---
# 1. DATASET PER MAPPA LEAFLET (Include geometria e conteggi per i popup)
progetto_clean_sf <- comuni_finali %>%
  select(
    # --- Identificativi ---
    PRO_COM,                 
    COMUNE = COMUNE.x,       
    
    # --- Demografia ---
    pop_2001,                
    pop_2021,                
    var_pop_perc,            
    categoria_spopolamento,  
    
    # --- Analisi Spaziale: Distanze (Variabili continue) ---
    dist_eolico_km,
    dist_fv_km,
    dist_idro_km,
    
    # --- Analisi Spaziale: Coperture Buffer (Variabili binarie) ---
    starts_with("cop_eo"),   
    starts_with("cop_fv"),
    starts_with("cop_id"),
    
    # --- Dati Impianti: Potenza e Numero (Per i Popup) ---
    n_impianti_eo, pot_eolico_mw,   # Numero e MW Eolico
    n_impianti_fv, pot_fv_mw,       # Numero e MW Fotovoltaico
    n_impianti_idro, pot_idro_mw, # Numero e MW Idro
    
    # --- Geometria ---
    geometry                 
  )

# --- Step 9: CRUSCOTTO PROFESSIONALE - SPOPOLAMENTO E RINNOVABILI ---

# ------------------------------------------------------------------------------
# 1. SEMPLIFICAZIONE E TRASFORMAZIONE
# ------------------------------------------------------------------------------

comuni_light <- ms_simplify(progetto_clean_sf, keep = 0.05, keep_shapes = TRUE)
comuni_map   <- st_transform(comuni_light, crs = 4326) %>%
  mutate(pot_tot_mw = pot_eolico_mw + pot_fv_mw + pot_idro_mw)

# Trasformazione Impianti
eolico_map <- st_transform(eo_punti, crs = 4326)
fv_map     <- st_transform(fv_punti, crs = 4326)
idro_map   <- st_transform(id_punti, crs = 4326) %>%
  mutate(POT_TOT_MW = replace_na(POT_TUR_MW, 0) + replace_na(POT_PO_MW, 0))

# Trasformazione Buffer 5km
b_eo_5km  <- st_transform(buffer_eolico_5km, crs = 4326)
b_fv_5km  <- st_transform(buffer_fv_5km, crs = 4326)
b_id_5km  <- st_transform(buffer_idro_5km, crs = 4326)

# ------------------------------------------------------------------------------
# 2. PREPARAZIONE POPUP E CONTROLLI DATI
# ------------------------------------------------------------------------------

popup_comuni <- ~paste0(
  "<div style='font-family: Arial; font-size: 12px;'>",
  "<b style='color: #2e7d32; font-size: 14px;'>", COMUNE, "</b><br><hr style='margin: 5px 0;'>",
  "<b>Variazione Demografica:</b> ", round(var_pop_perc, 1), "%<br>",
  "<b>Categoria:</b> ", categoria_spopolamento, "<br><br>",
  "<b>Potenza Totale Installata:</b> ", round(pot_tot_mw, 2), " MW<br>",
  "<b>N. Impianti nel Comune:</b><br>",
  "- Eolico: ", n_impianti_eo, "<br>",
  "- Fotovoltaico: ", n_impianti_fv, "<br>",
  "- Idroelettrico: ", n_impianti_idro,
  "</div>"
)

# ------------------------------------------------------------------------------
# 3. PALETTE E ELEMENTI HTML (Logo, Legenda Marker, Nota)
# ------------------------------------------------------------------------------

pal_spopolamento <- colorFactor(
  palette = c("#d73027", "#fdae61", "#1a9850"), 
  domain = comuni_map$categoria_spopolamento, 
  na.color = "transparent"
)

pal_potenza <- colorBin(
  palette = "YlOrBr", 
  domain = comuni_map$pot_tot_mw,
  bins = c(0, 0.1, 1, 5, 10, 50, 100, Inf),
  na.color = "transparent"
)

titolo_html <- tags$div(
  style = "padding: 10px; background-color: rgba(255,255,255,0.9); border-radius: 5px; font-family: Arial; font-weight: bold; border: 2px solid #2e7d32; box-shadow: 3px 3px 5px grey;", 
  "Analisi Spopolamento e Transizione Energetica (2001-2021)"
)

logo_sbl_html <- HTML('
<div>
  <a href="https://sblconsultancy.it/" target="_blank">
    <img src="https://portiamovalore.uniba.it/uploads/loghi/LinkedIn%20Logo_1639558059.png" style="height:60px; opacity: 0.95; cursor: pointer;">
  </a>
</div>')

legenda_markers_html <- tags$div(
  style = "padding: 8px; background-color: rgba(255,255,255,0.9); border-radius: 5px; font-family: Arial; font-size: 11px; border: 1px solid #999; line-height: 1.5;",
  tags$b("Legenda Impianti"), tags$br(),
  tags$span(style = "color: #3498db; font-size: 14px;", "●"), " Eolico", tags$br(),
  tags$span(style = "color: #f39c12; font-size: 14px;", "●"), " Fotovoltaico", tags$br(),
  tags$span(style = "color: #00b894; font-size: 14px;", "●"), " Idroelettrico"
)

nota_metodologica_html <- tags$div(
  style = "background: rgba(255, 255, 255, 0.98); padding: 15px; border-radius: 10px; border: 1px solid #2e7d32; max-width: 280px; max-height: 280px; overflow-y: auto; font-family: 'Segoe UI', Arial, sans-serif; font-size: 11.5px; color: #333; line-height: 1.6; box-shadow: 0 4px 25px rgba(0,0,0,0.2);",
  tags$h3(style = "color: #2e7d32; margin-top: 0; border-bottom: 2px solid #2e7d32; padding-bottom: 10px; font-size: 16px;", "Nota Metodologica"),
  tags$b("1. Disegno di Ricerca e Obiettivi"),
  tags$p("Lo studio indaga le dinamiche di spopolamento dei comuni italiani (2001-2021) in relazione alla diffusione delle infrastrutture energetiche da Fonti Rinnovabili (FER). L'obiettivo è verificare se la presenza di impianti eolici, fotovoltaici e idroelettrici possa agire come driver di resilienza per le aree interne marginalizzate o se sia correlata a fenomeni di ulteriore declino demografico."),
  tags$b("2. Fonti Dati e Pre-processing"),
  tags$p("L'analisi integra dati eterogenei in un unico ambiente georeferenziato:"),
  tags$ul(style = "padding-left: 18px;",
          tags$li(tags$b("Dati Demografici (ISTAT):"), " Serie storiche 2001-2021 normalizzate su basi territoriali aggiornate per il calcolo della Variazione Percentuale."),
          tags$li(tags$b("Database Impianti FER:"), " Censimento nazionale degli impianti Eolici, Fotovoltaici e Idroelettrici (coordinate e potenza nominale in MW).")),
  tags$b("3. Classificazione della Dinamica Demografica"),
  tags$p("La variazione della popolazione è stata segmentata per isolare i cluster di crisi strutturale:"),
  tags$ul(style = "padding-left: 18px;",
          tags$li(tags$b("Alto spopolamento:"), " var. < -20%."),
          tags$li(tags$b("Medio spopolamento:"), " var. tra -5% e -20%."),
          tags$li(tags$b("Stabile o in crescita:"), " var. > -5% (resilienza locale).")),
  tags$b("4. Analisi Geospaziale (GIS)"),
  tags$p("Sistema di riferimento: UTM Zona 32N (EPSG:32632) per misurazioni metriche planimetriche. Operazioni:"),
  tags$ul(style = "padding-left: 18px;",
          tags$li(tags$b("Centroidi:"), " Estrazione dei centri geometrici comunali per l'analisi di prossimità."),
          tags$li(tags$b("Buffer (Isocrone di Rispetto):"), " Generazione di aree di influenza a 5, 10 e 20 km dai siti produttivi."),
          tags$li(tags$b("Aggregazione Spaziale:"), " Computo della potenza totale installata (MW) e densità di impianti per unità amministrativa.")),
  tags$b("5. Analisi Statistica e Modellizzazione"),
  tags$p("L'approccio statistico prevede l'applicazione di test di correlazione di Pearson per misurare il grado di dipendenza tra potenza installata e dinamiche demografiche, supportato da modelli di regressione per isolare l'effetto delle FER rispetto alle variabili geografiche latitudinali."),
  tags$b("6. Strumenti Software"),
  tags$p("Pipeline sviluppata interamente in linguaggio R tramite l'integrazione dei pacchetti: sf (geospaziale), tidyverse (data manipulation), leaflet (visualizzazione interattiva) e rmapshaper (ottimizzazione topologica).")
)

# ------------------------------------------------------------------------------
# 4. COSTRUZIONE MAPPA LEAFLET (FIX LEGENDE NATIVE E OVERLAYGROUPS)
# ------------------------------------------------------------------------------

mappa_progetto_final <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addMapPane("sfondo", zIndex = 210) %>%
  addMapPane("poligoni", zIndex = 220) %>%
  addMapPane("buffer", zIndex = 230) %>%
  addMapPane("punti", zIndex = 430) %>% 
  addMapPane("etichettetesto", zIndex = 450) %>%
  
  addProviderTiles("Esri.WorldGrayCanvas", options = providerTileOptions(pane = "sfondo")) %>%
  
  # --- LAYER BASE 1: SPOPOLAMENTO ---
  addPolygons(
    data = comuni_map, fillColor = ~pal_spopolamento(categoria_spopolamento), 
    weight = 0.5, color = "white", fillOpacity = 0.7, 
    group = "Coropletica Spopolamento", options = pathOptions(pane = "poligoni"),
    popup = popup_comuni
  ) %>%
  
  # --- LAYER BASE 2: POTENZA TOTALE ---
  addPolygons(
    data = comuni_map, fillColor = ~pal_potenza(pot_tot_mw), 
    weight = 0.5, color = "white", fillOpacity = 0.7, 
    group = "Potenza Totale Comune (MW)", options = pathOptions(pane = "poligoni"),
    popup = popup_comuni
  ) %>%
  
  # --- BUFFER 5 KM ---
  addPolygons(data = b_eo_5km, fillColor = "#3498db", color = "#2980b9", weight = 1, fillOpacity = 0.15, group = "Buffer Eolico 5km", options = pathOptions(pane = "buffer")) %>%
  addPolygons(data = b_fv_5km, fillColor = "#f1c40f", color = "#f39c12", weight = 1, fillOpacity = 0.15, group = "Buffer FV 5km", options = pathOptions(pane = "buffer")) %>%
  addPolygons(data = b_id_5km, fillColor = "#00b894", color = "#008f73", weight = 1, fillOpacity = 0.15, group = "Buffer Idro 5km", options = pathOptions(pane = "buffer")) %>%
  
  # --- MARKER IMPIANTI (PULITI) ---
  addCircleMarkers(
    data = eolico_map, radius = 5, color = "white", weight = 1, fillColor = "#3498db", fillOpacity = 0.9, 
    group = "Impianti Eolici", options = pathOptions(pane = "punti"),
    popup = ~paste0("<b>Tipologia:</b> Eolico Onshore<br><b>Potenza:</b> ", VALORE, " MW")
  ) %>%
  addCircleMarkers(
    data = fv_map, radius = 5, color = "white", weight = 1, fillColor = "#f39c12", fillOpacity = 0.9, 
    group = "Impianti Fotovoltaici", options = pathOptions(pane = "punti"), 
    clusterOptions = markerClusterOptions(),
    popup = ~paste0("<b>Tipologia:</b> Fotovoltaico<br><b>Potenza:</b> ", VALORE, " MW")
  ) %>%
  addCircleMarkers(
    data = idro_map, radius = 5, color = "#333333", weight = 1.2, fillColor = "#00b894", fillOpacity = 0.9, 
    group = "Impianti Idroelettrici", options = pathOptions(pane = "punti"), 
    popup = ~paste0("<b>Tipologia:</b> Idroelettrico<br><b>Potenza Totale:</b> ", POT_TOT_MW, " MW")
  ) %>%
  
  # --- CONTROLLI E LEGENDE (ORA CON GROUP NATIVO) ---
  addControl(logo_sbl_html, position = "bottomright") %>%
  addControl(titolo_html, position = "topright") %>%
  addControl(nota_metodologica_html, position = "bottomleft") %>%
  addControl(legenda_markers_html, position = "bottomright") %>%
  
  addLegend(data = comuni_map, pal = pal_spopolamento, values = ~categoria_spopolamento, title = "Classe Spopolamento", position = "topright", group = "Coropletica Spopolamento") %>%
  addLegend(data = comuni_map, pal = pal_potenza, values = ~pot_tot_mw, title = "Potenza MW Comune", position = "topright", group = "Potenza Totale Comune (MW)") %>%
  
  addLayersControl(
    overlayGroups = c(
      "Coropletica Spopolamento", 
      "Potenza Totale Comune (MW)", 
      "Impianti Eolici", 
      "Impianti Fotovoltaici", 
      "Impianti Idroelettrici", 
      "Buffer Eolico 5km", 
      "Buffer FV 5km", 
      "Buffer Idro 5km"
    ),
    options = layersControlOptions(collapsed = FALSE), position = "topleft"
  ) %>%
  
  hideGroup(c("Potenza Totale Comune (MW)", "Buffer Eolico 5km", "Buffer FV 5km", "Buffer Idro 5km", "Impianti Eolici", "Impianti Fotovoltaici", "Impianti Idroelettrici")) %>%
  
  onRender("
    function(el, x) { 
      L.control.zoom({position: 'bottomright'}).addTo(this); 
      var labels = document.querySelectorAll('.leaflet-control-layers-overlays label');
      for (var i = 0; i < labels.length; i++) {
        if (labels[i].innerText.includes('Potenza Totale Comune (MW)')) {
          labels[i].style.borderBottom = '1px solid #2e7d32'; 
          labels[i].style.paddingBottom = '8px';
          labels[i].style.marginBottom = '8px';
        }
      }
    }
  ")

# --- STEP 10: SALVATAGGIO OUTPUT FINALI (MAPPA, EXCEL E RDS) ---

# SALVATAGGIO MAPPA INTERATTIVA (HTML)
saveWidget(mappa_progetto_final, file = "Dashboard_Progetto_Rinnovabili_EsriGray.html", selfcontained = TRUE)

# Creiamo l'oggetto definitivo filtrando i comuni senza dati demografici
progetto_analisi_statistica <- comuni_finali %>%
  filter(!is.na(categoria_spopolamento)) %>%
  select(
    # --- Identificativi ---
    PRO_COM, 
    COMUNE = COMUNE.x,
    
    # --- Variabili Target (Demografia) ---
    pop_2001, 
    pop_2021, 
    var_pop_perc, 
    categoria_spopolamento,
    
    # --- Variabili Esplicative 1: Impianti e Potenza ---
    n_impianti_eo, pot_eolico_mw,
    n_impianti_fv, pot_fv_mw,
    n_impianti_idro, pot_idro_mw,
    
    # --- Variabili Esplicative 2: Distanze (Variabili Continue) ---
    dist_eolico_km, 
    dist_fv_km, 
    dist_idro_km,
    
    # --- Variabili Esplicative 3: Coperture (Variabili Binarie 0/1) ---
    starts_with("cop_"),
    
    # --- Geometria ---
    geometry 
  )

# --- SALVATAGGIO OUTPUT ---

# 1. RDS: Il formato migliore per R (mantiene classi, factor e geometrie)
saveRDS(progetto_analisi_statistica, "Dataset_Finale_Rinnovabili.rds")

library(writexl)
# 2. EXCEL:
progetto_analisi_statistica %>% 
  st_drop_geometry() %>% 
  write_xlsx("Tabella_Analisi_Comuni_Italia.xlsx")
