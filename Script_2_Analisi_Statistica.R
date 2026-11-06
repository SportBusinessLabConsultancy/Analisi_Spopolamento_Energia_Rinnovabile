# ==============================================================================
# PROGETTO TIROCINIO: Spopolamento e Transizione Energetica
# SCRIPT 2: Analisi Statistica, Clustering e Interpretazione Risultati
# ==============================================================================

# --- 1. CARICAMENTO LIBRERIE ---
library(tidyverse)
library(broom)
library(cluster)
library(sf)
library(writexl)
library(viridis)
library(ggplot2)
library(hrbrthemes)
library(dplyr)

# --- 2. IMPORTAZIONE DATI ---
dati_stat <- read_rds("Dataset_Finale_Rinnovabili.rds")

# --- 3. FEATURE ENGINEERING BASE ---
dati_analisi <- dati_stat %>%
  mutate(
    pot_tot_mw = pot_eolico_mw + pot_fv_mw + pot_idro_mw
  )

# --- 4. ANALISI SEPARATA PER TIPOLOGIA DI FONTE ---

# 4.1 CORRELAZIONI DISTANZA VS VARIAZIONE DEMOGRAFICA
cor_dist_eo <- cor.test(dati_analisi$var_pop_perc, dati_analisi$dist_eolico_km, use = "complete.obs")
cor_dist_fv <- cor.test(dati_analisi$var_pop_perc, dati_analisi$dist_fv_km, use = "complete.obs")
cor_dist_id <- cor.test(dati_analisi$var_pop_perc, dati_analisi$dist_idro_km, use = "complete.obs")

df_cor_distanze <- tibble(
  Fonte = c("Eolico", "Fotovoltaico", "Idroelettrico"),
  Pearson_R = c(cor_dist_eo$estimate, cor_dist_fv$estimate, cor_dist_id$estimate),
  P_Value = c(cor_dist_eo$p.value, cor_dist_fv$p.value, cor_dist_id$p.value)
)
print(df_cor_distanze)

# 4.2 VISUALIZZAZIONE GRAFICA DISTANZE 
dati_long_dist <- dati_analisi %>%
  select(PRO_COM, var_pop_perc, dist_eolico_km, dist_fv_km, dist_idro_km) %>%
  pivot_longer(cols = starts_with("dist_"), names_to = "Fonte", values_to = "Distanza_km") %>%
  mutate(Fonte = case_when(
    Fonte == "dist_eolico_km" ~ "Eolico",
    Fonte == "dist_fv_km" ~ "Fotovoltaico",
    Fonte == "dist_idro_km" ~ "Idroelettrico"
  )) %>%
  drop_na(Distanza_km)

grafico_distanze_sep <- ggplot(dati_long_dist, aes(x = Distanza_km, y = var_pop_perc, color = Fonte)) +
  geom_point(alpha = 0.15) + 
  geom_smooth(method = "lm", color = "black", se = TRUE, linetype = "dashed") +
  scale_color_manual(values = c("Eolico" = "#3498db", "Fotovoltaico" = "#f1c40f", "Idroelettrico" = "#2ecc71")) +
  facet_wrap(~ Fonte, scales = "free_x") +
  labs(
    title = "Relazione tra Prossimità agli Impianti e Variazione Popolazione",
    subtitle = "Analisi separata per tipologia di fonte (Eolico, FV, Idro)",
    x = "Distanza dall'impianto più vicino (km)",
    y = "Variazione Popolazione 2001-2021 (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(-60, 40))

print(grafico_distanze_sep)

# --- TEST: L'IDROELTTRICO ---

# Subset
analisi_idro_quota <- dati_analisi %>%
  mutate(classe_idro = ifelse(pot_idro_mw > 5, "Grande Idro", "Piccolo/Assente"))

# Identificazione top idro
top_idro <- dati_analisi %>%
  st_drop_geometry() %>% # Togliamo la geometria per leggere meglio la tabella
  select(COMUNE, pot_idro_mw, var_pop_perc, categoria_spopolamento) %>%
  arrange(desc(pot_idro_mw)) %>% # Ordiniamo dal più potente al meno potente
  head(10)

# Risultato
print(top_idro)

# 4.3 CORRELAZIONI POTENZA VS VARIAZIONE DEMOGRAFICA
cor_pot_eo <- cor.test(dati_analisi$var_pop_perc, dati_analisi$pot_eolico_mw, use = "complete.obs")
cor_pot_fv <- cor.test(dati_analisi$var_pop_perc, dati_analisi$pot_fv_mw, use = "complete.obs")
cor_pot_id <- cor.test(dati_analisi$var_pop_perc, dati_analisi$pot_idro_mw, use = "complete.obs")

df_cor_potenze <- tibble(
  Fonte = c("Eolico", "Fotovoltaico", "Idroelettrico"),
  Pearson_R = c(cor_pot_eo$estimate, cor_pot_fv$estimate, cor_pot_id$estimate),
  P_Value = c(cor_pot_eo$p.value, cor_pot_fv$p.value, cor_pot_id$p.value)
)
print(df_cor_potenze)

# --- GRAFICO DISPERSIONE: POTENZA (MW) VS VARIAZIONE POPOLAZIONE ---
dati_long_potenza <- dati_analisi %>%
  select(PRO_COM, var_pop_perc, pot_eolico_mw, pot_fv_mw, pot_idro_mw) %>%
  pivot_longer(
    cols = starts_with("pot_"), 
    names_to = "Fonte", 
    values_to = "Potenza_MW"
  ) %>%
  mutate(Fonte = case_when(
    Fonte == "pot_eolico_mw" ~ "Eolico",
    Fonte == "pot_fv_mw" ~ "Fotovoltaico",
    Fonte == "pot_idro_mw" ~ "Idroelettrico"
  )) %>%
  # Filtriamo i comuni con 0 potenza
  filter(Potenza_MW > 0) 

# Creazione del Grafico
grafico_potenze_final <- ggplot(dati_long_potenza, aes(x = Potenza_MW, y = var_pop_perc, color = Fonte)) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm", color = "black", se = TRUE, linetype = "dashed") +
  scale_color_manual(values = c(
    "Eolico" = "#3498db", 
    "Fotovoltaico" = "#f1c40f", 
    "Idroelettrico" = "#2ecc71"
  )) +
  # Usiamo la scala logaritmica sulla X perché la potenza varia da 0.1 a 2000 MW
  scale_x_log10() + 
  facet_wrap(~ Fonte, scales = "free_x") + 
  labs(
    title = "Impatto della Potenza Installata sulla Dinamica Demografica",
    subtitle = "Relazione tra MW (scala log) e Variazione Popolazione 2001-2021",
    x = "Potenza Installata (MW) - Scala Logaritmica",
    y = "Variazione Popolazione (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

print(grafico_potenze_final)

# 4.4 ANALISI POTENZA MEDIA PER CATEGORIA DEMOGRAFICA E FONTE
tabella_fonti_cat <- dati_analisi %>%
  st_drop_geometry() %>%
  group_by(categoria_spopolamento) %>%
  summarise(
    Media_Eolico = mean(pot_eolico_mw, na.rm = TRUE),
    Media_FV = mean(pot_fv_mw, na.rm = TRUE),
    Media_Idro = mean(pot_idro_mw, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with("Media_"), names_to = "Fonte", values_to = "MW_Medi") %>%
  mutate(Fonte = str_replace(Fonte, "Media_", ""))

grafico_confronto <- ggplot(tabella_fonti_cat, aes(x = categoria_spopolamento, y = MW_Medi, fill = Fonte)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.9) +
  scale_fill_manual(values = c("Eolico" = "#3498db", "FV" = "#f1c40f", "Idro" = "#2ecc71")) +
  labs(
    title = "Potenza Media FER per Categoria di Spopolamento",
    subtitle = "L'eolico domina le aree ad alto spopolamento, il FV le aree stabili",
    x = "Categoria Demografica",
    y = "Potenza Media Installata (MW)"
  ) +
  theme_minimal()

print(grafico_confronto)


# 4.5 DISTRIBUZIONE DELLA POTENZA
dati_long_potenza <- dati_analisi %>%
  select(PRO_COM, categoria_spopolamento, pot_eolico_mw, pot_fv_mw, pot_idro_mw) %>%
  pivot_longer(cols = starts_with("pot_"), names_to = "Fonte", values_to = "Potenza_MW") %>%
  filter(Potenza_MW > 0) %>%
  mutate(Fonte = case_when(
    Fonte == "pot_eolico_mw" ~ "Eolico",
    Fonte == "pot_fv_mw" ~ "Fotovoltaico",
    Fonte == "pot_idro_mw" ~ "Idroelettrico"
  ))

grafico_boxplot_sep <- ggplot(dati_long_potenza, aes(x = categoria_spopolamento, y = Potenza_MW, fill = categoria_spopolamento)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 1) +
  scale_y_log10() +
  scale_fill_brewer(palette = "Reds", direction = -1) +
  facet_wrap(~ Fonte) + # 3 grafici separati
  labs(
    title = "Intensità della Potenza Installata per Classe Demografica",
    subtitle = "Solo comuni ospitanti (Scala Logaritmica)",
    x = "",
    y = "Potenza Installata (MW) - Log10"
  ) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

print(grafico_boxplot_sep)

# 4.6 ANALISI DI PROSSIMITÀ (USO DELLE VARIABILI BINARIE BUFFER 10 KM)
analisi_buffer_10km <- dati_analisi %>%
  st_drop_geometry() %>%
  group_by(categoria_spopolamento) %>%
  summarise(
    Tot_Comuni = n(),
    Perc_Coperti_Eolico = mean(cop_eo_10km, na.rm = TRUE) * 100,
    Perc_Coperti_FV = mean(cop_fv_10km, na.rm = TRUE) * 100,
    Perc_Coperti_Idro = mean(cop_id_10km, na.rm = TRUE) * 100
  ) %>%
  mutate(across(starts_with("Perc_"), ~round(., 1)))

print(analisi_buffer_10km)


# ---5. REGRESSIONE LINEARE MULTIPLA (ANALISI COMPARATIVA FER) ---
modello_regressione <- lm(
  var_pop_perc ~ log1p(pot_eolico_mw) + dist_eolico_km + 
    log1p(pot_fv_mw)     + dist_fv_km + 
    log1p(pot_idro_mw)   + dist_idro_km, 
  data = dati_analisi
)

risultati_modello <- tidy(modello_regressione)
print(risultati_modello)

# FUNZIONE DI INTERPRETAZIONE AUTOMATICA PER OGNI FONTE
analizza_impatto <- function(fonte_nome, term_name) {
  riga <- risultati_modello %>% filter(term == term_name)
  
  if(nrow(riga) > 0) {
    p_val <- riga$p.value
    coeff <- riga$estimate
    
    if(p_val < 0.05) {
      direzione <- if(coeff < 0) "NEGATIVO (correlato a maggior spopolamento)" else "POSITIVO (correlato a stabilità/crescita)"
      cat(paste0("=> INSIGHT ", fonte_nome, ": Impatto SIGNIFICATIVO e ", direzione, " [p-val: ", round(p_val, 4), "]\n"))
    } else {
      cat(paste0("=> INSIGHT ", fonte_nome, ": Nessun impatto statisticamente significativo (p > 0.05).\n"))
    }
  }
}

cat("\n--- CONCLUSIONI AUTOMATIZZATE ---\n")
analizza_impatto("EOLICO", "log1p(pot_eolico_mw)")
analizza_impatto("FOTOVOLTAICO", "log1p(pot_fv_mw)")
analizza_impatto("IDROELETTRICO", "log1p(pot_idro_mw)")

cat("\n--- QUALITÀ DEL MODELLO (R-SQUARED) ---\n")
cat(paste0("Il modello spiega il ", round(glance(modello_regressione)$r.squared * 100, 2), "% della varianza demografica.\n"))

# SIMULATORE INTERATTIVO SHINY (IMPATTO ENERGETICO)

library(shiny)
library(bslib)

server <- function(input, output) {
  output$risultato <- renderText({
      valore <- -4.70 + 
      (log1p(input$mw_eo)   * -2.59)   + (input$dist_eo   * 0.113) +
      (log1p(input$mw_fv)   * 3.35)   + (input$dist_fv   * -0.616) +
      (log1p(input$mw_idro) * -1.44)   + (input$dist_idro * -0.027)
    
    paste0(round(valore, 2), " %")
  })
}

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  titlePanel("Simulatore Demografico SBL - Proiezione Completa"),
  
  sidebarLayout(
    sidebarPanel(
      style = "max-height: 90vh; overflow-y: auto;", # Aggiunge lo scroll se i cursori sono troppi
      helpText("Simula l'impatto combinato di tutte le fonti:"),
      
      # SEZIONE EOLICO
      strong("EOLICO"),
      sliderInput("mw_eo", "Potenza (MW):", 0, 200, 10),
      sliderInput("dist_eo", "Distanza (km):", 0, 60, 12),
      hr(),
      
      # SEZIONE FOTOVOLTAICO
      strong("FOTOVOLTAICO"),
      sliderInput("mw_fv", "Potenza (MW):", 0, 100, 44),
      sliderInput("dist_fv", "Distanza (km):", 0, 60, 5),
      hr(),
      
      # SEZIONE IDROELETTRICO
      strong("IDROELETTRICO"),
      sliderInput("mw_idro", "Potenza (MW):", 0, 200, 5),
      sliderInput("dist_idro", "Distanza (km):", 0, 60, 20),
      
      hr(),
      p("Basato sui coefficienti reali della regressione lineare.")
    ),
    
    mainPanel(
      card(
        card_header("Variazione di Popolazione Prevista"),
        card_body(
          h1(textOutput("risultato"), align = "center", style = "color: #2c3e50;"),
          p("Nota: Un valore negativo indica spopolamento.", align = "center")
        )
      )
    )
  )
)

shinyApp(ui, server)


# ---6. CLUSTERING A 5 GRUPPI (K-MEANS) ---

# Preparazione Dati
dati_cluster_5 <- dati_analisi %>%
  st_drop_geometry() %>%
  select(COMUNE, var_pop_perc, 
         pot_eolico_mw, pot_fv_mw, pot_idro_mw, 
         dist_eolico_km, dist_fv_km, dist_idro_km) %>%
  drop_na()

# Controllo correlazioni e Standardizzazione
dati_per_cor <- dati_cluster_5 %>%
  select(var_pop_perc, pot_eolico_mw, pot_fv_mw, pot_idro_mw, 
         dist_eolico_km, dist_fv_km, dist_idro_km) %>%
  drop_na()
matrice_cor <- cor(dati_per_cor, use = "complete.obs")
print(round(matrice_cor, 2))

dati_standard_5 <- scale(dati_cluster_5 %>% select(-COMUNE))

# Esecuzione K-Means con K = 5
set.seed(42) 
kmeans_5 <- kmeans(dati_standard_5, centers = 5, nstart = 25)

# Verifica Qualità
sil_5 <- silhouette(kmeans_5$cluster, dist(dati_standard_5))
cat("=> Indice di Silhouette Medio (K=5):", round(mean(sil_5[, 3]), 2), "\n")

# Aggiungiamo i cluster al dataset
dati_cluster_5$Cluster_5 <- kmeans_5$cluster

# Calcolo Medie
sil_profile <- silhouette(dati_cluster_5$Cluster_5, dist(dati_standard_5))
dati_cluster_5$sil_width <- sil_profile[, "sil_width"]

sintesi_5 <- dati_cluster_5 %>%
  group_by(Cluster_5) %>%
  summarise(
    N = n(),
    Sil_Media = mean(sil_width),
    Pop_Var = mean(var_pop_perc),
    MW_Eo = mean(pot_eolico_mw),
    MW_FV = mean(pot_fv_mw),
    MW_Idro = mean(pot_idro_mw),
    Dist_Eo = mean(dist_eolico_km),
    Dist_Idro = mean(dist_idro_km)
  )
print(sintesi_5)

# ASSEGNAZIONE NUOVE TIPOLOGIE
dati_cluster_5 <- dati_cluster_5 %>%
  mutate(Tipologia = case_when(
    Cluster_5 == 5 ~ "Grandi Hub Idroelettrici",
    Cluster_5 == 3 ~ "Poli Eolici (Crisi Demografica)",
    Cluster_5 == 1 ~ "Aree Interne e Marginali",
    Cluster_5 == 4 ~ "Centri Urbani e Resilienti",
    Cluster_5 == 2 ~ "Territori Rurali in Transizione",
    TRUE ~ "Altro"
  ))

# GRAFICO PCA
pca_5 <- prcomp(dati_standard_5)
df_pca_5 <- as.data.frame(pca_5$x[, 1:2])
df_pca_5$Tipologia <- dati_cluster_5$Tipologia

ggplot(df_pca_5, aes(x = PC1, y = PC2, color = Tipologia)) +
  geom_point(alpha = 0.6, size = 2) +
  stat_ellipse(aes(fill = Tipologia), geom = "polygon", alpha = 0.1, color = NA) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "PCA Cluster a 5 gruppi", subtitle = "Verifica della frammentazione dei profili") +
  theme_minimal() + theme(legend.position = "bottom")

ggsave("pca_finale.png", plot = last_plot(), width = 10, height = 7, dpi = 300)

# --- Cuscrotto interattivo ---

library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(htmltools)
library(sf)
library(rmapshaper)

# IMPORTAZIONE IMPIANTI (Punti) 
cat("Caricamento layer impianti...\n")

eo_punti <- st_read("dati_gis/eo_pot_comunale_2019.shp") %>%
  mutate(PRO_COM = str_pad(as.character(PRO_COM), width = 6, side = "left", pad = "0")) %>%
  filter(VALORE > 0) %>%
  st_centroid() %>% 
  st_transform(crs = 4326)

fv_punti <- st_read("dati_gis/fv_impianti_comune_2021.shp") %>%
  mutate(PRO_COM = str_pad(as.character(PRO_COM), width = 6, side = "left", pad = "0")) %>%
  filter(VALORE > 0) %>%
  st_centroid() %>%
  st_transform(crs = 4326)

id_punti <- st_read("dati_gis/po_esistenti_comunali_2021.shp") %>%
  mutate(PRO_COM = str_pad(as.character(PRO_COM), width = 6, side = "left", pad = "0"),
         VALORE = ifelse(POT_TUR_MW > 0, POT_TUR_MW, POT_PO_MW)) %>% 
  filter(VALORE > 0) %>%
  st_centroid() %>%
  st_transform(crs = 4326)

# PREPARAZIONE DATI COMUNALI (Cluster + Spopolamento)
cat("Unione dati e semplificazione geometrie...\n")

comuni_map <- dati_stat %>%
  left_join(
    dati_cluster_5 %>% select(COMUNE, Cluster_5, Tipologia), 
    by = "COMUNE"
  ) %>%
  filter(!is.na(Tipologia)) %>%
  # Semplificazione immediata per liberare RAM
  ms_simplify(keep = 0.05, keep_shapes = TRUE) %>%
  st_transform(crs = 4326)

# PALETTE
pal_clusters <- colorFactor(
  palette = c(
    "#33A02C",  # Rosso FUOCO per Grandi Hub
    "#1F78B4",
    "#E31A23",
    "#6A3D9A",  # Viola INTENSO
    "#FF7F00"   # Arancione VIVO
  ), 
  domain = comuni_map$Tipologia
)

pal_cat_spop <- colorFactor(
  palette = c("#d7191c", "#fdae61", "#1a9641"), 
  domain = comuni_map$categoria_spopolamento
)

popup_comuni <- ~paste0(
  "<div style='font-family: Arial; font-size: 14px; padding: 5px;'>",
  "<b>", COMUNE, "</b><br>",
  "<b>Cluster:</b> ", Tipologia, "<br>",
  "<b>Categoria Spopolamento:</b> ", categoria_spopolamento,
  "</div>"
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

# NOTA METODOLOGICA
nota_metodologica_html <- tags$div(
  style = "padding: 10px; background: rgba(255,255,255,0.9); 
           border-radius: 5px; font-size: 11px; max-width: 250px;
           border: 1px solid #ccc; font-family: Arial;",
  tags$b("Nota Metodologica:"), tags$br(),
  "I cluster (K=5) analizzano la correlazione tra spopolamento 
   e transizione energetica. Dati basati su censimenti ISTAT 
   e basi dati energetiche 2019-2021."
)

# COSTRUZIONE MAPPA FINALE (CON LEGENDE INTERATTIVE) 
mappa_final <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Layer Poligoni: Cluster
  addPolygons(
    data = comuni_map, 
    fillColor = ~pal_clusters(Tipologia),
    weight = 0.5, 
    color = "white", 
    fillOpacity = 0.7,
    group = "Cluster Territoriali", 
    popup = popup_comuni, 
    label = ~COMUNE
  ) %>%
  
  # Layer Poligoni: Spopolamento
  addPolygons(
    data = comuni_map, 
    fillColor = ~pal_cat_spop(categoria_spopolamento),
    weight = 0.5, 
    color = "white", 
    fillOpacity = 0.7,
    group = "Classe Spopolamento", 
    popup = popup_comuni, 
    label = ~COMUNE
  ) %>%
  
  # Punti Impianti
  addCircleMarkers(
    data = eo_punti, 
    radius = 4, 
    color = "#3498db", 
    stroke = FALSE, 
    fillOpacity = 0.8,
    group = "Impianti Eolici", 
    popup = ~paste0("Eolico: ", VALORE, " MW")
  ) %>%
  addCircleMarkers(
    data = fv_punti, 
    radius = 2, 
    color = "#f39c12", 
    stroke = FALSE, 
    fillOpacity = 0.6,
    group = "Impianti Fotovoltaici", 
    clusterOptions = markerClusterOptions()
  ) %>%
  addCircleMarkers(
    data = id_punti, 
    radius = 4, 
    color = "#00b894", 
    stroke = FALSE, 
    fillOpacity = 0.8,
    group = "Impianti Idroelettrici", 
    popup = ~paste0("Idro: ", VALORE, " MW")
  ) %>%
  
  # --- CONTROLLO LAYER (OVERLAYGROUPS con QUADRATI) ---
  addLayersControl(
    overlayGroups = c(
      "Cluster Territoriali",
      "Classe Spopolamento", 
      "Impianti Eolici", 
      "Impianti Fotovoltaici", 
      "Impianti Idroelettrici"
    ),
    options = layersControlOptions(collapsed = FALSE), 
    position = "topleft"
  ) %>%
  
  # LEGENDE INTERATTIVE
  addLegend(
    pal = pal_clusters, 
    values = comuni_map$Tipologia, 
    title = "Cluster K=5", 
    position = "bottomright", 
    group = "Cluster Territoriali"
  ) %>%
  addLegend(
    pal = pal_cat_spop, 
    values = comuni_map$categoria_spopolamento, 
    title = "Classe Demografica", 
    position = "bottomright", 
    group = "Classe Spopolamento"
  ) %>%
  
  # ELEMENTI FISSI
  addControl(logo_sbl_html, position = "topright") %>%
  addControl(legenda_markers_html, position = "topright") %>% 
  addControl(nota_metodologica_html, position = "bottomleft") %>%
  
  # Nascondi inizialmente alcuni layer
  hideGroup(c("Impianti Eolici", "Impianti Fotovoltaici", "Impianti Idroelettrici", "Classe Spopolamento"))

# Salvataggio
saveWidget(mappa_final, file = "Dashboard_SBL_Cluster.html", selfcontained = TRUE)


# --- STEP 6: ANALISI DI DENSITÀ SPAZIALE ---

# Estraiamo le coordinate direttamente dal dataset principale
# Filtriamo i comuni con variazione popolazione molto negativa (es. < -7%)
coords_crisi <- as.data.frame(st_coordinates(st_centroid(dati_analisi %>% filter(var_pop_perc < -7))))
colnames(coords_crisi) <- c("X", "Y")

# Filtriamo i comuni con potenza significativa
coords_energetici <- as.data.frame(st_coordinates(st_centroid(dati_analisi %>% filter(pot_tot_mw > 0.5))))
colnames(coords_energetici) <- c("X", "Y")

# Creiamo la mappa
mappa_densita_confronto <- ggplot() +
  # Sfondo Italia
  geom_sf(data = dati_analisi, fill = "grey95", color = "white", size = 0.05) +
  
  # Layer Densità Spopolamento (ROSSO)
  stat_density_2d(data = coords_crisi, 
                  aes(x = X, y = Y, fill = after_stat(level), alpha = after_stat(level)), 
                  geom = "polygon", bins = 15) +
  
  scale_fill_gradient(low = "#fee0d2", high = "#de2d26", name = "Densità Crisi") +
  
  # Layer Impianti
  geom_point(data = coords_energetici, 
             aes(x = X, y = Y), color = "#00ced1", size = 0.5, alpha = 0.4) +
  
  labs(title = "Analisi Spaziale: Dove la Crisi incontra l'Energia",
       subtitle = "Zone rosse: concentrazione spopolamento (< -7%) | Punti cyan: Impianti FER",
       x = NULL, y = NULL) +
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 14)) +
  guides(alpha = "none")

# 3. Visualizza e salva
#print(mappa_densita_confronto)
ggsave("mappa_test.png", mappa_densita_confronto, width = 10, height = 12, dpi = 300)

# Tabella riassuntiva di supporto all'analisi visiva
# Uniamo le metriche sensate in un'unica tabella pulita
sintesi_spaziale <- dati_analisi %>%
  st_drop_geometry() %>%
  group_by(categoria_spopolamento) %>%
  summarise(
    Numero_Comuni = n(),
    Perc_Comuni_con_Impianti = round(sum(pot_tot_mw > 0) / n() * 100, 1),
    Potenza_Media_MW = round(sum(pot_tot_mw) / n(), 2)
  )

print(sintesi_spaziale)
