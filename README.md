# 🌱 Analisi Spopolamento e Energia Rinnovabile

Analisi spaziale e statistica della relazione tra spopolamento demografico e diffusione delle infrastrutture da Fonti Rinnovabili (FER) nei comuni italiani, sviluppata in R.

---

## 🗺️ Dashboard interattive

👉 [Dashboard Progetto Rinnovabili](https://sportbusinesslabconsultancy.github.io/Analisi_Spopolamento_Energia_Rinnovabile/Dashboard_Progetto_Rinnovabili_EsriGray.html)

👉 [Dashboard Cluster SBL](https://sportbusinesslabconsultancy.github.io/Analisi_Spopolamento_Energia_Rinnovabile/Dashboard_SBL_Cluster_EsriGray.html)

---

## 📌 Descrizione

Lo studio indaga le dinamiche di spopolamento dei comuni italiani (2001–2021) in relazione alla diffusione delle infrastrutture energetiche da Fonti Rinnovabili (FER). L'obiettivo è verificare se la presenza di impianti eolici, fotovoltaici e idroelettrici possa agire come driver di resilienza per le aree interne marginalizzate o se sia correlata a fenomeni di ulteriore declino demografico.

L'analisi integra dati eterogenei in un unico ambiente georeferenziato, combinando serie storiche demografiche ISTAT con il censimento nazionale degli impianti FER, per produrre due dashboard interattive che esplorano i pattern territoriali e i cluster statistici.

---

## 🎯 Obiettivi

- Analizzare le dinamiche di spopolamento dei comuni italiani (2001–2021) in relazione alla diffusione delle infrastrutture FER (impianti eolici, fotovoltaici e idroelettrici)
- Verificare se la presenza di impianti rinnovabili possa agire come driver di resilienza demografica per le aree interne marginalizzate
- Costruire un indicatore di spopolamento comunale basato sulla variazione percentuale della popolazione con classificazione in tre categorie (alto spopolamento, medio, stabile/crescita)
- Analizzare la distribuzione spaziale degli impianti FER e la loro densità per unità amministrativa
- Esplorare la correlazione tra potenza installata e dinamiche demografiche tramite analisi statistica e clustering territoriale

---

## 🔬 Metodologia

**Fonti dati e pre-processing**
L'analisi integra dati eterogenei in un unico ambiente georeferenziato: dati demografici ISTAT (serie storiche 2001–2021 normalizzate su basi territoriali aggiornate per il calcolo della Variazione Percentuale) e il Database Impianti FER — censimento nazionale degli impianti Eolici, Fotovoltaici e Idroelettrici (coordinate e potenza nominale in MW).

**Classificazione della dinamica demografica**
La variazione della popolazione è stata segmentata per isolare i cluster di crisi strutturale: alto spopolamento (var. < -20%), medio spopolamento (var. tra -5% e -20%), stabile o in crescita (var. > -5%, resilienza locale).

**Analisi geospaziale (GIS)**
Sistema di riferimento UTM Zona 32N (EPSG:32632) per misurazioni metriche planimetriche. Operazioni: estrazione dei centri geometrici comunali per l'analisi di prossimità; generazione di aree di influenza (isocrone di rispetto) a 5, 10 e 20 km dai siti produttivi; computo della potenza totale installata (MW) e densità di impianti per unità amministrativa.

**Analisi statistica e modellizzazione**
L'approccio statistico prevede l'applicazione di test di correlazione di Pearson per misurare il grado di dipendenza tra potenza installata e dinamiche demografiche, supportato da modelli di regressione per isolare l'effetto delle FER rispetto alle variabili geografiche latitudinali.

**Pipeline software**
Pipeline sviluppata interamente in R tramite l'integrazione dei pacchetti: `sf` (geospaziale), `tidyverse` (data manipulation), `leaflet` (visualizzazione interattiva) e `rmapshaper` (ottimizzazione topologica).

---

## 📊 Risultati principali

- Mappata la distribuzione spaziale degli impianti FER su tutto il territorio nazionale con calcolo della densità e potenza installata per comune
- Costruito l'indicatore di spopolamento per tutti i comuni italiani e classificati in tre categorie demografiche (2001–2021)
- Analizzata la correlazione tra presenza di impianti rinnovabili e dinamiche demografiche tramite test di Pearson e modelli di regressione
- Identificati cluster territoriali di comuni per tipologia demografica e dotazione FER
- Prodotte due dashboard interattive per l'esplorazione dei pattern territoriali e dei cluster statistici

---

## 🛠️ Tecnologie utilizzate

- **R** — elaborazione, analisi statistica e geospaziale
- **sf** — gestione dati geospaziali e operazioni GIS
- **tidyverse / dplyr** — data manipulation e pre-processing
- **leaflet** — visualizzazione interattiva delle mappe
- **rmapshaper** — ottimizzazione topologica dei layer
- **ISTAT** — serie storiche demografiche comunali 2001–2021
- **Database Impianti FER** — censimento nazionale impianti eolici, fotovoltaici e idroelettrici

---

## 📁 Struttura del progetto

```
Analisi_Spopolamento_Energia_Rinnovabile/
├── README.md
├── Script1_Importazione_dati_e_mappa.R
├── Script_2_Analisi_Statistica.R
├── Dashboard_Progetto_Rinnovabili_EsriGray.html
├── Dashboard_SBL_Cluster_EsriGray.html
└── Analisi_dello_spopolamento_in_rapporto_all_energia_rinnovabile.pdf
```

> ⚠️ I dati grezzi (shapefile comunali, dataset ISTAT, database FER) non sono inclusi nel repository per ragioni di dimensione. Le fonti sono indicate nella sezione Dati.

---

## 📂 Dati

| Fonte | Descrizione |
|-------|-------------|
| [ISTAT](https://www.istat.it) | Serie storiche demografiche comunali 2001–2021, shapefile confini comunali |
| [GSE](https://www.gse.it) | Database impianti FER — eolico, fotovoltaico, idroelettrico (coordinate e potenza MW) |

---

## 🔗 Link utili

- 🌐 [Sport Business Lab Consultancy](https://www.sblconsultancy.it/)
