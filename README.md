# Humanitarian Horizons: IFRC Disaster Response Analytics

Two complementary data visualization projects built on real IFRC GO Platform data: an **R Markdown storyboard** (a fixed, narrated story) and an **R Shiny dashboard** (a free-form interactive explorer), for STQD6124 Data Visualization & Communication, MSc Data Science & Analytics, Universiti Kebangsaan Malaysia (UKM).

**Live storyboard:** https://rpubs.com/Shhab94/1446417

---

## Repository structure

```
.
├── README.md
├── storyboard/
│   └── IFRC_Disaster_Data_Storyboard.Rmd      # flexdashboard storyboard (fixed narrative, 10 frames)
├── shiny-dashboard/
│   └── IFRC_Dashboard.R                       # R Shiny app (interactive explorer, 3 tabs)
├── docs/
│   ├── IFRC_Dashboard_Report.pdf              # dashboard design & walkthrough report
│   └── IFRC_Dashboard_Report.docx             # same report, editable
└── data/
    ├── 3_Emergency_Events_Data_all_.csv
    ├── 4_Countries.csv
    └── 5_Appeals.csv
```

Both projects read from the same `data/` folder, so there is no duplication needed. If your folder names differ from the above when you upload, just update the relative paths at the top of each `.Rmd`/`.R` file (`read_csv("data/...")` / `read.csv("...")`) to match.

## What's in this repo

### 1. Storyboard (`storyboard/`)
A 10-frame `flexdashboard::flex_dashboard` storyboard that tells one fixed, narrated story: global disaster trends, regional vulnerability, funding gaps, and an interactive world map, closing with a synthesis of five key findings. Built with a "journal" Bootswatch theme, a custom IFRC-red navbar, and a mix of column, row, and tabset layouts across frames.

**Live version:** https://rpubs.com/Shhab94/1446417

### 2. Shiny Dashboard (`shiny-dashboard/`)
A 3-tab interactive R Shiny application for open-ended exploration of the same underlying data:
- **Events Map**: filterable choropleth map of logged disaster events, with a click-through event detail panel
- **Funding & Appeals**: funding requested vs. funded by region, plus a full searchable appeal-level table
- **Disaster Trends**: disaster type frequency, year-by-year trends, severity distribution, and top affected countries

A full design and walkthrough report for the dashboard (with screenshots of every tab) is in `docs/`.

### 3. Data (`data/`)
All three files are original, unedited extracts from the [IFRC GO Platform](https://go.ifrc.org):

| File | Records | Description |
|---|---|---|
| `3_Emergency_Events_Data_all_.csv` | 1,000 | Field-reported disaster events |
| `4_Countries.csv` | 253 country profiles | National Society & region reference data |
| `5_Appeals.csv` | 1,000 | IFRC funding appeals |

## Data-quality notes (for defense / Q&A)

Both projects follow the same no-fabrication rule: every chart uses real fields, and any data limitation is disclosed rather than papered over.

- **`num_affected`** (Emergency Events) is populated for only 3.5% of records, too sparse to chart reliably. The storyboard uses `num_beneficiaries` (Appeals) instead, which is 100% complete.
- **`inform_score`** (Countries) exists as a column but is **0% populated** in this extract, a verified finding, not an assumption. Both projects use appeal/event frequency as a transparent, disclosed proxy for risk concentration instead.
- Country/region joins use the platform's own numeric IDs (`countries.id`, `country.id` → `id`) rather than matching on free-text country names, avoiding the classic bug where spelling mismatches silently drop countries from a map.

## How to run

### Storyboard
```r
install.packages(c("flexdashboard", "dplyr", "tidyr", "ggplot2",
                    "readr", "scales", "lubridate", "forcats", "plotly"))
```
Open `storyboard/IFRC_Disaster_Data_Storyboard.Rmd` in RStudio and click **Knit**.

### Shiny dashboard
```r
install.packages(c("shiny", "leaflet", "plotly", "dplyr", "lubridate",
                    "stringr", "DT", "scales", "RColorBrewer",
                    "sf", "rnaturalearth", "rnaturalearthdata"))
```
Open `shiny-dashboard/IFRC_Dashboard.R` in RStudio and click **Run App**.

## Author

Mhamad Shhab Aldeen Hasan (P166175)
MSc Data Science & Analytics, UKM
STQD6124 Data Visualization & Communication, July 2026
