# ============================================================
# IFRC GO Humanitarian Dashboard
# STQD6124 - Data Visualization & Communication
# Author: Mhamad Shhab Aldeen Hasan (P166175)
# ============================================================
# HOW TO RUN:
#   1. Place this file in the same folder as your three CSV files:
#      - 3_Emergency_Events_Data_all_.csv
#      - 4_Countries.csv
#      - 5_Appeals.csv
#   2. Install packages (run once in console):
#      install.packages(c("shiny","leaflet","plotly","dplyr","lubridate",
#                         "stringr","DT","scales","RColorBrewer",
#                         "sf","rnaturalearth","rnaturalearthdata"))
#   3. Click "Run App" in RStudio, or run: shiny::runApp("IFRC_Dashboard.R")
# ============================================================

library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(lubridate)
library(stringr)
library(DT)
library(scales)
library(RColorBrewer)
library(sf)
library(rnaturalearth)

# ─────────────────────────────────────────────
# 0. LOAD & CLEAN DATA
# ─────────────────────────────────────────────

clean_html <- function(x) {
  x <- gsub("<[^>]+>", " ", x)
  x <- gsub("&nbsp;",  " ", x)
  x <- gsub("&amp;",   "&", x)
  x <- gsub("&lt;",    "<", x)
  x <- gsub("&gt;",    ">", x)
  x <- gsub("&quot;",  '"', x)
  x <- str_squish(x)
  x
}

events_raw <- read.csv(
  "3_Emergency_Events_Data_all_.csv",
  stringsAsFactors = FALSE, na.strings = c("", "NA")
)
countries_raw <- read.csv(
  "4_Countries.csv",
  stringsAsFactors = FALSE, na.strings = c("", "NA")
)
appeals_raw <- read.csv(
  "5_Appeals.csv",
  stringsAsFactors = FALSE, na.strings = c("", "NA")
)

# ── Events ──────────────────────────────────
events <- events_raw %>%
  rename(
    country_name    = countries.name,
    society_name    = countries.society_name,
    iso3            = countries.iso3,
    dtype           = dtype.name,
    severity        = ifrc_severity_level_display,
    event_name      = name,
    start_date      = disaster_start_date
  ) %>%
  mutate(
    start_date  = as.Date(substr(start_date, 1, 10)),
    year        = year(start_date),
    summary_clean = clean_html(summary),
    severity    = factor(severity, levels = c("Yellow", "Orange", "Red"))
  ) %>%
  filter(!is.na(country_name), !is.na(start_date))

# ── Countries (lookup: iso3 → society_url) ──
country_lookup <- countries_raw %>%
  select(iso3, society_url) %>%
  filter(!is.na(iso3), iso3 != "")

# Join society_url into events
events <- events %>%
  left_join(country_lookup, by = "iso3")

# ── Appeals ──────────────────────────────────
# Note: appeals_raw has both 'status' (numeric code) and 'status_display' (text).
# We drop the raw 'status' column first to avoid a name conflict when renaming.
appeals <- appeals_raw %>%
  select(-status) %>%
  rename(
    appeal_name    = name,
    country_name   = country.name,
    society_name   = country.society_name,
    iso3           = country.iso3,
    region         = region.label,
    cluster        = sector,
    status         = status_display,
    funded         = amount_funded,
    requested      = amount_requested,
    dtype          = dtype.name,
    appeal_type    = atype_display,
    beneficiaries  = num_beneficiaries
  ) %>%
  mutate(
    funded    = as.numeric(funded),
    requested = as.numeric(requested),
    gap       = requested - funded,
    pct_funded = ifelse(requested > 0, funded / requested * 100, NA),
    cluster   = ifelse(is.na(cluster) | cluster == "",
                       "Unspecified", cluster),
    # Clean cluster label for display (they are IFRC country cluster admin names)
    start_date = as.Date(substr(start_date, 1, 10))
  ) %>%
  left_join(country_lookup, by = "iso3")

# ── Country centroids for map ─────────────────
# Using world.cities from maps package for lat/lon
country_coords <- maps::world.cities %>%
  filter(capital == 1) %>%
  group_by(country.etc) %>%
  slice(1) %>%
  ungroup() %>%
  select(country_name = country.etc, lat, lon = long)

# ── World country polygons for choropleth map ─
# Loaded once here (not inside server) since it never changes with filters.
world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso3 = iso_a3, name_long)

# Build event counts per country for choropleth
event_summary <- events %>%
  group_by(country_name, iso3) %>%
  summarise(
    event_count    = n(),
    red_count      = sum(severity == "Red",    na.rm = TRUE),
    orange_count   = sum(severity == "Orange", na.rm = TRUE),
    yellow_count   = sum(severity == "Yellow", na.rm = TRUE),
    society_name   = first(na.omit(society_name)),
    society_url    = first(na.omit(society_url)),
    .groups = "drop"
  ) %>%
  left_join(country_coords, by = "country_name")

# ── Severity colours (IFRC official) ─────────
sev_colors <- c(
  "Yellow" = "#F7C948",
  "Orange" = "#F47A20",
  "Red"    = "#E01B1B"
)

dtype_colors <- c(
  "Flood"             = "#0072BC",
  "Pluvial/Flash Flood" = "#0095D9",
  "Epidemic"          = "#009639",
  "Fire"              = "#FF6B35",
  "Cyclone"           = "#7B4F9E",
  "Population Movement" = "#E8A838",
  "Earthquake"        = "#B5582C",
  "Complex Emergency" = "#CC0044",
  "Civil Unrest"      = "#E8384F",
  "Drought"           = "#D4A017",
  "Landslide"         = "#8B6F47",
  "Other"             = "#888888"
)

# ─────────────────────────────────────────────
# 1. UI
# ─────────────────────────────────────────────

ui <- fluidPage(
  
  tags$head(
    tags$title("IFRC Humanitarian Dashboard"),
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&display=swap"
    ),
    tags$style(HTML("

      /* ── Base ── */
      * { box-sizing: border-box; }
      body {
        font-family: 'Inter', sans-serif;
        background: #F5F6F8;
        color: #1a1a2e;
        margin: 0; padding: 0;
        font-size: 14px;
      }

      /* ── Top bar ── */
      .top-bar {
        background: #CC0033;
        color: white;
        padding: 12px 24px;
        display: flex;
        align-items: center;
        gap: 14px;
        position: sticky;
        top: 0;
        z-index: 1000;
        box-shadow: 0 2px 8px rgba(0,0,0,0.18);
      }
      .top-bar img { height: 32px; }
      .top-bar h1 {
        font-size: 17px;
        font-weight: 600;
        margin: 0;
        letter-spacing: 0.01em;
      }
      .top-bar .sub {
        font-size: 12px;
        opacity: 0.85;
        margin-left: auto;
      }

      /* ── Stat bar ── */
      .stat-bar {
        background: white;
        border-bottom: 1px solid #E5E7EB;
        padding: 10px 24px;
        display: flex;
        gap: 32px;
        align-items: center;
      }
      .stat-item { text-align: center; }
      .stat-val {
        font-size: 20px;
        font-weight: 600;
        color: #CC0033;
        line-height: 1.2;
      }
      .stat-lbl {
        font-size: 11px;
        color: #6B7280;
        text-transform: uppercase;
        letter-spacing: 0.05em;
      }
      .stat-div {
        width: 1px;
        height: 36px;
        background: #E5E7EB;
      }

      /* ── Tab nav ── */
      .nav-tabs { border-bottom: 2px solid #E5E7EB; background: white; padding: 0 24px; }
      .nav-tabs > li > a {
        color: #6B7280 !important;
        font-weight: 500;
        font-size: 13px;
        border: none !important;
        border-bottom: 2px solid transparent !important;
        margin-bottom: -2px;
        padding: 12px 16px;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li > a:hover {
        color: #CC0033 !important;
        border-bottom-color: #CC0033 !important;
        background: transparent !important;
      }
      .tab-content { padding: 20px 24px; }

      /* ── Filter sidebar ── */
      .filter-panel {
        background: white;
        border-radius: 10px;
        padding: 16px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.07);
        margin-bottom: 16px;
      }
      .filter-panel h4 {
        font-size: 12px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.06em;
        color: #6B7280;
        margin: 0 0 12px;
      }
      .selectize-input, select.form-control {
        border-radius: 6px !important;
        border: 1px solid #D1D5DB !important;
        font-size: 13px !important;
      }
      .form-group label {
        font-size: 12px;
        font-weight: 500;
        color: #374151;
      }
      .btn-reset {
        background: #F3F4F6;
        border: 1px solid #D1D5DB;
        color: #374151;
        border-radius: 6px;
        font-size: 12px;
        font-weight: 500;
        padding: 6px 14px;
        width: 100%;
        cursor: pointer;
      }
      .btn-reset:hover { background: #E5E7EB; }

      /* ── Cards ── */
      .card {
        background: white;
        border-radius: 10px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.07);
        overflow: hidden;
      }
      .card-header {
        padding: 12px 16px;
        border-bottom: 1px solid #F3F4F6;
        font-size: 13px;
        font-weight: 600;
        color: #374151;
        display: flex;
        align-items: center;
        justify-content: space-between;
      }
      .card-body { padding: 16px; }

      /* ── Event detail panel ── */
      .detail-panel {
        background: white;
        border-radius: 10px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.07);
        height: 460px;
        overflow-y: auto;
        padding: 0;
      }
      .detail-header {
        background: #CC0033;
        color: white;
        padding: 14px 16px;
        font-size: 14px;
        font-weight: 600;
        border-radius: 10px 10px 0 0;
      }
      .detail-body { padding: 14px 16px; }
      .detail-row {
        display: flex;
        gap: 8px;
        margin-bottom: 10px;
        font-size: 13px;
        align-items: flex-start;
      }
      .detail-lbl {
        min-width: 80px;
        color: #6B7280;
        font-weight: 500;
        font-size: 12px;
        padding-top: 1px;
      }
      .detail-val { color: #1a1a2e; line-height: 1.5; }
      .sev-badge {
        display: inline-block;
        padding: 2px 10px;
        border-radius: 20px;
        font-size: 11px;
        font-weight: 600;
      }
      .sev-Yellow { background: #FEF3C7; color: #92400E; }
      .sev-Orange { background: #FED7AA; color: #9A3412; }
      .sev-Red    { background: #FEE2E2; color: #991B1B; }
      .summary-box {
        background: #F9FAFB;
        border-left: 3px solid #CC0033;
        border-radius: 0 6px 6px 0;
        padding: 10px 12px;
        font-size: 12.5px;
        line-height: 1.7;
        color: #374151;
        max-height: 180px;
        overflow-y: auto;
        margin-top: 4px;
      }
      .society-link {
        display: inline-block;
        margin-top: 4px;
        color: #CC0033;
        font-size: 12px;
        font-weight: 500;
        text-decoration: none;
      }
      .society-link:hover { text-decoration: underline; }
      .no-select {
        text-align: center;
        padding: 60px 20px;
        color: #9CA3AF;
        font-size: 13px;
      }
      .no-select .icon { font-size: 32px; margin-bottom: 8px; }

      /* ── Event list ── */
      .event-list { max-height: 460px; overflow-y: auto; }
      .event-item {
        padding: 10px 14px;
        border-bottom: 1px solid #F3F4F6;
        cursor: pointer;
        transition: background 0.12s;
      }
      .event-item:hover { background: #FFF5F5; }
      .event-item.selected { background: #FFF0F0; border-left: 3px solid #CC0033; }
      .event-item-title {
        font-size: 12.5px;
        font-weight: 500;
        color: #1a1a2e;
        margin-bottom: 3px;
        line-height: 1.4;
      }
      .event-item-meta {
        font-size: 11px;
        color: #9CA3AF;
        display: flex;
        gap: 8px;
        align-items: center;
      }
      .dot { width: 8px; height: 8px; border-radius: 50%; display: inline-block; }

      /* ── Map ── */
      #events_map { border-radius: 10px; box-shadow: 0 1px 4px rgba(0,0,0,0.07); }

      /* ── Funding tab ── */
      .funding-metric {
        background: white;
        border-radius: 10px;
        padding: 14px 18px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.07);
        text-align: center;
      }
      .funding-metric .val {
        font-size: 22px;
        font-weight: 700;
        line-height: 1.2;
      }
      .funding-metric .lbl {
        font-size: 11px;
        color: #6B7280;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        margin-top: 4px;
      }
      .val-req  { color: #374151; }
      .val-fund { color: #059669; }
      .val-gap  { color: #CC0033; }
      .val-pct  { color: #F97316; }

      /* ── Status badge ── */
      .badge-Active { background:#D1FAE5; color:#065F46; padding:2px 8px;
                      border-radius:12px; font-size:11px; font-weight:600; }
      .badge-Closed { background:#FEF3C7; color:#92400E; padding:2px 8px;
                      border-radius:12px; font-size:11px; font-weight:600; }
      .badge-Frozen { background:#E5E7EB; color:#6B7280; padding:2px 8px;
                      border-radius:12px; font-size:11px; font-weight:600; }

      /* ── DataTable overrides ── */
      .dataTables_wrapper { font-size: 12.5px; }
      table.dataTable thead th {
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        color: #6B7280 !important;
        font-weight: 600;
        border-bottom: 2px solid #E5E7EB !important;
      }
      .dataTables_filter input {
        border: 1px solid #D1D5DB !important;
        border-radius: 6px !important;
        font-size: 12px !important;
      }

      /* Scrollbar styling */
      ::-webkit-scrollbar { width: 5px; height: 5px; }
      ::-webkit-scrollbar-track { background: #F3F4F6; }
      ::-webkit-scrollbar-thumb { background: #D1D5DB; border-radius: 3px; }

    "))
  ),
  
  # ── Top bar ──────────────────────────────
  div(class = "top-bar",
      tags$img(
        src = "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a7/IFRC_logo.svg/200px-IFRC_logo.svg.png"
      ),
      h1("IFRC Global Humanitarian Dashboard"),
      div(class = "sub", "IFRC GO Data · 2023–2026 · STQD6124")
  ),
  
  # ── Stat bar ──────────────────────────────
  div(class = "stat-bar",
      div(class = "stat-item",
          div(class = "stat-val", "1,000"),
          div(class = "stat-lbl", "Events")
      ),
      div(class = "stat-div"),
      div(class = "stat-item",
          div(class = "stat-val", textOutput("stat_countries", inline = TRUE)),
          div(class = "stat-lbl", "Countries")
      ),
      div(class = "stat-div"),
      div(class = "stat-item",
          div(class = "stat-val", "$4.34B"),
          div(class = "stat-lbl", "Aid Requested")
      ),
      div(class = "stat-div"),
      div(class = "stat-item",
          div(class = "stat-val", "$1.99B"),
          div(class = "stat-lbl", "Aid Funded")
      ),
      div(class = "stat-div"),
      div(class = "stat-item",
          div(class = "stat-val", style = "color:#CC0033;", "$2.35B"),
          div(class = "stat-lbl", "Funding Gap")
      ),
      div(class = "stat-div"),
      div(class = "stat-item",
          div(class = "stat-val", "6"),
          div(class = "stat-lbl", "Red Alerts")
      )
  ),
  
  # ── Tabs ──────────────────────────────────
  navbarPage(
    title = NULL,
    windowTitle = "IFRC Dashboard",
    id = "main_tabs",
    
    # ══════════════════════════════════════
    # TAB 1: Events Map
    # ══════════════════════════════════════
    tabPanel("Events Map",
             
             fluidRow(
               
               # Left: Filters
               column(3,
                      div(class = "filter-panel",
                          h4("Filter Events"),
                          selectInput("f_region", "Region",
                                      choices = c("All regions" = "",
                                                  "Africa", "Americas", "Asia Pacific",
                                                  "Europe", "Middle East & North Africa"),
                                      selected = ""
                          ),
                          selectInput("f_country", "Country",
                                      choices  = c("All countries" = "", sort(unique(events$country_name))),
                                      selected = ""
                          ),
                          selectInput("f_dtype", "Disaster type",
                                      choices  = c("All types" = "", sort(unique(events$dtype))),
                                      selected = ""
                          ),
                          selectInput("f_severity", "Severity",
                                      choices  = c("All levels" = "", "Red", "Orange", "Yellow"),
                                      selected = ""
                          ),
                          sliderInput("f_year", "Year",
                                      min = 2023, max = 2026, value = c(2023, 2026),
                                      step = 1, sep = ""
                          ),
                          actionButton("btn_reset", "Reset filters", class = "btn-reset")
                      ),
                      
                      # Event list
                      div(class = "card",
                          div(class = "card-header",
                              "Matching events",
                              span(style = "font-size:12px;color:#9CA3AF;font-weight:400;",
                                   textOutput("event_count", inline = TRUE))
                          ),
                          div(class = "event-list", uiOutput("event_list_ui"))
                      )
               ),
               
               # Right: Map (now spans the old middle + right columns), description below it
               column(9,
                      div(style = "margin-bottom:16px;",
                          leafletOutput("events_map", height = "460px")
                      ),
                      div(class = "detail-panel", style = "height:auto; max-height:400px;",
                          uiOutput("event_detail_ui")
                      )
               )
             )
    ),
    
    # ══════════════════════════════════════
    # TAB 2: Funding & Appeals
    # ══════════════════════════════════════
    tabPanel("Funding & Appeals",
             
             # Funding metric cards
             fluidRow(
               column(3,
                      div(class = "funding-metric",
                          div(class = "val val-req",
                              textOutput("f_total_req", inline = TRUE)),
                          div(class = "lbl", "Total requested")
                      )
               ),
               column(3,
                      div(class = "funding-metric",
                          div(class = "val val-fund",
                              textOutput("f_total_fund", inline = TRUE)),
                          div(class = "lbl", "Total funded")
                      )
               ),
               column(3,
                      div(class = "funding-metric",
                          div(class = "val val-gap",
                              textOutput("f_total_gap", inline = TRUE)),
                          div(class = "lbl", "Funding gap")
                      )
               ),
               column(3,
                      div(class = "funding-metric",
                          div(class = "val val-pct",
                              textOutput("f_pct", inline = TRUE)),
                          div(class = "lbl", "Average funded")
                      )
               )
             ),
             
             br(),
             
             fluidRow(
               # Filters
               column(3,
                      div(class = "filter-panel",
                          h4("Filter Appeals"),
                          selectInput("a_region", "Region",
                                      choices = c("All regions" = "",
                                                  sort(unique(appeals$region[!is.na(appeals$region)]))),
                                      selected = ""
                          ),
                          selectInput("a_country", "Country",
                                      choices = c("All countries" = "",
                                                  sort(unique(appeals$country_name[!is.na(appeals$country_name)]))),
                                      selected = ""
                          ),
                          selectInput("a_dtype", "Disaster type",
                                      choices = c("All types" = "",
                                                  sort(unique(appeals$dtype[!is.na(appeals$dtype)]))),
                                      selected = ""
                          ),
                          selectInput("a_status", "Appeal status",
                                      choices = c("All statuses" = "", "Active", "Closed", "Frozen"),
                                      selected = ""
                          ),
                          selectInput("a_atype", "Appeal type",
                                      choices = c("All types" = "", "DREF", "Emergency Appeal",
                                                  "Forecast Based Action"),
                                      selected = ""
                          ),
                          # Status explainer
                          div(style = "margin-top:12px; font-size:11.5px; color:#6B7280; line-height:1.8;",
                              tags$b("Status guide:"), br(),
                              tags$span(class = "badge-Active", "Active"), " — appeal open, receiving funds", br(),
                              tags$span(class = "badge-Closed", "Closed"), " — appeal ended, funds finalized", br(),
                              tags$span(class = "badge-Frozen", "Frozen"), " — paused (access/security)"
                          )
                      )
               ),
               
               column(9,
                      # Chart: requested vs funded by region
                      div(class = "card", style = "margin-bottom:16px;",
                          div(class = "card-header", "Funding requested vs funded by region"),
                          div(class = "card-body",
                              plotlyOutput("funding_region_chart", height = "260px")
                          )
                      ),
                      
                      # Appeals table
                      div(class = "card",
                          div(class = "card-header",
                              "Appeal details",
                              span(style = "font-size:12px;color:#9CA3AF;font-weight:400;",
                                   textOutput("appeals_count", inline = TRUE))
                          ),
                          div(class = "card-body", style = "padding:0;",
                              DTOutput("appeals_table")
                          )
                      )
               )
             )
    ),
    
    # ══════════════════════════════════════
    # TAB 3: Disaster Trends
    # ══════════════════════════════════════
    tabPanel("Disaster Trends",
             fluidRow(
               column(6,
                      div(class = "card", style = "margin-bottom:16px;",
                          div(class = "card-header", "Disaster type frequency"),
                          div(class = "card-body",
                              plotlyOutput("dtype_bar", height = "320px")
                          )
                      )
               ),
               column(6,
                      div(class = "card", style = "margin-bottom:16px;",
                          div(class = "card-header", "Events per year by type"),
                          div(class = "card-body",
                              plotlyOutput("trend_line", height = "320px")
                          )
                      )
               )
             ),
             fluidRow(
               column(6,
                      div(class = "card",
                          div(class = "card-header", "Severity distribution"),
                          div(class = "card-body",
                              plotlyOutput("severity_donut", height = "300px")
                          )
                      )
               ),
               column(6,
                      div(class = "card",
                          div(class = "card-header", "Top 15 most affected countries (by event count)"),
                          div(class = "card-body",
                              plotlyOutput("top_countries_bar", height = "300px")
                          )
                      )
               )
             )
    )
  )
)

# ─────────────────────────────────────────────
# 2. SERVER
# ─────────────────────────────────────────────

server <- function(input, output, session) {
  
  # ── Selected event (click from list or map) ──
  selected_event <- reactiveVal(NULL)
  
  # ── Filtered events ───────────────────────────
  filtered_events <- reactive({
    df <- events
    
    # Region filter (via appeals region data mapped to country)
    # We use continent-level mapping based on country
    if (!is.null(input$f_region) && input$f_region != "") {
      # Map IFRC region labels to country names via appeals table
      region_countries <- appeals %>%
        filter(region == input$f_region) %>%
        pull(country_name) %>%
        unique()
      df <- df %>% filter(country_name %in% region_countries)
    }
    
    if (!is.null(input$f_country) && input$f_country != "") {
      df <- df %>% filter(country_name == input$f_country)
    }
    if (!is.null(input$f_dtype) && input$f_dtype != "") {
      df <- df %>% filter(dtype == input$f_dtype)
    }
    if (!is.null(input$f_severity) && input$f_severity != "") {
      df <- df %>% filter(as.character(severity) == input$f_severity)
    }
    df <- df %>%
      filter(year >= input$f_year[1], year <= input$f_year[2])
    df
  })
  
  # ── Filtered appeals ──────────────────────────
  filtered_appeals <- reactive({
    df <- appeals
    if (!is.null(input$a_region)  && input$a_region  != "") df <- df %>% filter(region       == input$a_region)
    if (!is.null(input$a_country) && input$a_country != "") df <- df %>% filter(country_name == input$a_country)
    if (!is.null(input$a_dtype)   && input$a_dtype   != "") df <- df %>% filter(dtype        == input$a_dtype)
    if (!is.null(input$a_status)  && input$a_status  != "") df <- df %>% filter(status       == input$a_status)
    if (!is.null(input$a_atype)   && input$a_atype   != "") df <- df %>% filter(appeal_type  == input$a_atype)
    df
  })
  
  # ── Reset button ──────────────────────────────
  observeEvent(input$btn_reset, {
    updateSelectInput(session, "f_region",   selected = "")
    updateSelectInput(session, "f_country",  selected = "")
    updateSelectInput(session, "f_dtype",    selected = "")
    updateSelectInput(session, "f_severity", selected = "")
    updateSliderInput(session, "f_year", value = c(2023, 2026))
    selected_event(NULL)
  })
  
  # ── Stat bar ──────────────────────────────────
  output$stat_countries <- renderText({
    length(unique(events$country_name))
  })
  
  # ── Event count label ─────────────────────────
  output$event_count <- renderText({
    paste0(nrow(filtered_events()), " events")
  })
  
  # ── Event list (left panel) ───────────────────
  output$event_list_ui <- renderUI({
    df <- filtered_events() %>%
      arrange(desc(start_date)) %>%
      head(80)
    
    if (nrow(df) == 0) {
      return(div(class = "no-select",
                 div(class = "icon", "📭"),
                 "No events match your filters"
      ))
    }
    
    sel_id <- selected_event()
    
    lapply(seq_len(nrow(df)), function(i) {
      row  <- df[i, ]
      sev  <- as.character(row$severity)
      col  <- sev_colors[sev]
      if (is.na(col)) col <- "#888"
      is_sel <- !is.null(sel_id) && !is.na(row$event_name) &&
        row$event_name == sel_id
      
      div(
        class = paste0("event-item", if (is_sel) " selected" else ""),
        onclick = sprintf(
          "Shiny.setInputValue('clicked_event', '%s', {priority: 'event'})",
          gsub("'", "\\\\'", row$event_name)
        ),
        div(class = "event-item-title", row$event_name),
        div(class = "event-item-meta",
            span(class = "dot",
                 style = paste0("background:", col, ";")),
            sev,
            "·",
            row$dtype,
            "·",
            format(row$start_date, "%d %b %Y")
        )
      )
    })
  })
  
  # ── Click event from list ─────────────────────
  observeEvent(input$clicked_event, {
    selected_event(input$clicked_event)
  })
  
  # ── Click event from map ──────────────────────
  observeEvent(input$events_map_shape_click, {
    click <- input$events_map_shape_click
    if (!is.null(click)) {
      # click$id is the country's display name (set via layerId in addPolygons)
      df <- filtered_events()
      sel <- df %>% filter(country_name == click$id)
      if (nrow(sel) > 0) selected_event(sel$event_name[1])
    }
  })
  
  # ── Event detail panel ────────────────────────
  output$event_detail_ui <- renderUI({
    ev_name <- selected_event()
    
    if (is.null(ev_name)) {
      return(div(
        div(class = "detail-header", "Event details"),
        div(class = "no-select",
            div(class = "icon", "👆"),
            "Click an event from the list or map marker to see details"
        )
      ))
    }
    
    row <- filtered_events() %>%
      filter(event_name == ev_name) %>%
      slice(1)
    
    if (nrow(row) == 0) {
      row <- events %>% filter(event_name == ev_name) %>% slice(1)
    }
    
    if (nrow(row) == 0) {
      return(div(class = "detail-header", "Event not found"))
    }
    
    sev  <- as.character(row$severity[1])
    url  <- row$society_url[1]
    soc  <- row$society_name[1]
    
    div(
      div(class = "detail-header", row$event_name[1]),
      div(class = "detail-body",
          
          div(class = "detail-row",
              div(class = "detail-lbl", "Country"),
              div(class = "detail-val", row$country_name[1])
          ),
          
          div(class = "detail-row",
              div(class = "detail-lbl", "Date"),
              div(class = "detail-val",
                  format(row$start_date[1], "%d %B %Y"))
          ),
          
          div(class = "detail-row",
              div(class = "detail-lbl", "Type"),
              div(class = "detail-val", row$dtype[1])
          ),
          
          div(class = "detail-row",
              div(class = "detail-lbl", "Severity"),
              div(class = "detail-val",
                  span(class = paste0("sev-badge sev-", sev), sev)
              )
          ),
          
          div(class = "detail-row",
              div(class = "detail-lbl", "National Society"),
              div(class = "detail-val",
                  if (!is.na(soc)) soc else "—",
                  if (!is.na(url) && url != "") {
                    tags$a(
                      href   = url,
                      target = "_blank",
                      class  = "society-link",
                      paste0("Visit ", if (!is.na(soc)) soc else "website", " →")
                    )
                  }
              )
          ),
          
          div(class = "detail-row",
              div(class = "detail-lbl", "Summary")
          ),
          if (!is.na(row$summary_clean[1]) && row$summary_clean[1] != "") {
            div(class = "summary-box", row$summary_clean[1])
          } else {
            div(class = "summary-box",
                style = "color:#9CA3AF;font-style:italic;",
                "No summary available for this event.")
          }
      )
    )
  })
  
  # ── Map ───────────────────────────────────────
  output$events_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      setView(lng = 20, lat = 15, zoom = 2)
  })
  
  observe({
    df <- filtered_events()
    
    # Build per-country aggregates for map
    map_data <- df %>%
      group_by(country_name, iso3) %>%
      summarise(
        event_count  = n(),
        society_name = first(na.omit(society_name)),
        society_url  = first(na.omit(society_url)),
        top_dtype    = names(sort(table(dtype), decreasing = TRUE))[1],
        sev_worst    = ifelse(any(severity == "Red",    na.rm = TRUE), "Red",
                              ifelse(any(severity == "Orange", na.rm = TRUE), "Orange",
                                     "Yellow")),
        .groups = "drop"
      ) %>%
      filter(!is.na(iso3), iso3 != "")
    
    # Join onto ALL world country polygons (not just matched ones) so every
    # country is shown and highlighted, same as the storyboard's map —
    # countries with zero matching events simply fall back to light gray.
    map_sf <- world_sf %>%
      left_join(map_data, by = "iso3")
    
    pal <- colorNumeric(
      palette  = c("#F2F2F2", "#CC0033"),
      domain   = c(0, max(map_data$event_count, 1)),
      na.color = "#F2F2F2"
    )
    map_sf$fill_color <- ifelse(is.na(map_sf$event_count), "#F2F2F2",
                                pal(map_sf$event_count))
    
    map_sf$display_name <- ifelse(is.na(map_sf$country_name),
                                  map_sf$name_long, map_sf$country_name)
    map_sf$display_count <- ifelse(is.na(map_sf$event_count), 0, map_sf$event_count)
    
    # Popup content
    map_sf$popup <- paste0(
      "<b>", map_sf$display_name, "</b><br>",
      "<span style='color:#6B7280;font-size:12px;'>",
      map_sf$display_count, " event(s)",
      ifelse(!is.na(map_sf$top_dtype), paste0(" · ", map_sf$top_dtype), ""),
      "</span><br>",
      ifelse(!is.na(map_sf$society_name),
             paste0("<span style='font-size:12px;'>", map_sf$society_name, "</span>"), ""),
      ifelse(!is.na(map_sf$society_url) & map_sf$society_url != "",
             paste0("<br><a href='", map_sf$society_url,
                    "' target='_blank' style='color:#CC0033;font-size:12px;'>",
                    "Visit National Society →</a>"),
             "")
    )
    
    leafletProxy("events_map") %>%
      clearShapes() %>%
      addPolygons(
        data        = map_sf,
        fillColor   = ~fill_color,
        fillOpacity = 0.85,
        color       = "white",
        weight      = 0.6,
        popup       = ~popup,
        layerId     = ~display_name,
        label       = ~paste0(display_name, ": ", display_count, " events"),
        highlightOptions = highlightOptions(
          weight = 2, color = "#CC0033", bringToFront = TRUE
        )
      )
  })
  
  # ── Funding metrics ───────────────────────────
  output$f_total_req  <- renderText({
    paste0("$", round(sum(filtered_appeals()$requested, na.rm = TRUE) / 1e6, 1), "M")
  })
  output$f_total_fund <- renderText({
    paste0("$", round(sum(filtered_appeals()$funded, na.rm = TRUE) / 1e6, 1), "M")
  })
  output$f_total_gap  <- renderText({
    paste0("$", round(sum(filtered_appeals()$gap, na.rm = TRUE) / 1e6, 1), "M")
  })
  output$f_pct <- renderText({
    df  <- filtered_appeals()
    req <- sum(df$requested, na.rm = TRUE)
    fun <- sum(df$funded,    na.rm = TRUE)
    if (req > 0) paste0(round(fun / req * 100, 1), "%") else "—"
  })
  
  output$appeals_count <- renderText({
    paste0(nrow(filtered_appeals()), " appeals")
  })
  
  # ── Funding by region chart ───────────────────
  output$funding_region_chart <- renderPlotly({
    df <- filtered_appeals() %>%
      group_by(region) %>%
      summarise(
        Requested = sum(requested, na.rm = TRUE) / 1e6,
        Funded    = sum(funded,    na.rm = TRUE) / 1e6,
        .groups   = "drop"
      ) %>%
      filter(!is.na(region)) %>%
      arrange(desc(Requested))
    
    plot_ly(df, y = ~region, x = ~Requested,
            name = "Requested", type = "bar",
            orientation = "h",
            marker = list(color = "#E5E7EB")) %>%
      add_trace(x = ~Funded, name = "Funded",
                marker = list(color = "#059669")) %>%
      layout(
        barmode = "overlay",
        xaxis   = list(title = "USD (millions)", tickprefix = "$",
                       gridcolor = "#F3F4F6"),
        yaxis   = list(title = "", autorange = "reversed"),
        legend  = list(orientation = "h", y = -0.2),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font    = list(family = "Inter", size = 12),
        margin  = list(l = 10, r = 20, t = 10, b = 40)
      )
  })
  
  # ── Appeals table ─────────────────────────────
  output$appeals_table <- renderDT({
    df <- filtered_appeals() %>%
      mutate(
        Funded     = paste0("$", formatC(funded,    format = "f", digits = 0,
                                         big.mark = ",")),
        Requested  = paste0("$", formatC(requested, format = "f", digits = 0,
                                         big.mark = ",")),
        Gap        = paste0("$", formatC(gap,        format = "f", digits = 0,
                                         big.mark = ",")),
        Pct_Funded = ifelse(!is.na(pct_funded),
                            paste0(round(pct_funded, 1), "%"), "—"),
        Status_Badge = paste0('<span class="badge-', status, '">', status, '</span>'),
        Society_Link = ifelse(!is.na(society_url) & society_url != "",
                              paste0('<a href="', society_url,
                                     '" target="_blank">🔗</a>'), ""),
        IFRC_Cluster = str_trunc(cluster, 45)
      ) %>%
      select(
        `Disaster`      = appeal_name,
        `Country`       = country_name,
        `Society`       = society_name,
        `Region`        = region,
        `Type`          = dtype,
        `Appeal type`   = appeal_type,
        `Status`        = Status_Badge,
        `Requested`     = Requested,
        `Funded`        = Funded,
        `Gap`           = Gap,
        `% Funded`      = Pct_Funded,
        `IFRC Cluster`  = IFRC_Cluster,
        `Website`       = Society_Link
      )
    
    datatable(
      df,
      escape    = FALSE,
      selection = "single",
      rownames  = FALSE,
      options   = list(
        pageLength  = 10,
        scrollX     = TRUE,
        dom         = "frtip",
        columnDefs  = list(
          list(width = "220px", targets = 0),
          list(width = "120px", targets = 1),
          list(width = "60px",  targets = 12)
        )
      )
    )
  })
  
  # ── Trends: dtype bar ─────────────────────────
  output$dtype_bar <- renderPlotly({
    df <- events %>%
      count(dtype) %>%
      arrange(n) %>%
      mutate(color = ifelse(dtype %in% names(dtype_colors),
                            dtype_colors[dtype], "#888888"))
    
    plot_ly(df, x = ~n, y = ~reorder(dtype, n),
            type = "bar", orientation = "h",
            marker = list(color = ~color)) %>%
      layout(
        xaxis = list(title = "Number of events", gridcolor = "#F3F4F6"),
        yaxis = list(title = ""),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font   = list(family = "Inter", size = 12),
        margin = list(l = 10, r = 20, t = 10, b = 40),
        showlegend = FALSE
      )
  })
  
  # ── Trends: line chart by year ────────────────
  output$trend_line <- renderPlotly({
    df <- events %>%
      count(year, dtype) %>%
      filter(dtype %in% names(dtype_colors))
    
    plot_ly(df, x = ~year, y = ~n, color = ~dtype,
            colors = dtype_colors,
            type = "scatter", mode = "lines+markers",
            line = list(width = 2)) %>%
      layout(
        xaxis = list(title = "Year", dtick = 1, gridcolor = "#F3F4F6"),
        yaxis = list(title = "Events", gridcolor = "#F3F4F6"),
        legend = list(orientation = "v", font = list(size = 11)),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font   = list(family = "Inter", size = 12),
        margin = list(l = 10, r = 10, t = 10, b = 40)
      )
  })
  
  # ── Trends: severity donut ────────────────────
  output$severity_donut <- renderPlotly({
    df <- events %>% count(severity) %>% filter(!is.na(severity))
    
    plot_ly(df,
            labels  = ~severity,
            values  = ~n,
            type    = "pie",
            hole    = 0.55,
            marker  = list(colors = c("#F7C948","#F47A20","#E01B1B"),
                           line   = list(color = "white", width = 2))) %>%
      layout(
        showlegend    = TRUE,
        legend        = list(orientation = "h", y = -0.1),
        paper_bgcolor = "rgba(0,0,0,0)",
        font   = list(family = "Inter", size = 12),
        margin = list(l = 10, r = 10, t = 10, b = 40)
      )
  })
  
  # ── Trends: top countries bar ─────────────────
  output$top_countries_bar <- renderPlotly({
    df <- events %>%
      count(country_name) %>%
      arrange(desc(n)) %>%
      head(15)
    
    plot_ly(df,
            x = ~n,
            y = ~reorder(country_name, n),
            type = "bar",
            orientation = "h",
            marker = list(color = "#CC0033", opacity = 0.8)) %>%
      layout(
        xaxis = list(title = "Events", gridcolor = "#F3F4F6"),
        yaxis = list(title = ""),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font   = list(family = "Inter", size = 12),
        margin = list(l = 10, r = 20, t = 10, b = 40),
        showlegend = FALSE
      )
  })
}

# ─────────────────────────────────────────────
# 3. RUN
# ─────────────────────────────────────────────

shinyApp(ui = ui, server = server)
