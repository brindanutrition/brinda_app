dashboardBody(
  tags$style("body {
      -moz-transform: scale(1.1, 1.1); /* Moz-browsers */
      zoom: 1.1; /* Other non-webkit browsers */
      zoom: 110%; /* Webkit browsers */
    }"
  ),
  use_theme(source("ui/styles/color_theme.R", local = TRUE)$mytheme),
  tabItems(
    # --- Import Data UI -----------------------------------------------------
    tabItem(
      tabName = "DataImportTab",
      source(
        file = "ui/components/tab_dataimport.R",
        local = TRUE,
        encoding = "UTF-8"
      )$value
    ),
    # --- Select Biomarkers UI -----------------------------------------------
    tabItem(
      tabName = "BiomarkerTab",
      source(
        file = "ui/components/tab_biomarker.R",
        local = TRUE,
        encoding = "UTF-8"
      )$value
    ),
    # --- Select Cutoff UI ---------------------------------------------------
    tabItem(
      tabName = "CutoffTab",
      source(
        file = "ui/components/tab_cutoff.R",
        local = TRUE,
        encoding = "UTF-8"
      )$value
    ),
    # --- Run BRINDA Adjustment UI -------------------------------------------
    tabItem(
      tabName = "BrindaTab",
      source(
        file = "ui/components/tab_brinda.R",
        local = TRUE,
        encoding = "UTF-8"
      )$value
    ),
    # --- Generate Report UI -------------------------------------------------
    tabItem(
      tabName = "ReportTab",
      source(
        file = "ui/components/tab_report.R",
        local = TRUE,
        encoding = "UTF-8"
      )$value
    )
  )
)