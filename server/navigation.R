DataImportSidebar <- source("ui/components/sidebar_dataimport.R")$value
BiomarkerSidebar <- source("ui/components/sidebar_biomarker.R")$value
CutoffSidebar <- source("ui/components/sidebar_cutoff.R")$value
BrindaSidebar <- source("ui/components/sidebar_brinda.R")$value
ReportSidebar <- source("ui/components/sidebar_report.R")$value


observe_sidebar_transitions <- function(input, output) {

  observe({
    # DataImportTab
    observeEvent(1==1,{output$sidebarmenu <- DataImportSidebar})
    observeEvent(input$clickNextOnDataImportPage,{output$sidebarmenu <- BiomarkerSidebar})

    # BiomarkerTab
    observeEvent(input$clickPrevOnBiomarkerPage,{output$sidebarmenu <- DataImportSidebar})
    observeEvent(input$clickNextOnBiomarkerPage,{output$sidebarmenu <- CutoffSidebar})

    # CutoffTab
    observeEvent(input$clickPrevOnCutoffPage,{output$sidebarmenu <- BiomarkerSidebar})
    observeEvent(input$clickNextOnCutoffPage,{output$sidebarmenu <- BrindaSidebar})

    # BrindaTab
    observeEvent(input$clickPrevOnBrindaPage,{output$sidebarmenu <- CutoffSidebar})
    observeEvent(input$clickNextOnBrindaPage,{output$sidebarmenu <- ReportSidebar})

    # ReportTab
    observeEvent(input$clickPrevOnReportPage,{output$sidebarmenu <- BrindaSidebar})
  })
}

observe_tab_navigation <- function(input, output, session) {

    # DataImportTab
    observeEvent(
        input$clickNextOnDataImportPage,{
            updateTabItems(session, "tabs", "BiomarkerTab")
    })

    # BiomarkerTab
    observeEvent(
        input$clickPrevOnBiomarkerPage,{
            updateTabItems(session, "tabs", "DataImportTab")
    })

    observeEvent(
        input$clickNextOnBiomarkerPage,{
            updateTabItems(session, "tabs", "CutoffTab")
    })

    # CutoffTab
    observeEvent(
        input$clickPrevOnCutoffPage,{
            updateTabItems(session, "tabs", "BiomarkerTab")
    })

    observeEvent(
        input$clickNextOnCutoffPage,{
            updateTabItems(session, "tabs", "BrindaTab")
    })

    # BrindaTab
    observeEvent(
        input$clickPrevOnBrindaPage,{
            updateTabItems(session, "tabs", "CutoffTab")
    })

    observeEvent(
        input$clickNextOnBrindaPage,{
        updateTabItems(session, "tabs", "ReportTab")
    })

    # ReportTab
    observeEvent(
        input$clickPrevOnReportPage,{
            updateTabItems(session, "tabs", "BrindaTab")
    })
}
