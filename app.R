# --- Load Libraries -----------------------------------------------------------
library(shiny)
library(ggplot2)
library(datamods)
library(sortable)
library(shinydashboard)
library(shinyalert)
library(DT)
library(tidyverse)
library(patchwork)
library(ggridges)
library(BRINDA)
library(fresh)
source("./R/unit_converter.R")

# --- Shiny theme --------------------------------------------------------------
mytheme <- create_theme(
  theme = "flatly",
  adminlte_color(
    light_blue = "#301014"
  ),
  adminlte_sidebar(
    dark_bg = "#121111",
    dark_hover_bg = "#140708",
    dark_color = "white",
    width = "300px"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "white", 
    info_box_bg = "black"
  )
)

# --- colors -------------------------------------------------------------------
cols <- c(
  "#a6cee3",
  "#1f78b4",
  "#b2df8a",
  "#33a02c",
  "#fb9a99",
  "#e31a1c",
  "#fdbf6f",
  "#ff7f00",
  "#cab2d6",
  "#6a3d9a",
  "#b15928"
)

# --- header -------------------------------------------------------------------
body <- dashboardPage(
  dashboardHeader(
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 100px;}"),
            tags$style(".main-header .logo {height: 100px;}")),
    title = div(
      img(
        id="back2main",
        src="logo1.png",
        style = "height:95px;")
    )),

  # --- sidebar ----------------------------------------------------------------
  dashboardSidebar(
    tags$style(".left-side, .main-sidebar {padding-top: 100px;}"),
    use_theme(mytheme),
    width = 300,
    sidebarMenuOutput("sidebarmenu")
  ),
  # --- body -------------------------------------------------------------------
  dashboardBody(
    tags$style("
              body {
    -moz-transform: scale(1.1, 1.1); /* Moz-browsers */
    zoom: 1.1; /* Other non-webkit browsers */
    zoom: 110%; /* Webkit browsers */
}
              "),
    use_theme(mytheme),
    tabItems(
      # --- Import Data UI -----------------------------------------------------
      tabItem(
        tabName = "import",
        source(
          file = "ui_import.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value
      ),
      # --- Select Biomarkers UI -----------------------------------------------
      tabItem(
        tabName = "select",
        source(
          file = "ui_select.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value
      ),
      # --- Select Cutoff UI ---------------------------------------------------
      tabItem(
        tabName = "cutoff",
        source(
          file = "ui_cutoff.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value
      ),
      # --- Run BRINDA Adjustment UI -------------------------------------------
      tabItem(
        tabName = "brinda",
        source(
          file = "ui_brinda.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value
      ),
      # --- Generate Report UI -------------------------------------------------
      tabItem(
        tabName = "report",
        source(
          file = "ui_report.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value
      )
    )
  )
)

# --- Shiny UI -----------------------------------------------------------------
ui <- function(request){(body)}

# --- Shiny Server -------------------------------------------------------------
server <- function(input, output, session) {
  # --- Section to Import Data -------------------------------------------------
  observeEvent(input$importData, {
    import_modal(
      id = "myid",
      from = c("file"),
      title = "Import data to be used in application"
    )
  })
  
  imported <- import_server("myid", return_class = "tbl_df")
  # --- Render data table ------------------------------------------------------
  output$table <- renderDataTable({
    req(imported$data())
    df <- imported$data()
    datatable(df,options = list(scrollX = T,lengthMenu=c(5,10,25)))
  })
  # --- Update Sidebar depending on what step you are on -----------------------
  observe({
    observeEvent(1==1,{
      output$sidebarmenu <- renderMenu({
        sidebarMenu(
          id = "tabs",
          menuItem("Step 1: Import Data",
                   tabName = "import",
                   icon = icon("cloud-upload"),
                   badgeLabel = "Step 1",
                   badgeColor = "yellow",
                   selected = T),
          menuItem("Step 2: Select Biomarkers",
                   tabName = "blank2",
                   icon = icon("list"),
                   badgeLabel = "Step 2",
                   badgeColor = "black"),
          menuItem("Step 3: Optional: Apply Cutoff",
                   tabName = "blank3",
                   icon = icon("filter"),
                   badgeLabel = "Step 3",
                   badgeColor = "black"),
          menuItem("Step 4: Run BRINDA Adjustment",
                   tabName = "blank4",
                   icon =  icon("calculator"),
                   badgeLabel = "Step 4",
                   badgeColor = "black"),
          menuItem("Step 5: Report",
                   tabName = "blank5",
                   icon =  icon("file"),
                   badgeLabel = "Step 5",
                   badgeColor = "black")
        )
      })
    })
    observeEvent(input$importData,{
      output$sidebarmenu <- renderMenu({
        sidebarMenu(
          id = "tabs",
          menuItem("Step 1: Import Data",
                   tabName = "import",
                   icon = icon("cloud-upload"),
                   badgeLabel = "Step 1",
                   badgeColor = "yellow",
                   selected = T),
          menuItem("Step 2: Select Biomarkers",
                   tabName = "select",
                   icon = icon("list"),
                   badgeLabel = "Step 2",
                   badgeColor = "orange"),
          menuItem("Step 3: Optional: Apply Cutoff",
                   tabName = "blank3",
                   icon = icon("filter"),
                   badgeLabel = "Step 3",
                   badgeColor = "black"),
          menuItem("Step 4: Run BRINDA Adjustment",
                   tabName = "blank4",
                   icon =  icon("calculator"),
                   badgeLabel = "Step 4",
                   badgeColor = "black"),
          menuItem("Step 5: Report",
                   tabName = "blank5",
                   icon =  icon("file"),
                   badgeLabel = "Step 5",
                   badgeColor = "black")
        )
      })
    })
    observeEvent(input$setMarkers,{
      output$sidebarmenu <- renderMenu({
        sidebarMenu(
          id = "tabs",
          menuItem("Step 1: Import Data",
                   tabName = "import",
                   icon = icon("cloud-upload"),
                   badgeLabel = "Step 1",
                   badgeColor = "yellow"),
          menuItem("Step 2: Select Biomarkers",
                   tabName = "select",
                   icon = icon("list"),
                   badgeLabel = "Step 2",
                   badgeColor = "orange",
                   selected = T),
          menuItem("Step 3: Optional: Apply Cutoff",
                   tabName = "cutoff",
                   icon = icon("filter"),
                   badgeLabel = "Step 3",
                   badgeColor = "maroon"),
          menuItem("Step 4: Run BRINDA Adjustment",
                   tabName = "blank4",
                   icon =  icon("calculator"),
                   badgeLabel = "Step 4",
                   badgeColor = "black"),
          menuItem("Step 5: Report",
                   tabName = "blank5",
                   icon =  icon("file"),
                   badgeLabel = "Step 5",
                   badgeColor = "black")
        )
      })
    })
    observeEvent(input$setCutoff,{
      output$sidebarmenu <- renderMenu({
        sidebarMenu(
          id = "tabs",
          menuItem("Step 1: Import Data",
                   tabName = "import",
                   icon = icon("cloud-upload"),
                   badgeLabel = "Step 1",
                   badgeColor = "yellow"),
          menuItem("Step 2: Select Biomarkers",
                   tabName = "select",
                   icon = icon("list"),
                   badgeLabel = "Step 2",
                   badgeColor = "orange"),
          menuItem("Step 3: Optional: Apply Cutoff",
                   tabName = "cutoff",
                   icon = icon("filter"),
                   badgeLabel = "Step 3",
                   badgeColor = "maroon",
                   selected = T),
          menuItem("Step 4: Run BRINDA Adjustment",
                   tabName = "brinda",
                   icon =  icon("calculator"),
                   badgeLabel = "Step 4",
                   badgeColor = "maroon"),
          menuItem("Step 5: Report",
                   tabName = "blank5",
                   icon =  icon("file"),
                   badgeLabel = "Step 5",
                   badgeColor = "black")
        )
      })
    })
    observeEvent(input$applyBrinda,{
      output$sidebarmenu <- renderMenu({
        sidebarMenu(
          id = "tabs",
          menuItem("Step 1: Import Data",
                   tabName = "import",
                   icon = icon("cloud-upload"),
                   badgeLabel = "Step 1",
                   badgeColor = "yellow"),
          menuItem("Step 2: Select Biomarkers",
                   tabName = "select",
                   icon = icon("list"),
                   badgeLabel = "Step 2",
                   badgeColor = "orange"),
          menuItem("Step 3: Optional: Apply Cutoff",
                   tabName = "cutoff",
                   icon = icon("filter"),
                   badgeLabel = "Step 3",
                   badgeColor = "maroon"),
          menuItem("Step 4: Run BRINDA Adjustment",
                   tabName = "brinda",
                   icon =  icon("calculator"),
                   badgeLabel = "Step 4",
                   badgeColor = "maroon",
                   selected = T),
          menuItem("Step 5: Report",
                   tabName = "report",
                   icon =  icon("file"),
                   badgeLabel = "Step 5",
                   badgeColor = "fuchsia")
        )
      })
    })
  })
  # --- Update Selected Tab Based on Previous/Next Buttons ---------------------
  observeEvent(input$prevImport,{
    updateTabItems(session,
                   "tabs",
                   "import")
  })
  observeEvent(input$nextSelect,{
    updateTabItems(session,
                   "tabs",
                   "select")
  })
  observeEvent(input$prevSelect,{
    updateTabItems(session,
                   "tabs",
                   "select")
  })
  observeEvent(input$nextCutoff,{
    updateTabItems(session,
                   "tabs",
                   "cutoff")
  })
  observeEvent(input$prevCutoff,{
    updateTabItems(session,
                   "tabs",
                   "cutoff")
  })
  observeEvent(input$nextBrinda,{
    updateTabItems(session,
                   "tabs",
                   "brinda")
  })
  observeEvent(input$prevBrinda,{
    updateTabItems(session,
                   "tabs",
                   "brinda")
  })
  observeEvent(input$nextReport,{
    updateTabItems(session,
                   "tabs",
                   "report")
  })
  # --- Updates Column Names Per User's Data -----------------------------------
  ## Dynamically removes options based on 
  ## user's previous choice as to not 
  ## select the same column twice
  observe({
    updateSelectInput(session, "rbp",
                      label = "Retinol Binding Protein",
                      choices = c("",names(imported$data())[!(names(imported$data()) %in% c(input$rt,input$ft,input$stfr,input$zn,input$agp,input$crp,input$fasting,input$time))]),
                      selected = input$rbp)
    updateSelectInput(session, "rt",
                      label = "Retinol",
                      choices = c("",names(imported$data())[!(names(imported$data()) %in% c(input$rbp,input$ft,input$stfr,input$zn,input$agp,input$crp,input$fasting,input$time))]),
                      selected = input$rt)
    updateSelectInput(session, "ft",
                      label = "Ferritin",
                      choices = c("",names(imported$data())[!(names(imported$data()) %in% c(input$rt,input$rbp,input$stfr,input$zn,input$agp,input$crp,input$fasting,input$time))]),
                      selected = input$ft)
    updateSelectInput(session, "stfr",
                      label = "Soluble Transferrin Receptor",
                      choices = c("",names(imported$data())[!(names(imported$data()) %in% c(input$rt,input$ft,input$rbp,input$zn,input$agp,input$crp,input$fasting,input$time))]),
                      selected = input$stfr)
    updateSelectInput(session, "zn",
                      label = "Zinc",
                      choices = c("",names(imported$data())[!(names(imported$data()) %in% c(input$rt,input$ft,input$stfr,input$rbp,input$agp,input$crp,input$fasting,input$time))]),
                      selected = input$zn)
    updateSelectInput(session, "agp",
                      label = "AGP *",
                      choices = c("",names(imported$data())[!(names(imported$data()) %in% c(input$rt,input$ft,input$stfr,input$zn,input$rbp,input$crp,input$fasting,input$time))]),
                      selected = input$agp)
    updateSelectInput(session, "crp",
                      label = "CRP *",
                      choices = c("",names(imported$data())[!(names(imported$data()) %in% c(input$rt,input$ft,input$stfr,input$zn,input$agp,input$rbp,input$fasting,input$time))]),
                      selected = input$crp)

  # --- Density Plots to be generated ------------------------------------------
  ## each time the user selects what 
  ## columns in their data are the marker columns
  output$rbpPlot <- renderPlot({
    req(input$rbp)
    ggplot(imported$data(), aes(x=imported$data()[[as.character(input$rbp)]])) +
      geom_density(fill=cols[1],color="darkslategrey")+
      theme_minimal()+
      ggtitle(
        paste("Retinol Binding Protein (",as.character(input$rbpU),")",sep="")
      )+
      ylab("")+
      xlab(as.character(input$rbpU))
  })
  output$rtPlot <- renderPlot({
    req(input$rt)
    ggplot(imported$data(), aes(x=imported$data()[[as.character(input$rt)]])) +
      geom_density(fill=cols[2],color="darkslategrey")+
      theme_minimal()+
      ggtitle(
        paste("Retinol (",as.character(input$rtU),")",sep="")
      )+
      ylab("")+
      xlab(as.character(input$rtU))
  })
  output$ftPlot <- renderPlot({
    req(input$ft)
    ggplot(imported$data(), aes(x=imported$data()[[as.character(input$ft)]])) +
      geom_density(fill=cols[3],color="darkslategrey")+
      theme_minimal()+
      ggtitle(
        paste("Ferritin (",as.character(input$ftU),")",sep="")
      )+
      ylab("")+
      xlab(as.character(input$ftU))
  })
  output$stfrPlot <- renderPlot({
    req(input$stfr)
    ggplot(imported$data(), aes(x=imported$data()[[as.character(input$stfr)]])) +
      geom_density(fill=cols[4],color="darkslategrey")+
      theme_minimal()+
      ggtitle(
        paste("Soluble Transferrin Receptor (",as.character(input$stfrU),")",sep="")
      )+
      ylab("")+
      xlab(as.character(input$stfrU))
  })
  output$znPlot <- renderPlot({
    req(input$zn)
    ggplot(imported$data(), aes(x=imported$data()[[as.character(input$zn)]])) +
      geom_density(fill=cols[5],color="darkslategrey")+
      theme_minimal()+
      ggtitle(
        paste("Zinc (",as.character(input$znU),")",sep="")
      )+
      ylab("")+
      xlab(as.character(input$znU))
  })
  output$agpPlot <- renderPlot({
    req(input$agp)
    ggplot(imported$data(), aes(x=imported$data()[[as.character(input$agp)]])) +
      geom_density(fill=cols[6],color="darkslategrey")+
      theme_minimal()+
      ggtitle(
        paste("AGP (",as.character(input$agpU),")",sep="")
      )+
      ylab("")+
      xlab(as.character(input$agpU))
  })
  output$crpPlot <- renderPlot({
    req(input$crp)
    ggplot(imported$data(), aes(x=imported$data()[[as.character(input$crp)]])) +
      geom_density(fill=cols[7],color="darkslategrey")+
      theme_minimal()+
      ggtitle(
        paste("CRP (",as.character(input$crpU),")",sep="")
      )+
      ylab("")+
      xlab(as.character(input$crpU))
  })
  # --- Generates Pop-up when steps are completed ------------------------------
  observe({
    req(input$setMarkers)
    shinyalert("DONE",
               paste("Biomarkers were set successfully."),
               type = "success")
  })
  observe({
    req(input$setCutoff)
    shinyalert("DONE",
               paste("Cutoffs were set successfully."),
               type = "success")
  })
  observe({
    req(input$applyBrinda)
    shinyalert("DONE",
               paste("BRINDA adjustment was applied successfully."),
               type = "success")
  })
  
  
  # --- Adds in manual AGP/CRP boxes when user selects manual population -------
  observe({
    req(input$pop)
    if(input$pop == "Manual"){
      output$manAgp <- renderUI({
        textInput("manAgp", "Enter Manual AGP Reference Value")
      })
      output$manCrp <- renderUI({
        textInput("manCrp", "Enter Manual CRP Reference Value")
      })
    }else{
      output$manAgp <- renderUI({
      })
      output$manCrp <- renderUI({
      })
    }
  })
  # --- Cutoff Unit Output -----------------------------------------------------
  ## Outputs the same unit as specified in step 2
  output$rbpUnitC <- renderText({
    req(input$rbpU)
    input$rbpU
  })
  output$rtUnitC <- renderText({
    req(input$rtU)
    input$rtU
  })
  output$ftUnitC <- renderText({
    req(input$ftU)
    input$ftU
  })
  output$stfrUnitC <- renderText({
    req(input$stfrU)
    input$stfrU
  })
  output$zn_morn_fast_UnitC <- renderText({
    req(input$znU)
    input$znU
  })
  output$zn_morn_nonfast_UnitC <- renderText({
    req(input$znU)
    input$znU
  })
  output$zn_after_nonfast_UnitC <- renderText({
    req(input$znU)
    input$znU
  })
  # --- output suggested cutoffs -----------------------------------------------
  output$rbpSug <- renderText({
    req(input$rbp)
    req(input$rbpU)
    req(input$pop)
    if(as.character(input$rbpU) == "\u03BCmol/L"){
      cutoff="0.7 \u03BCmol/L"
    }else if(as.character(input$rbpU) == "\u03BCg/dL"){
      cutoff=paste(
        as.character(0.7 * 0.03491),
        "\u03BCg/dL"
      )
    }
    return(cutoff)
  })
  output$rtSug <- renderText({
    req(input$rt)
    req(input$rtU)
    req(input$pop)
    if(as.character(input$rtU) == "\u03BCmol/L"){
      cutoff="0.7 \u03BCmol/L"
    }else if(as.character(input$rtU) == "\u03BCg/dL"){
      cutoff=paste(
        as.character(0.7 * 0.03491),
        "\u03BCg/dL"
      )
    }
    return(cutoff)
  })
  output$ftSug <- renderText({
    req(input$ft)
    req(input$ftU)
    req(input$pop)
    if(as.character(input$ftU) == "\u03BCg/L" &
       as.character(input$pop) == "PSC"){
      cutoff="12 \u03BCg/L"
    }else if(as.character(input$ftU) == "\u03BCg/L" &
             as.character(input$pop) == "WRA"){
      cutoff="15 \u03BCg/L"
    }else if(as.character(input$ftU) == "ng/mL" &
             as.character(input$pop) == "PSC"){
      cutoff="12 ng/mL"
    }else if(as.character(input$ftU) == "ng/mL" &
             as.character(input$pop) == "WRA"){
      cutoff="15 ng/mL"
    }else if(as.character(input$ftU) == "\u03BCg/L" &
             !(as.character(input$pop) %in% c("WRA","PSC"))){
      cutoff="12-15 \u03BCg/L"
    }else if(as.character(input$ftU) == "ng/mL" &
             !(as.character(input$pop) %in% c("WRA","PSC"))){
      cutoff="12-15 ng/mL"
    }
    return(cutoff)
  })
  output$stfrSug <- renderText({
    req(input$stfr)
    req(input$stfrU)
    req(input$pop)
    if(as.character(input$stfrU) == "mg/L"){
      cutoff="8.3 mg/L"
    }else if(as.character(input$stfrU) == "g/L"){
      cutoff="8.3 g/L"
    }
    return(cutoff)
  })
  "\u03BCmol/L"
  output$znSug <- renderText({
    req(input$zn)
    req(input$znU)
    req(input$pop)
    req(input$fastingVar)
    req(input$timeVar)
    #fastingVarClean <- ifelse()
    #timeVarClean <- ifelse()
    if(as.character(input$znU) == "\u03BCmol/L" &
       as.character(input$pop) == "PSC" &
       as.character(input$timeVar) == "morning"){
      cutoff="9.9 \u03BCmol/L"
    }else if(as.character(input$znU) == "\u03BCmol/L" &
             as.character(input$pop) == "PSC" &
             as.character(input$timeVar) != "morning"){
      cutoff="8.7 \u03BCmol/L"
    }else if(as.character(input$znU) == "\u03BCmol/L" &
             as.character(input$pop) == "WRA" &
             as.character(input$fastingVar) == "fasted" &
             as.character(input$timeVar) == "morning"){
      cutoff="10.7 \u03BCmol/L"
    }else if(as.character(input$znU) == "\u03BCmol/L" &
             as.character(input$pop) == "WRA" &
             as.character(input$fastingVar) != "fasted" &
             as.character(input$timeVar) == "morning"){
      cutoff="10.1 \u03BCmol/L"
    }else if(as.character(input$znU) == "\u03BCmol/L" &
             as.character(input$pop) == "WRA" &
             as.character(input$timeVar) != "morning"){
      cutoff="9.0 \u03BCmol/L"
    }else if(as.character(input$znU) == "\u03BCmol/L" &
             !(as.character(input$pop) %in% c("WRA","PSC"))){
      cutoff="9.3-11.3 \u03BCmol/L"
    }
    #########################################################
    else if(as.character(input$znU) == "\u03BCg/L" &
       as.character(input$pop) == "PSC" &
       as.character(input$timeVar) == "morning"){
      cutoff=paste(as.character(9.9*64.5),
                   "\u03BCg/L")
    }else if(as.character(input$znU) == "\u03BCg/L" &
             as.character(input$pop) == "PSC" &
             as.character(input$timeVar) != "morning"){
      cutoff=paste(as.character(8.7*64.5),
                   "\u03BCg/L")
    }else if(as.character(input$znU) == "\u03BCg/L" &
             as.character(input$pop) == "WRA" &
             as.character(input$fastingVar) == "fasted" &
             as.character(input$timeVar) == "morning"){
      cutoff=paste(as.character(10.7*64.5),
                   "\u03BCg/L")
    }else if(as.character(input$znU) == "\u03BCg/L" &
             as.character(input$pop) == "WRA" &
             as.character(input$fastingVar) != "fasted" &
             as.character(input$timeVar) == "morning"){
      cutoff=paste(as.character(10.1*64.5),
                   "\u03BCg/L")
    }else if(as.character(input$znU) == "\u03BCg/L" &
             as.character(input$pop) == "WRA" &
             as.character(input$timeVar) != "morning"){
      cutoff=paste(as.character(9.0*64.5),
                   "\u03BCg/L")
    }else if(as.character(input$znU) == "\u03BCg/L" &
             !(as.character(input$pop) %in% c("WRA","PSC"))){
      cutoff="599.9-728.9 \u03BCg/L"
    }
    return(cutoff)
  })
  # --- Plot cutoff density plots ----------------------------------------------
  cutoffBarPlot <- function(){
    cutoff_map <- data.frame(
      variable=c(
        as.character(input$ft),
        as.character(input$stfr),
        as.character(input$rt),
        as.character(input$rbp),
        as.character(input$zn),
        as.character(input$agp),
        as.character(input$crp)
      ),
      cutoff=c(
        as.numeric(input$ftC),
        as.numeric(input$stfrC),
        as.numeric(input$rtC),
        as.numeric(input$rbpC),
        as.numeric(input$znC),
        1,
        5
      ),
      long_name =c(
        paste0("Ferritin"," (",as.character(input$ftU),")"),
        paste0("Soluble Transferrin Receptor"," (",as.character(input$stfrU),")"),
        paste0("Retinol"," (",as.character(input$rtU),")"),
        paste0("Retinol Binding Protein"," (",as.character(input$rbpU),")"),
        paste0("Zinc"," (",as.character(input$znU),")"),
        paste0("AGP"," (",as.character(input$agpU),")"),
        paste0("CRP"," (",as.character(input$crpU),")")
      )
    )
    
    def_df <- imported$data() %>%
      reshape2::melt() %>% 
      inner_join(.,
                 cutoff_map,
                 by="variable") %>%
      group_by(variable,long_name) %>%
      mutate(deficient=ifelse(value<cutoff,"deficient","normal"),
             stfr_deficient=ifelse(value>cutoff,"deficient","normal")) %>%
      summarise(percent_def=length(deficient[deficient=="deficient"])/
                  length(deficient)*100,
                stfr_percent_def=length(stfr_deficient[stfr_deficient=="deficient"])/
                  length(stfr_deficient)*100)
    
    # implementing fix for Soluble Transferrin Receptor deficiency
    def_df$percent_def[def_df$long_name=="Soluble Transferrin Receptor"] = def_df$stfr_percent_def[def_df$long_name=="Soluble Transferrin Receptor"]
    
    # ensure order for biomarkers
    def_df$long_name <- factor(def_df$long_name,
                               levels =c(
                                 paste0("Ferritin"," (",as.character(input$ftU),")"),
                                 paste0("Soluble Transferrin Receptor"," (",as.character(input$stfrU),")"),
                                 paste0("Retinol"," (",as.character(input$rtU),")"),
                                 paste0("Retinol Binding Protein"," (",as.character(input$rbpU),")"),
                                 paste0("Zinc"," (",as.character(input$znU),")"),
                                 paste0("AGP"," (",as.character(input$agpU),")"),
                                 paste0("CRP"," (",as.character(input$crpU),")")
                               ))
    
    # plot the deficiency for biomarkers
    ggplot(def_df %>%
                   mutate(Reference=ifelse(
                     grepl("agp|crp",long_name,ignore.case = TRUE),
                     "Reference Biomarker",
                     "Biomarker")), 
                 aes(x = long_name, y= percent_def)) +
      geom_bar(position="dodge",stat="identity",width = 0.5,fill=cols[6]) +
      theme_bw()+
      scale_y_continuous(labels = scales::percent)+
      ylim(c(0,100))+
      labs(
        x="",
        y="Deficiency (%)",
        title = "Biomarker Deficiency - Prior to BRINDA"
      )+
      theme(text = element_text(size=14),
            legend.position = "none",
            axis.text.x =  element_text(angle=45,hjust=1,size=14))+
      facet_grid(~Reference,scales = "free_x",space = "free")
    
  }
  output$cutoffBar <- renderPlot({
    req(input$setCutoff)
    print(cutoffBarPlot())
  })
  # --- Download BRINDA adjusted barplot ---------------------------------------
  output$downloadNonAdjBar <- downloadHandler(
    filename = "BRINDA_non_adjusted_barplot.png",
    content = function(file) {
      png(file)
      print(cutoffBarPlot())
      dev.off()
    }) 
  # --- Apply BRINDA adjustment ------------------------------------------------
  ## make brinda variable available outside observe statement
  brinda <- reactive({
    req(input$applyBrinda)
    dataset<-imported$data()
    to_map <- data.frame(
      variable=c(
        as.character(input$ft),
        as.character(input$stfr),
        as.character(input$rt),
        as.character(input$rbp),
        as.character(input$zn),
        as.character(input$agp),
        as.character(input$crp)
      ),
      long_name =c(
        "sf",
        "stfr",
        "sr",
        "rbp",
        "zn",
        "agp",
        "crp"
      )
    )%>%
      mutate(variable=ifelse(variable=="",
                             "not_present",
                             variable))
    long_names <- stringi::stri_replace_all_regex(
      str=names(dataset),
      pattern=to_map$variable,
      replacement = to_map$long_name,
      vectorise_all = FALSE)
    
    names(dataset) <- long_names
    
    all_names <- ifelse(to_map$long_name %in% long_names,
                        to_map$long_name,
                        NA)
    
    args=list(
      retinol_binding_protein_varname=all_names[4],
      retinol_varname=all_names[3],
      ferritin_varname=all_names[1],
      soluble_transferrin_receptor_varname=all_names[2],
      zinc_varname=all_names[5],
      crp_varname=all_names[7],
      agp_varname=all_names[6],
      population=as.character(input$pop),
      output_format="FULL"
    )
    brinda<-purrr::pmap(args,BRINDA,dataset=dataset)
    if(as.character(input$outputType)=="only output adjusted values"){
      adj_names <- c(names(dataset),paste0(names(dataset),"_adj"))
      adj_names <- adj_names[adj_names %in% names(brinda[[1]])]
      result <- brinda[[1]] %>%
        select(all_of(adj_names))
    }else{
      result <- brinda[[1]]
    }
    return(result)
  })
  # --- output brinda results table --------------------------------------------
  output$brindaTbl <- renderDataTable({
    req(input$applyBrinda)
    req(input$pop)
    req(input$outputType)
    df <- brinda()
    datatable(df,options = list(scrollX = T,lengthMenu = c(5,10,25)))
  })
  # --- Barplot of Adjusted/Unadjusted Values ----------------------------------
  adjBarPlot <- function(){
    df <- brinda()
    adj_names <-names(df)[grepl("adj|AGP|agp|crp|CRP",names(df))]
    non_adj_names <- gsub("_adj","",adj_names)
    both_names <- c(non_adj_names,adj_names)
    tmp=reshape2::melt(
      df %>%
        select(all_of(both_names))
    )
    to_map <- data.frame(
      variable=c(
        variable=c(
          "sf",
          "stfr",
          "sr",
          "rbp",
          "zn",
          "agp",
          "crp",
          "sf_adj",
          "stfr_adj",
          "sr_adj",
          "rbp_adj",
          "zn_adj"
        )
      ),
      long_name =c(
        paste0("Ferritin"," (",as.character(input$ftU),")"),
        paste0("Soluble Transferrin Receptor"," (",as.character(input$stfrU),")"),
        paste0("Retinol"," (",as.character(input$rtU),")"),
        paste0("Retinol Binding Protein"," (",as.character(input$rbpU),")"),
        paste0("Zinc"," (",as.character(input$znU),")"),
        paste0("AGP"," (",as.character(input$agpU),")"),
        paste0("CRP"," (",as.character(input$crpU),")"),
        paste0("Ferritin - Adjusted"," (",as.character(input$ftU),")"),
        paste0("Soluble Transferrin Receptor - Adjusted"," (",as.character(input$stfrU),")"),
        paste0("Retinol - Adjusted"," (",as.character(input$rtU),")"),
        paste0("Retinol Binding Protein - Adjusted"," (",as.character(input$rbpU),")"),
        paste0("Zinc - Adjusted"," (",as.character(input$znU),")")
      )
    )
    
    plottingDF <- tmp %>%
      group_by(variable) %>%
      summarize(
        geom_mean = exp(mean(log(value),na.rm=T))
      ) %>%
      mutate(Adjusted = ifelse(grepl("adj",variable),"BRINDA Adjusted","Not Adjusted") ) %>%
      inner_join(.,
                 to_map,
                 by="variable") %>%
      mutate(base_var=factor(gsub(" - Adjusted","",long_name),
                             levels=c(
                               paste0("Ferritin"," (",as.character(input$ftU),")"),
                               paste0("Soluble Transferrin Receptor"," (",as.character(input$stfrU),")"),
                               paste0("Retinol"," (",as.character(input$rtU),")"),
                               paste0("Retinol Binding Protein"," (",as.character(input$rbpU),")"),
                               paste0("Zinc"," (",as.character(input$znU),")"),
                               paste0("AGP"," (",as.character(input$agpU),")"),
                               paste0("CRP"," (",as.character(input$crpU),")")
                             )))
    
    ggplot(plottingDF%>%
             mutate(Reference=ifelse(
               grepl("agp|crp",base_var,ignore.case = TRUE),
               "Reference Biomarker",
               "Biomarker")), aes(x = base_var, y= geom_mean, fill = Adjusted)) +
      geom_bar(stat="identity", width=.5, position = "dodge",alpha=0.8) +
      theme_bw()+
      scale_fill_manual(values = cols[c(2,6)])+
      scale_y_log10()+
      labs(
        x="",
        y="Geometric Mean",
        fill="",
        title="BRINDA Adjustment Bar Plot"
      )+
      theme(text = element_text(size=14),
            axis.text.x =  element_text(angle=45,hjust=1,size=14))+
      facet_grid(~Reference,scales = "free_x",space = "free")
  }
  
  output$adjBarPlot <- renderPlot({
    print(adjBarPlot())
  })
  
  defBarPlot <- function(){
    df <- brinda()
    adj_names <-names(df)[grepl("adj|AGP|agp|crp|CRP",names(df))]
    non_adj_names <- gsub("_adj","",adj_names)
    both_names <- c(non_adj_names,adj_names)
    adj_v_non_adj= df %>%
      select(all_of(both_names))
    cutoff_map <- data.frame(
      variable=c(
        variable=c(
          "sf",
          "stfr",
          "sr",
          "rbp",
          "zn",
          "agp",
          "crp",
          "sf_adj",
          "stfr_adj",
          "sr_adj",
          "rbp_adj",
          "zn_adj"
        )
      ),
      cutoff=c(
        as.numeric(input$ftC),
        as.numeric(input$stfrC),
        as.numeric(input$rtC),
        as.numeric(input$rbpC),
        as.numeric(input$znC),
        1,
        5,
        as.numeric(input$ftC),
        as.numeric(input$stfrC),
        as.numeric(input$rtC),
        as.numeric(input$rbpC),
        as.numeric(input$znC)
      ),
      long_name =c(
        paste0("Ferritin"," (",as.character(input$ftU),")"),
        paste0("Soluble Transferrin Receptor"," (",as.character(input$stfrU),")"),
        paste0("Retinol"," (",as.character(input$rtU),")"),
        paste0("Retinol Binding Protein"," (",as.character(input$rbpU),")"),
        paste0("Zinc"," (",as.character(input$znU),")"),
        paste0("AGP"," (",as.character(input$agpU),")"),
        paste0("CRP"," (",as.character(input$crpU),")"),
        paste0("Ferritin - Adjusted"," (",as.character(input$ftU),")"),
        paste0("Soluble Transferrin Receptor - Adjusted"," (",as.character(input$stfrU),")"),
        paste0("Retinol - Adjusted"," (",as.character(input$rtU),")"),
        paste0("Retinol Binding Protein - Adjusted"," (",as.character(input$rbpU),")"),
        paste0("Zinc - Adjusted"," (",as.character(input$znU),")")
        
      )
    )
    
    def_df <- adj_v_non_adj %>%
      reshape2::melt() %>% 
      inner_join(.,
                 cutoff_map,
                 by="variable") %>%
      group_by(variable,long_name) %>%
      mutate(deficient=ifelse(value<cutoff,"deficient","normal"),
             stfr_deficient=ifelse(value>cutoff,"deficient","normal")) %>%
      summarise(percent_def=length(deficient[deficient=="deficient"])/
                  length(deficient)*100,
                stfr_percent_def=length(stfr_deficient[stfr_deficient=="deficient"])/
                  length(stfr_deficient)*100)
    
    # implementing fix for Soluble Transferrin Receptor deficiency
    def_df$percent_def[def_df$long_name=="Soluble Transferrin Receptor"] = def_df$stfr_percent_def[def_df$long_name=="Soluble Transferrin Receptor"]
    
    # add in the biomarker column
    def_df$base_var <- factor(gsub(" - Adjusted","",def_df$long_name),
                              levels = c(
                                paste0("Ferritin"," (",as.character(input$ftU),")"),
                                paste0("Soluble Transferrin Receptor"," (",as.character(input$stfrU),")"),
                                paste0("Retinol"," (",as.character(input$rtU),")"),
                                paste0("Retinol Binding Protein"," (",as.character(input$rbpU),")"),
                                paste0("Zinc"," (",as.character(input$znU),")"),
                                paste0("AGP"," (",as.character(input$agpU),")"),
                                paste0("CRP"," (",as.character(input$crpU),")")
                              ))
    def_df$adj <- ifelse(grepl("Adjusted",def_df$long_name),
                         "BRINDA Adjusted",
                         "Not Adjusted"
    )
    
    # plot the deficiency for biomarkers
    ggplot(def_df%>%
             mutate(Reference=ifelse(
               grepl("agp|crp",base_var,ignore.case = TRUE),
               "Reference Biomarker",
               "Biomarker")), aes(x = base_var, y= percent_def,fill=adj)) +
      geom_bar(position="dodge",stat="identity",width = 0.5) +
      theme_bw()+
      scale_y_continuous(labels = scales::percent)+
      scale_fill_manual(values=cols[c(2,6)])+
      ylim(c(0,100))+
      labs(
        x="",
        y="Deficiency (%)",
        title = "Biomarker Deficiency",
        fill=""
      )+
      theme(text = element_text(size=14),
            axis.text.x =  element_text(angle=45,hjust=1,size=14))+
        facet_grid(~Reference,scales = "free_x",space = "free")
  }
  output$defBarPlot <- renderPlot({
    print(defBarPlot())
  })
  
  density <- function(){
    df <- brinda()
    adj_names <-names(df)[grepl("adj|AGP|agp|crp|CRP",names(df))]
    non_adj_names <- gsub("_adj","",adj_names)
    both_names <- c(non_adj_names,adj_names)
    
    to_map <- data.frame(
      variable=c(
        variable=c(
          "sf",
          "stfr",
          "sr",
          "rbp",
          "zn",
          "agp",
          "crp",
          "sf_adj",
          "stfr_adj",
          "sr_adj",
          "rbp_adj",
          "zn_adj"
        )
      ),
      long_name =c(
        paste0("Ferritin"," (",as.character(input$ftU),")"),
        paste0("Soluble Transferrin Receptor"," (",as.character(input$stfrU),")"),
        paste0("Retinol"," (",as.character(input$rtU),")"),
        paste0("Retinol Binding Protein"," (",as.character(input$rbpU),")"),
        paste0("Zinc"," (",as.character(input$znU),")"),
        paste0("AGP"," (",as.character(input$agpU),")"),
        paste0("CRP"," (",as.character(input$crpU),")"),
        paste0("Ferritin - Adjusted"," (",as.character(input$ftU),")"),
        paste0("Soluble Transferrin Receptor - Adjusted"," (",as.character(input$stfrU),")"),
        paste0("Retinol - Adjusted"," (",as.character(input$rtU),")"),
        paste0("Retinol Binding Protein - Adjusted"," (",as.character(input$rbpU),")"),
        paste0("Zinc - Adjusted"," (",as.character(input$znU),")")
      )
    )
    
    tmp=reshape2::melt(
      df %>%
        select(all_of(both_names))
    ) %>%
      mutate(Adjusted = ifelse(grepl("adj",variable),"BRINDA Adjusted","Not Adjusted")) %>%
      inner_join(
        .,
        to_map,
        by=c("variable")
      ) %>%
      mutate(base_var = factor(gsub(" - Adjusted","",long_name),
                               levels = c(
                                 paste0("CRP"," (",as.character(input$crpU),")"),
                                 paste0("AGP"," (",as.character(input$agpU),")"),
                                 paste0("Zinc"," (",as.character(input$znU),")"),
                                 paste0("Retinol Binding Protein"," (",as.character(input$rbpU),")"),
                                 paste0("Retinol"," (",as.character(input$rtU),")"),
                                 paste0("Soluble Transferrin Receptor"," (",as.character(input$stfrU),")"),
                                 paste0("Ferritin"," (",as.character(input$ftU),")")
                               )))
    
    ggplot(tmp%>%
             mutate(Reference=ifelse(
               grepl("agp|crp",base_var,ignore.case = TRUE),
               "Reference Biomarker",
               "Biomarker")), aes(y=base_var,x=value,fill=Adjusted)) +
      geom_density_ridges(color="darkslategrey",alpha=0.8)+
      scale_fill_manual(values = cols[c(2,6)])+
      theme_bw()+
      scale_x_log10()+
      labs(
        x="",
        y="",
        fill="",
        title="BRINDA Adjustment Density Plot"
      )+
      theme(text = element_text(size=14))+
      facet_grid(Reference~,scales = "free_x",space = "free")
  }
  output$densityplot <- renderPlot({
    print(density())
  })
  # --- Download BRINDA adjusted table -----------------------------------------
  output$downloadRes <- downloadHandler(
    filename = function() {
      paste("BRINDA_Results", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(brinda(), file, row.names = FALSE)
    })
  # --- Download BRINDA adjusted barplot ---------------------------------------
  output$downloadAdjBar <- downloadHandler(
    filename = "BRINDA_adjustment_barplot.png",
    content = function(file) {
      png(file)
      print(adjBarPlot())
      dev.off()
    })  
  # --- Download BRINDA adjusted barplot ---------------------------------------
  output$downloadDefBar <- downloadHandler(
    filename = "BRINDA_deficiency_barplot.png",
    content = function(file) {
      png(file)
      print(defBarPlot())
      dev.off()
    }) 
  # --- Download BRINDA adjusted density plot ----------------------------------
  output$downloadDensity <- downloadHandler(
    filename = "BRINDA_densityplot.png",
    content = function(file) {
      png(file)
      print(density())
      dev.off()
    }) 
  # --- Download BRINDA adjusted table -----------------------------------------
  output$downloadRes2 <- downloadHandler(
    filename = function() {
      paste("BRINDA_Results", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(brinda(), file, row.names = FALSE)
    })
  # --- Create a stats object --------------------------------------------------
  stats <- function(){
    to_map <- data.frame(
      variable=c(
          as.character(input$ft),
          as.character(input$stfr),
          as.character(input$rt),
          as.character(input$rbp),
          as.character(input$zn),
          as.character(input$agp),
          as.character(input$crp)
      ),
      Biomarker =c(
        "Ferritin",
        "Soluble Transferrin Receptor",
        "Retinol",
        "Retinol Binding Protein",
        "Zinc",
        "AGP",
        "CRP"
      ),
      Unit = c(
        as.character(input$ftU),
        as.character(input$stfrU),
        as.character(input$rtU),
        as.character(input$rbpU),
        as.character(input$znU),
        as.character(input$agpU),
        as.character(input$crpU)
      )
    )
    
    stats <- imported$data() %>%
      reshape2::melt() %>%
      group_by(variable) %>%
      summarise(
        N=length(value[!is.na(value)]),
        Minimum=min(value[!is.na(value)],na.rm = T),
        Mean=mean(value[!is.na(value)],na.rm=T),
        `Geometric Mean`=exp(mean(log(value[!is.na(value)]),na.rm=T)),
        Median=median(value[!is.na(value)],na.rm = T),
        Max=max(value[!is.na(value)],na.rm=T)) 
    
    stats_w_biomarker <- to_map %>%
      inner_join(.,
                 stats,
                 by="variable") %>%
      rename("Variable"="variable")
  }
  
  init_def <- function(){
    cutoff_map <- data.frame(
      variable=c(
        as.character(input$ft),
        as.character(input$stfr),
        as.character(input$rt),
        as.character(input$rbp),
        as.character(input$zn),
        as.character(input$agp),
        as.character(input$crp)
      ),
      cutoff=c(
        as.numeric(input$ftC),
        as.numeric(input$stfrC),
        as.numeric(input$rtC),
        as.numeric(input$rbpC),
        as.numeric(input$znC),
        1,
        5
      ),
      unit=c(
        as.character(input$ftU),
        as.character(input$stfrU),
        as.character(input$rtU),
        as.character(input$rbpU),
        as.character(input$znU),
        as.character(input$agpU),
        as.character(input$crpU)
      ),
      Biomarker =c(
        "Ferritin",
        "Soluble Transferrin Receptor",
        "Retinol",
        "Retinol Binding Protein",
        "Zinc",
        "AGP",
        "CRP"
        
      )
    )
    
    def_df <- imported$data() %>%
      reshape2::melt() %>% 
      inner_join(.,
                 cutoff_map,
                 by="variable") %>%
      group_by(variable,Biomarker,unit,cutoff) %>%
      mutate(deficient=ifelse(value<cutoff,"deficient","normal"),
             stfr_deficient=ifelse(value>cutoff,"deficient","normal")) %>%
      summarise(`Percent Deficiency`=length(deficient[deficient=="deficient"])/
                  length(deficient)*100,
                stfr_percent_def=length(stfr_deficient[stfr_deficient=="deficient"])/
                  length(stfr_deficient)*100) %>%
      ungroup()
    
    # implementing fix for Soluble Transferrin Receptor deficiency
    if("Soluble Transferrin Receptor" %in% def_df$Biomarker){
      def_df$`Percent Deficiency` <- ifelse(def_df$Biomarker == "Soluble Transferrin Receptor",
                                            def_df$stfr_percent_def,
                                            def_df$`Percent Deficiency`)
      def_df <- def_df %>%
        filter(!is.na(cutoff)) %>%
        mutate(`Value Cutoff`= ifelse(Biomarker == "Soluble Transferrin Receptor",
                                      paste(">",as.character(cutoff),unit),
                                      paste("<",as.character(cutoff),unit)
        )) %>%
        select(-c(stfr_percent_def,cutoff,unit)) %>%
        rename("Variable"="variable")
    }else{
      def_df <- def_df %>%
        filter(!is.na(cutoff)) %>%
        mutate(`Value Cutoff`= ifelse(Biomarker == "Soluble Transferrin Receptor",
                                      paste(">",as.character(cutoff),unit),
                                      paste("<",as.character(cutoff),unit)
        )) %>%
        select(-c(stfr_percent_def,cutoff,unit)) %>%
        rename("Variable"="variable")
    }
    return(def_df)
  }
  
  adj_def_df <- function(){
    cutoff_map <- data.frame(
      variable=c(
        "sf",
        "stfr",
        "sr",
        "rbp",
        "zn",
        "agp",
        "crp",
        "sf_adj",
        "stfr_adj",
        "sr_adj",
        "rbp_adj",
        "zn_adj"
      ),
      Biomarker =c(
        "Ferritin",
        "Soluble Transferrin Receptor",
        "Retinol",
        "Retinol Binding Protein",
        "Zinc",
        "AGP",
        "CRP",
        "Ferritin - Adjusted",
        "Soluble Transferrin Receptor - Adjusted",
        "Retinol - Adjusted",
        "Retinol Binding Protein - Adjusted",
        "Zinc - Adjusted"
      ),
      cutoff=c(
        as.numeric(input$ftC),
        as.numeric(input$stfrC),
        as.numeric(input$rtC),
        as.numeric(input$rbpC),
        as.numeric(input$znC),
        1,
        5,
        as.numeric(input$ftC),
        as.numeric(input$stfrC),
        as.numeric(input$rtC),
        as.numeric(input$rbpC),
        as.numeric(input$znC)
      ),
      unit=c(
        as.character(input$ftU),
        as.character(input$stfrU),
        as.character(input$rtU),
        as.character(input$rbpU),
        as.character(input$znU),
        as.character(input$agpU),
        as.character(input$crpU),
        as.character(input$ftU),
        as.character(input$stfrU),
        as.character(input$rtU),
        as.character(input$rbpU),
        as.character(input$znU)
      )
    )
    
    def_df <- brinda() %>%
      reshape2::melt() %>% 
      inner_join(.,
                 cutoff_map,
                 by="variable") %>%
      group_by(variable,Biomarker,unit,cutoff) %>%
      mutate(deficient=ifelse(value<cutoff,"deficient","normal"),
             stfr_deficient=ifelse(value>cutoff,"deficient","normal")) %>%
      summarise(`Percent Deficiency`=length(deficient[deficient=="deficient"])/
                  length(deficient)*100,
                stfr_percent_def=length(stfr_deficient[stfr_deficient=="deficient"])/
                  length(stfr_deficient)*100) %>%
      ungroup()
    
    # implementing fix for Soluble Transferrin Receptor deficiency
    if("Soluble Transferrin Receptor" %in% def_df$Biomarker){
      def_df$`Percent Deficiency` <- ifelse(def_df$Biomarker == "Soluble Transferrin Receptor",
                                            def_df$stfr_percent_def,
                                            def_df$`Percent Deficiency`)
      def_df <- def_df %>%
        filter(!is.na(cutoff)) %>%
        mutate(`Value Cutoff`= ifelse(Biomarker == "Soluble Transferrin Receptor",
                                      paste(">",as.character(cutoff),unit),
                                      paste("<",as.character(cutoff),unit)
        )) %>%
        select(-c(stfr_percent_def,cutoff,unit)) %>%
        rename("Variable"="variable")
    }else{
      def_df <- def_df %>%
        filter(!is.na(cutoff)) %>%
        mutate(`Value Cutoff`= ifelse(Biomarker == "Soluble Transferrin Receptor",
                                      paste(">",as.character(cutoff),unit),
                                      paste("<",as.character(cutoff),unit)
        )) %>%
        select(-c(stfr_percent_def,cutoff,unit)) %>%
        rename("Variable"="variable")
    }
    return(def_df)
  }
  # --- Download BRINDA adjustment Report --------------------------------------
  output$report <- downloadHandler(
    filename = function() {
      paste('BRINDA_Adjustment_Report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },

    content = function(file) {
      shinyalert("DONE",
                 paste("BRINDA adjustment report was generated successfully."),
                 type = "success")
      shiny::withProgress(
        message = paste0("Rendering", input$report, " Report"),
        value = 0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(5/10)
          src <- normalizePath('./reports/report.Rmd')
          
          # temporarily switch to the temp dir, in case you do not have write
          # permission to the current working directory
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          file.copy(src, 'report.Rmd', overwrite = TRUE)
          
          library(rmarkdown)
          out <- render('report.Rmd',
                        switch(
                          input$format,
                          PDF = pdf_document(),
                          HTML = html_document(
                            toc = TRUE,
                            toc_depth = 3,
                            toc_float = TRUE,
                            theme = "simplex"
                          ),
                          Word = word_document()),
                        params = list(data = imported$data(),
                                      rbp = input$rbp,
                                      rt = input$rt,
                                      ft = input$ft,
                                      stfr = input$stfr,
                                      zn = input$zn,
                                      agp = input$agp,
                                      crp = input$crp,
                                      pop = input$pop,
                                      rbpC = input$rbpC,
                                      rtC = input$rtC,
                                      ftC = input$ftC,
                                      stfrC = input$stfrC,
                                      znC = input$znC,
                                      initCut = cutoffBarPlot(),
                                      refAgp = input$refAgp,
                                      refCrp = input$refCrp,
                                      stats = stats(),
                                      init_df = init_def(),
                                      adj_def_df = adj_def_df(),
                                      adjBar = adjBarPlot(),
                                      defBar = defBarPlot(),
                                      density = density()))
          file.rename(out, file)
          shiny::incProgress(10/10)
        }
      )
      # src <- normalizePath('./reports/report.Rmd')
      # 
      # # temporarily switch to the temp dir, in case you do not have write
      # # permission to the current working directory
      # owd <- setwd(tempdir())
      # on.exit(setwd(owd))
      # file.copy(src, 'report.Rmd', overwrite = TRUE)
      # 
      # library(rmarkdown)
      # out <- render('report.Rmd',
      #               switch(
      #                 input$format,
      #                 PDF = pdf_document(),
      #                 HTML = html_document(
      #                   toc = TRUE,
      #                   toc_depth = 3,
      #                   toc_float = TRUE,
      #                   theme = "simplex"
      #                 ),
      #                 Word = word_document()),
      #               params = list(data = imported$data(),
      #                             rbp = input$rbp,
      #                             rt = input$rt,
      #                             ft = input$ft,
      #                             stfr = input$stfr,
      #                             zn = input$zn,
      #                             agp = input$agp,
      #                             crp = input$crp,
      #                             pop = input$pop,
      #                             rbpC = input$rbpC,
      #                             rtC = input$rtC,
      #                             ftC = input$ftC,
      #                             stfrC = input$stfrC,
      #                             znC = input$znC,
      #                             initCut = cutoffBarPlot(),
      #                             refAgp = input$refAgp,
      #                             refCrp = input$refCrp,
      #                             stats = stats(),
      #                             init_df = init_def(),
      #                             adj_def_df = adj_def_df(),
      #                             adjBar = adjBarPlot(),
      #                             defBar = defBarPlot(),
      #                             density = density()))
      # file.rename(out, file)
    }
  )
}
# --- Shiny Server and UI Call -------------------------------------------------
shinyApp(ui, server)
