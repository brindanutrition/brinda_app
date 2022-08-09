library(shiny)
library(ggplot2)
library(datamods)
library(sortable)
library(shinydashboard)
library(ggplot2)
library(shinyalert)
library(DT)
library(dplyr)
library(ggridges)
library(BRINDA)
library(fresh)

#############################################################################
## Shiny theme
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
#############################################################################
## colors
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

#############################################################################
## header
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
#############################################################################
## sidebar
  dashboardSidebar(
    tags$style(".left-side, .main-sidebar {padding-top: 100px;}"),
    use_theme(mytheme),
    width = 300,
    sidebarMenuOutput("sidebarmenu")
    ),
#############################################################################
## body
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
#############################################################################
## Import Data
      tabItem(
        tabName = "import",
        source(
          file = "ui_import.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value
      ),
#############################################################################
## Select Biomarkers
      tabItem(
        tabName = "select",
        uiOutput("drag_drop")
      ),
#############################################################################
## Select Cutoff
      tabItem(
        tabName = "cutoff",
        source(
          file = "ui_cutoff.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value
      ),
#############################################################################
## Run BRINDA Adjustment
      tabItem(
        tabName = "brinda",
        source(
          file = "ui_brinda.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value
      ),
#############################################################################
## Generate Report
      tabItem(
        tabName = "report",
        source(
          file = "ui_report.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value
        )
#############################################################################
    )
  )
)

#############################################################################
## Shiny UI
ui <- function(request){(body)}
#############################################################################
## Shiny Server
server <- function(input, output, session) {
#############################################################################
## Section to Import Data
  observeEvent(input$importData, {
    import_modal(
      id = "myid",
      from = c("file"),
      title = "Import data to be used in application"
    )
  })
  
  imported <- import_server("myid", return_class = "tbl_df")
#############################################################################
## Render data table with conditional formatting
  output$table <- renderDataTable({
    req(imported$data())
    df <- imported$data()
    datatable(df,options = list(scrollX = T,lengthMenu=c(5,10,25)))
  })
#############################################################################
## Update Sidebar depending on what 
## step you are on
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
#############################################################################
## Update Selected Tab Based on Previous/Next Buttons
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
#############################################################################
## Updates Column Names Per User's Data
## Dynamically removes options based on 
## user's previous choice as to not 
## select the same column twice
  observe({
    updateSelectInput(session, "rbp",
                      label = "Retinol Binding Protein *",
                      choices = c("",names(imported$data())[!(names(imported$data()) %in% c(input$rt,input$ft,input$stfr,input$zn,input$agp,input$crp,input$fasting,input$time))]),
                      selected = input$rbp)
    updateSelectInput(session, "rt",
                      label = "Retinol *",
                      choices = c("",names(imported$data())[!(names(imported$data()) %in% c(input$rbp,input$ft,input$stfr,input$zn,input$agp,input$crp,input$fasting,input$time))]),
                      selected = input$rt)
    updateSelectInput(session, "ft",
                      label = "Ferritin *",
                      choices = c("",names(imported$data())[!(names(imported$data()) %in% c(input$rt,input$rbp,input$stfr,input$zn,input$agp,input$crp,input$fasting,input$time))]),
                      selected = input$ft)
    updateSelectInput(session, "stfr",
                      label = "Soluble Transferrin Receptor *",
                      choices = c("",names(imported$data())[!(names(imported$data()) %in% c(input$rt,input$ft,input$rbp,input$zn,input$agp,input$crp,input$fasting,input$time))]),
                      selected = input$stfr)
    updateSelectInput(session, "zn",
                      label = "Zinc *",
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
    updateSelectInput(session, "fasting",
                      label = "Fasting Status *",
                      choices = c("",names(imported$data())[!(names(imported$data()) %in% c(input$rt,input$ft,input$stfr,input$zn,input$agp,input$rbp,input$time))]),
                      selected = input$fasting)
    updateSelectInput(session, "time",
                      label = "Time of Day *",
                      choices = c("",names(imported$data())[!(names(imported$data()) %in% c(input$rt,input$ft,input$stfr,input$zn,input$agp,input$rbp,input$fasting))]),
                      selected = input$time)
  })
  
  observeEvent(input$importData,{
    output$drag_drop <- renderUI({
      source(
        file = "ui_select.R",
        local = TRUE,
        encoding = "UTF-8"
      )$value
    })
  })
#############################################################################
## Density Plots to be generated
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

#############################################################################
## Generates Pop Ups When Certain Population 
## Groups Are Selected
  observe({
    req(input$pop)
    if(input$pop == "PSC"){
      shinyalert("Reference AGP/CRP Values Set",
                 paste("Reference Value of ln(AGP) set to: -0.52",
                       "Reference value of ln(CRP) set to: -2.26",
                       sep="\n"),
                 type = "info")
    } else if(input$pop == "WRA"){
      shinyalert("Reference AGP/CRP Values Set",
                 paste("ln(AGP) set to: -0.62",
                       "ln(CRP) set to: -1.83",
                       sep="\n"),
                 type="info")
    } else if(input$pop == "Other"){
      shinyalert("Reference AGP/CRP Values Set",
                 paste("ln(AGP) set to: lowest decile of AGP",
                       "ln(CRP) set to: lowest decile of CRP",
                       sep="\n"),
                 type="info")
    }
  })
#############################################################################
## Adds in manual AGP/CRP boxes when
## user selects manual population
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
##############################################################################
## Cutoff Unit Output
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
  ##############################################################################
  ## Output Deficiency Definition Tables

  output$defImage <- renderUI({
    width  <- session$clientData$output_defImage_width
    height <- session$clientData$output_defImage_height
    img(src="def.png",height=height,width=width)
  })
  output$defImage <- renderUI({
    # Read myImage's width and height. These are reactive values, so this
    # expression will re-run whenever they change.
    width  <- session$clientData$output_myImage_width
    height <- session$clientData$output_myImage_height
    
    # For high-res displays, this will be greater than 1
    pixelratio <- session$clientData$pixelratio
    img(src="def.png",width = width*pixelratio, height = height*pixelratio)
  
  })
##############################################################################
## Plot cutoff density plots
  cutoffBarPlot <- function(){
    rbp <-  imported$data() %>%
      select(c(as.character(input$rbp),as.character(input$fasting),as.character(input$time))) %>%
      mutate(biomarker=rep(as.character(input$rbp),nrow(.)))%>%
      mutate(deficient=ifelse(as.numeric(imported$data()[[as.character(input$rbp)]])<as.numeric(input$rbpC),"deficient","normal"))
    sr <-imported$data() %>%
      select(c(as.character(input$rt),as.character(input$fasting),as.character(input$time))) %>%
      mutate(biomarker=rep(as.character(input$rt),nrow(.)))%>%
      mutate(deficient=ifelse(as.numeric(imported$data()[[as.character(input$rt)]])<as.numeric(input$rtC),"deficient","normal"))
    sf <- imported$data() %>%
      select(c(as.character(input$ft),as.character(input$fasting),as.character(input$time))) %>%
      mutate(biomarker=rep(as.character(input$ft),nrow(.)))%>%
      mutate(deficient=ifelse(as.numeric(imported$data()[[as.character(input$ft)]])<as.numeric(input$ftC),"deficient","normal"))
    stfr <-imported$data() %>%
      select(c(as.character(input$stfr),as.character(input$fasting),as.character(input$time))) %>%
      mutate(biomarker=rep(as.character(input$stfr),nrow(.)))%>%
      mutate(deficient=ifelse(as.numeric(imported$data()[[as.character(input$stfr)]])<as.numeric(input$stfrC),"deficient","normal"))
    agp <- imported$data() %>%
      select(c(as.character(input$agp),as.character(input$fasting),as.character(input$time))) %>%
      mutate(biomarker=rep(as.character(input$agp),nrow(.)))%>%
      mutate(deficient=ifelse(as.numeric(imported$data()[[as.character(input$agp)]])<1,"deficient","normal"))
    crp <- imported$data() %>%
      select(c(as.character(input$crp),as.character(input$fasting),as.character(input$time))) %>%
      mutate(biomarker=rep(as.character(input$crp),nrow(.)))%>%
      mutate(deficient=ifelse(as.numeric(imported$data()[[as.character(input$crp)]])<5,"deficient","normal"))
    zn <- imported$data() %>%
      select(c(as.character(input$zn),as.character(input$fasting),as.character(input$time)))%>%
      mutate(biomarker=rep(as.character(input$zn),nrow(.)))%>%
      mutate(deficient=ifelse(
        (imported$data()[[as.character(input$fasting)]]=="fasted" & imported$data()[[as.character(input$time)]]=="morning" & as.numeric(imported$data()[[as.character(input$zn)]])<as.numeric(input$zn_morn_fast_C)) |
          (imported$data()[[as.character(input$fasting)]]=="fasted" & imported$data()[[as.character(input$time)]]=="afternoon" & as.numeric(imported$data()[[as.character(input$zn)]])<as.numeric(input$zn_morn_nonfast_C)) |
          (imported$data()[[as.character(input$fasting)]]=="non-fasted" & imported$data()[[as.character(input$time)]]=="afternoon" & as.numeric(imported$data()[[as.character(input$zn)]])<as.numeric(input$zn_after_nonfast_C))
        ,"deficient","normal"))
    defDf <- rbind.data.frame(
      rbp %>% select(biomarker,deficient),
      sr %>% select(biomarker,deficient),
      sf %>% select(biomarker,deficient),
      stfr %>% select(biomarker,deficient),
      zn %>% select(biomarker,deficient),
      agp %>% select(biomarker,deficient),
      crp %>% select(biomarker,deficient)
    ) %>%
      mutate(value=rep(1,nrow(.)))
    ggplot(defDf, aes(x = biomarker, y= value, fill = deficient)) +
      geom_bar(stat="identity", position = "fill", width = 0.5,alpha=0.8) +
      theme_bw()+
      scale_y_continuous(labels = scales::percent)+
      xlab("Biomarker")+
      ylab("Prevalence of Deficiency (%)")
  }
  output$cutoffBar <- renderPlot({
    req(input$setCutoff)
    print(cutoffBarPlot())
  })
##############################################################################
## Apply BRINDA adjustment
## make brinda variable available outside observe statement
  observe({
    req(input$applyBrinda)
    if (as.character(input$pop) == "WRA"){
      brinda <<- reactive({
        req(input$applyBrinda)
        req(input$pop)
        df = imported$data() %>%
          rename(rbp=as.character(input$rbp)) %>%
          rename(sr=as.character(input$rt)) %>%
          rename(sf=as.character(input$ft)) %>%
          rename(stfr=as.character(input$stfr)) %>%
          rename(zn=as.character(input$zn)) %>%
          rename(agp=as.character(input$agp)) %>%
          rename(crp=as.character(input$crp)) 
        if(as.character(input$outputType == "simple")){
          brinda <- BRINDA(
            dataset = df,
            retinol_binding_protein_varname = rbp,
            retinol_varname = sr,
            ferritin_varname = sf,
            soluble_transferrin_receptor_varname = stfr,
            zinc_varname = zn,
            crp_varname = crp,
            agp_varname = agp,
            population_group = "WRA",
            output_format = simple)
        }else{
          brinda <- BRINDA(
            dataset = df,
            retinol_binding_protein_varname = rbp,
            retinol_varname = sr,
            ferritin_varname = sf,
            soluble_transferrin_receptor_varname = stfr,
            zinc_varname = zn,
            crp_varname = crp,
            agp_varname = agp,
            population_group = "WRA",
            output_format = full)
        }
        brinda
      })
    }else if (as.character(input$pop) == "PSC"){
      brinda <<- reactive({
        req(input$applyBrinda)
        req(input$pop)
        df = imported$data() %>%
          rename(rbp=as.character(input$rbp)) %>%
          rename(sr=as.character(input$rt)) %>%
          rename(sf=as.character(input$ft)) %>%
          rename(stfr=as.character(input$stfr)) %>%
          rename(zn=as.character(input$zn)) %>%
          rename(agp=as.character(input$agp)) %>%
          rename(crp=as.character(input$crp)) 
        if(as.character(input$outputType == "simple")){
          brinda <- BRINDA(
            dataset = df,
            retinol_binding_protein_varname = rbp,
            retinol_varname = sr,
            ferritin_varname = sf,
            soluble_transferrin_receptor_varname = stfr,
            zinc_varname = zn,
            crp_varname = crp,
            agp_varname = agp,
            population_group = "PSC",
            output_format = simple)
        }else{
          brinda <- BRINDA(
            dataset = df,
            retinol_binding_protein_varname = rbp,
            retinol_varname = sr,
            ferritin_varname = sf,
            soluble_transferrin_receptor_varname = stfr,
            zinc_varname = zn,
            crp_varname = crp,
            agp_varname = agp,
            population_group = "PSC",
            output_format = full)
        }
        brinda
      })
    } else if (as.character(input$pop) == "Manual") {
      brinda <<- reactive({
        req(input$applyBrinda)
        req(input$pop)
        df = imported$data() %>%
          rename(rbp=as.character(input$rbp)) %>%
          rename(sr=as.character(input$rt)) %>%
          rename(sf=as.character(input$ft)) %>%
          rename(stfr=as.character(input$stfr)) %>%
          rename(zn=as.character(input$zn)) %>%
          rename(agp=as.character(input$agp)) %>%
          rename(crp=as.character(input$crp)) 
        if(as.character(input$outputType == "simple")){
          brinda <- BRINDA(
            dataset = df,
            retinol_binding_protein_varname = rbp,
            retinol_varname = sr,
            ferritin_varname = sf,
            soluble_transferrin_receptor_varname = stfr,
            zinc_varname = zn,
            crp_varname = crp,
            agp_varname = agp,
            population_group = "MANUAL",
            output_format = simple)
        }else{
          brinda <- BRINDA(
            dataset = df,
            retinol_binding_protein_varname = rbp,
            retinol_varname = sr,
            ferritin_varname = sf,
            soluble_transferrin_receptor_varname = stfr,
            zinc_varname = zn,
            crp_varname = crp,
            agp_varname = agp,
            population_group = "MANUAL",
            output_format = full)
        }
        brinda
      })
    } else if (as.character(input$pop) == "Other") {
      brinda <<- reactive({
        req(input$applyBrinda)
        req(input$pop)
        df = imported$data() %>%
          rename(rbp=as.character(input$rbp)) %>%
          rename(sr=as.character(input$rt)) %>%
          rename(sf=as.character(input$ft)) %>%
          rename(stfr=as.character(input$stfr)) %>%
          rename(zn=as.character(input$zn)) %>%
          rename(agp=as.character(input$agp)) %>%
          rename(crp=as.character(input$crp)) 
        if(as.character(input$outputType == "simple")){
          brinda <- BRINDA(
            dataset = df,
            retinol_binding_protein_varname = rbp,
            retinol_varname = sr,
            ferritin_varname = sf,
            soluble_transferrin_receptor_varname = stfr,
            zinc_varname = zn,
            crp_varname = crp,
            agp_varname = agp,
            population_group = "OTHER",
            output_format = simple)
        }else{
          brinda <- BRINDA(
            dataset = df,
            retinol_binding_protein_varname = rbp,
            retinol_varname = sr,
            ferritin_varname = sf,
            soluble_transferrin_receptor_varname = stfr,
            zinc_varname = zn,
            crp_varname = crp,
            agp_varname = agp,
            population_group = "OTHER",
            output_format = full)
        }
        brinda
      })
    }
  })
 
  #output brinda results table
  output$brindaTbl <- renderDataTable({
    req(input$applyBrinda)
    req(input$pop)
    df <- brinda()
    datatable(df,options = list(scrollX = T,lengthMenu = c(5,10,25)))
  })
  ############################################################################
  ## Barplot of Adjusted/Unadjusted Values
  adjBarPlot <- function(){
    df <- brinda()
    tmp=reshape2::melt(
      df[c(
        "rbp",
        "rbp_adj", 
        "sr",
        "sr_adj", 
        "sf",
        "sf_adj", 
        "stfr",
        "stfr_adj", 
        "zn",
        "zn_adj",
        "agp",
        "crp")])
    plottingDF <- tmp %>%
      group_by(variable) %>%
      summarize(
        geom_mean = exp(mean(log(value),na.rm=T))
      ) %>%
      mutate(gen=gsub("_.*","",variable)) %>%
      mutate(Adjusted = ifelse(grepl("adj",variable),"adjusted","non-adjusted") )
    
    ggplot(plottingDF, aes(x = gen, y= geom_mean, fill = Adjusted)) +
      geom_bar(stat="identity", width=.5, position = "dodge",alpha=0.8) +
      theme_bw()+
      scale_fill_manual(values = cols[c(2,6)])+
      scale_y_log10()+
      xlab("Biomarker") +
      ylab("Geometric Mean")
  }

  output$adjBarPlot <- renderPlot({
    print(adjBarPlot())
  })
  
  defBarPlot <- function(){
    rbp <-  brinda() %>%
      select(c("rbp_adj",as.character(input$fasting),as.character(input$time))) %>%
      mutate(biomarker=rep("rbp_adj",nrow(.)))%>%
      mutate(deficient=ifelse(as.numeric(brinda()[["rbp_adj"]])<as.numeric(input$rbpC),"deficient","normal"))
    sr <- brinda() %>%
      select(c("sr_adj",as.character(input$fasting),as.character(input$time))) %>%
      mutate(biomarker=rep("sr_adj",nrow(.)))%>%
      mutate(deficient=ifelse(as.numeric(brinda()[["sr_adj"]])<as.numeric(input$rtC),"deficient","normal"))
    sf <- brinda() %>%
      select(c("sf_adj",as.character(input$fasting),as.character(input$time))) %>%
      mutate(biomarker=rep("sf_adj",nrow(.)))%>%
      mutate(deficient=ifelse(as.numeric(brinda()[["sf_adj"]])<as.numeric(input$ftC),"deficient","normal"))
    stfr <- brinda() %>%
      select(c("stfr_adj",as.character(input$fasting),as.character(input$time))) %>%
      mutate(biomarker=rep("stfr_adj",nrow(.)))%>%
      mutate(deficient=ifelse(as.numeric(brinda()[["stfr_adj"]])<as.numeric(input$stfrC),"deficient","normal"))
    zn <- brinda() %>%
      select(c("zn_adj",as.character(input$fasting),as.character(input$time)))%>%
      mutate(biomarker=rep("zn_adj",nrow(.)))%>%
      mutate(deficient=ifelse(
        (brinda()[[as.character(input$fasting)]]=="fasted" & brinda()[[as.character(input$time)]]=="morning" & as.numeric(brinda()[["zn_adj"]])<as.numeric(input$zn_morn_fast_C)) |
          (brinda()[[as.character(input$fasting)]]=="fasted" & brinda()[[as.character(input$time)]]=="afternoon" & as.numeric(brinda()[["zn_adj"]])<as.numeric(input$zn_morn_nonfast_C)) |
          (brinda()[[as.character(input$fasting)]]=="non-fasted" & brinda()[[as.character(input$time)]]=="afternoon" & as.numeric(brinda()[["zn_adj"]])<as.numeric(input$zn_after_nonfast_C))
        ,"deficient","normal"))
    agp <- imported$data() %>%
      select(c(as.character(input$agp),as.character(input$fasting),as.character(input$time))) %>%
      mutate(biomarker=rep(as.character(input$agp),nrow(.)))%>%
      mutate(deficient=ifelse(as.numeric(imported$data()[[as.character(input$agp)]])<1,"deficient","normal"))
    crp <- imported$data() %>%
      select(c(as.character(input$crp),as.character(input$fasting),as.character(input$time))) %>%
      mutate(biomarker=rep(as.character(input$crp),nrow(.)))%>%
      mutate(deficient=ifelse(as.numeric(imported$data()[[as.character(input$crp)]])<5,"deficient","normal"))
    
    defDf <- rbind.data.frame(
      rbp %>% select(biomarker,deficient),
      sr %>% select(biomarker,deficient),
      sf %>% select(biomarker,deficient),
      stfr %>% select(biomarker,deficient),
      zn %>% select(biomarker,deficient),
      agp %>% select(biomarker,deficient),
      crp %>% select(biomarker,deficient)
      
    ) %>%
      mutate(value=rep(1,nrow(.)))
    ggplot(defDf, aes(x = biomarker, y= value, fill = deficient)) +
      geom_bar(stat="identity", position = "fill", width = 0.5,alpha=0.8) +
      theme_bw()+
      scale_y_continuous(labels = scales::percent)+
      xlab("Biomarker")+
      ylab("Prevalence of Deficiency (%)")
  }
  
  output$defBarPlot <- renderPlot({
    print(defBarPlot())
  })
  
  density <- function(){
    df <- brinda()
    tmp=reshape2::melt(
      df[c(
        "rbp",
        "rbp_adj", 
        "sr",
        "sr_adj", 
        "sf",
        "sf_adj", 
        "stfr",
        "stfr_adj", 
        "zn",
        "zn_adj",
        "agp",
        "crp")])
    plottingDF <- tmp %>%
      mutate(gen=gsub("_.*","",variable)) %>%
      mutate(Adjusted = ifelse(grepl("adj",variable),"adjusted","non-adjusted") )
    
    ggplot(plottingDF, aes(y=gen,x=value,fill=Adjusted)) +
      geom_density_ridges(color="darkslategrey",alpha=0.8)+
      scale_fill_manual(values = cols[c(2,6)])+
      theme_bw()+
      scale_x_log10()+
      xlab("Biomarker Value")+
      ylab("Biomarker")
  }
  output$densityplot <- renderPlot({
    print(density())
  })
#############################################################################
## Download BRINDA adjusted table
  output$downloadRes <- downloadHandler(
    filename = function() {
      paste("BRINDA_Results", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(brinda(), file, row.names = FALSE)
    })
#############################################################################
## Download BRINDA adjusted barplot
  output$downloadAdjBar <- downloadHandler(
    filename = "BRINDA_adjustment_barplot.png",
    content = function(file) {
      png(file)
      print(adjBarPlot())
      dev.off()
    })  
#############################################################################
## Download BRINDA adjusted barplot
  output$downloadDefBar <- downloadHandler(
    filename = "BRINDA_deficiency_barplot.png",
    content = function(file) {
      png(file)
      print(defBarPlot())
      dev.off()
    }) 
#############################################################################
## Download BRINDA adjusted density plot
  output$downloadDensity <- downloadHandler(
    filename = "BRINDA_densityplot.png",
    content = function(file) {
      png(file)
      print(density())
      dev.off()
    }) 
#############################################################################
## Download BRINDA adjusted table
  output$downloadRes2 <- downloadHandler(
    filename = function() {
      paste("BRINDA_Results", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(brinda(), file, row.names = FALSE)
    })
#############################################################################
## Create a stats object
  stats <- function(){
    rbp <- imported$data() %>%
      select(as.character(input$rbp)) %>%
      mutate(across(.cols=1, .fns=as.numeric)) %>%
      summarise(
        Biomarker="Retinol Binding Protein",
        `Variable Name` =colnames(.),
        N=length(.[!is.na(.)]),
        Minimum=min(.[!is.na(.)],na.rm = T),
        Mean=mean(.[!is.na(.)],na.rm=T),
        `Geometric Mean`=exp(mean(log(.[!is.na(.)]),na.rm=T)),
        Median=median(.[!is.na(.)],na.rm = T),
        Max=max(.[!is.na(.)],na.rm=T))
    sr <- imported$data() %>%
      select(as.character(input$rt)) %>%
      mutate(across(.cols=1, .fns=as.numeric)) %>%
      summarise(
        Biomarker="Retinol",
        `Variable Name` =colnames(.),
        N=length(.[!is.na(.)]),
        Minimum=min(.[!is.na(.)],na.rm = T),
        Mean=mean(.[!is.na(.)],na.rm=T),
        `Geometric Mean`=exp(mean(log(.[!is.na(.)]),na.rm=T)),
        Median=median(.[!is.na(.)],na.rm = T),
        Max=max(.[!is.na(.)],na.rm=T))
    sf <- imported$data() %>%
      select(as.character(input$ft)) %>%
      mutate(across(.cols=1, .fns=as.numeric)) %>%
      summarise(
        Biomarker="Ferritin",
        `Variable Name` =colnames(.),
        N=length(.[!is.na(.)]),
        Minimum=min(.[!is.na(.)],na.rm = T),
        Mean=mean(.[!is.na(.)],na.rm=T),
        `Geometric Mean`=exp(mean(log(.[!is.na(.)]),na.rm=T)),
        Median=median(.[!is.na(.)],na.rm = T),
        Max=max(.[!is.na(.)],na.rm=T))
    stfr <- imported$data() %>%
      select(as.character(input$stfr)) %>%
      mutate(across(.cols=1, .fns=as.numeric)) %>%
      summarise(
        Biomarker="Soluble Transferrin Receptor",
        `Variable Name` =colnames(.),
        N=length(.[!is.na(.)]),
        Minimum=min(.[!is.na(.)],na.rm = T),
        Mean=mean(.[!is.na(.)],na.rm=T),
        `Geometric Mean`=exp(mean(log(.[!is.na(.)]),na.rm=T)),
        Median=median(.[!is.na(.)],na.rm = T),
        Max=max(.[!is.na(.)],na.rm=T))
    zn <- imported$data() %>%
      select(as.character(input$zn)) %>%
      mutate(across(.cols=1, .fns=as.numeric)) %>%
      summarise(
        Biomarker="Zinc",
        `Variable Name` =colnames(.),
        N=length(.[!is.na(.)]),
        Minimum=min(.[!is.na(.)],na.rm = T),
        Mean=mean(.[!is.na(.)],na.rm=T),
        `Geometric Mean`=exp(mean(log(.[!is.na(.)]),na.rm=T)),
        Median=median(.[!is.na(.)],na.rm = T),
        Max=max(.[!is.na(.)],na.rm=T))
    agp <- imported$data() %>%
      select(as.character(input$agp)) %>%
      mutate(across(.cols=1, .fns=as.numeric)) %>%
      summarise(
        Biomarker="AGP",
        `Variable Name` =colnames(.),
        N=length(.[!is.na(.)]),
        Minimum=min(.[!is.na(.)],na.rm = T),
        Mean=mean(.[!is.na(.)],na.rm=T),
        `Geometric Mean`=exp(mean(log(.[!is.na(.)]),na.rm=T)),
        Median=median(.[!is.na(.)],na.rm = T),
        Max=max(.[!is.na(.)],na.rm=T))
    crp <- imported$data() %>%
      select(as.character(input$crp)) %>%
      mutate(across(.cols=1, .fns=as.numeric)) %>%
      summarise(
        Biomarker="CRP",
        `Variable Name` =colnames(.),
        N=length(.[!is.na(.)]),
        Minimum=min(.[!is.na(.)],na.rm = T),
        Mean=mean(.[!is.na(.)],na.rm=T),
        `Geometric Mean`=exp(mean(log(.[!is.na(.)]),na.rm=T)),
        Median=median(.[!is.na(.)],na.rm = T),
        Max=max(.[!is.na(.)],na.rm=T))
    df <- rbind.data.frame(
      rbp,
      sr,
      sf,
      stfr,
      zn,
      agp,
      crp
    )
    df <- cbind(df[,1:2],apply(df[,3:8],2,function(x){round(x,digits = 2)}))
    return(df)
  }
#############################################################################
## Download BRINDA adjustment Report

  output$report <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
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
        PDF = pdf_document(), HTML = html_document(), Word = word_document()),
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
                      zn_morn_fast_C = input$zn_morn_fast_C,
                      zn_morn_nonfast_C = input$zn_morn_nonfast_C ,
                      zn_after_nonfast_C = input$zn_after_nonfast_C ,
                      initCut = cutoffBarPlot(),
                      refAgp = input$refAgp,
                      refCrp = input$refCrp,
                      stats = stats(),
                      adjBar = adjBarPlot(),
                      defBar = defBarPlot(),
                      density = density()))
      file.rename(out, file)
    }
  )
}
###########################################################
## Shiny Server and UI Call
shinyApp(ui, server)
###########################################################
