library(shiny)
library(ggplot2)
library(datamods)
library(shinydashboard)
library(ggplot2)
library(RColorBrewer)
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
    # sidebarMenu(
    #   id="sider",
    #   menuItem("Step 1: Import Data",
    #     tabName = "import",
    #     icon = icon("cloud-upload"),
    #     badgeLabel = "Step 1",
    #     badgeColor = "yellow"
    #   ),
    #   menuItemOutput("step2"),
    #   menuItemOutput("step3"),
    #   menuItemOutput("step4"),
    #   menuItemOutput("step5")
    # )
    sidebarMenuOutput("sidebarmenu")
    ),
#############################################################################
## body
  dashboardBody(
    use_theme(mytheme),
    tabItems(
#############################################################################
## Import Data
      tabItem(
        tabName = "import",
        fluidRow(
          box(
            title=div(icon("table"),"Data Table"),
            width = 10,
            color="#301014",
            status = "primary", solidHeader = TRUE,
            actionButton("importData",
                         "Step 1: Import Data",
                         icon = icon("cloud-upload"),
                         style="background-color: #301014;color:white;"),
            br(""),
            dataTableOutput("table")
          )
        )
      ),
#############################################################################
## Select Markers
      tabItem(
        tabName = "select",
        box(
          title = div(icon("list"),"Select Markers"),
          width = 4,
          status = "primary",
          solidHeader = TRUE,
          selectInput("rbp","Retinol Binding Protein",choices = NULL),
          selectInput("rt","Retinol",choices = NULL),
          selectInput("ft","Ferritin",choices = NULL),
          selectInput("stfr","Soluble Transferrin Receptor",choices = NULL),
          selectInput("zn","Serum Zinc",choices = NULL),
          selectInput("agp","AGP",choices = NULL),
          selectInput("crp","CRP",choices = NULL),
          selectInput("pop","Population",choices = c("","WRA", "PSC", "Other","Manual")),
          uiOutput("manAgp"),
          uiOutput("manCrp")
        ),
        box(
          title = div(icon("check"),"Set Markers"),
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          actionButton("setMarkers",
                       "Set Markers",
                       icon = icon("check"),
                       style="background-color: #301014;color:white;"),
          br(""),
          plotOutput("rbpPlot",height = 115),
          plotOutput("rtPlot",height = 115),
          plotOutput("ftPlot",height = 115),
          plotOutput("stfrPlot",height = 115),
          plotOutput("znPlot",height = 115),
        )
      ),
#############################################################################
## Select Cutoff
      tabItem(
        tabName = "cutoff",
        box(
          title = div(icon("filter"),"Optional: Apply Cutoff"),
          width = 4,
          status = "primary",
          solidHeader = TRUE,
          textInput("rbpC","Retinol Binding Protein Cutoff",value = 0),
          textInput("rtC","Retinol Cutoff",value = 0),
          textInput("ftC","Ferritin Cutoff" ,value = 0),
          textInput("stfrC","Soluble Transferrin Receptor Cutoff",value = 0),
          textInput("znC","Serum Zinc Cutoff",value = 0)
        ),
        box(
          title = div(icon("filter"),"Set Cutoff"),
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          actionButton("setCutoff",
                       "Set Cutoff",
                       icon = icon("check"),
                       style="background-color: #301014;color:white;"),
          br(""),
          plotOutput("rbpPlotC",height = 115),
          plotOutput("rtPlotC",height = 115),
          plotOutput("ftPlotC",height = 115),
          plotOutput("stfrPlotC",height = 115),
          plotOutput("znPlotC",height = 115)
        )
      ),
#############################################################################
## Run BRINDA Adjustment
      tabItem(
        tabName = "brinda",
        fluidRow(
          box(
            title = div(icon("calculator"),"BRINDA"),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            tabBox(
              width = 12,
              tabPanel(
                title = div(icon("calculator"),"BRINDA"),
                actionButton("applyBrinda",
                             "Apply BRINDA Adjustment",
                             icon = icon("check"),
                             style="background-color: #301014;color:white;"),
                dataTableOutput("brindaTbl"),
                downloadButton("downloadRes","Download",
                               style="background-color: #301014;color:white;align:right")
              ),
              tabPanel(
                title = div(icon("chart-bar"),"Bar Plot"),
                plotOutput("barplot"),
                downloadButton("downloadBar","Download",
                               style="background-color: #301014;color:white;align:right")
              ),
              tabPanel(
                title = div(icon("chart-area"),"Density Plot"),
                plotOutput("densityplot"),
                downloadButton("downloadDensity","Download",
                               style="background-color: #301014;color:white;align:right")
              )
              )))
      ),
#############################################################################
## Generate Report
      tabItem(
        tabName = "report",
        box( 
          title = div(icon("file"),"Generate Report"),
          width = 4,
          status = "primary",
           solidHeader = TRUE,
          radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                       inline = TRUE),
          downloadButton("report","Generate Report",
                         icon = icon("play"),
                         style="background-color: #301014;color:white;")
          # downloadButton("report", "Generate report")
          )
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
    brks <- quantile(df[,sapply(df,is.numeric)], 
                     probs = seq(.05, .95, .05),
                     na.rm = TRUE)
    clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
    
    datatable(df,options = list(scrollX = T)) %>% 
      formatStyle(names(df)[sapply(df,is.numeric)],
                  backgroundColor = styleInterval(brks, clrs))
  })
#############################################################################
## Update Sidebar depending on what 
## step you are on
# observeEvent(input$importData,{
#   req(input$importData)
#   output$step2 <- renderMenu({
#     menuItem("Step 2: Select Markers",
#              tabName = "select",
#              icon = icon("list"),
#              badgeLabel = "Step 2",
#              badgeColor = "orange"
#     )
#   })
# })
#   observeEvent(input$setMarkers,{
#     req(input$setMarkers)
#     output$step3 <- renderMenu({
#       menuItem("Step 3: Optional: Apply Cutoff",
#                tabName = "cutoff",
#                icon = icon("filter"),
#                badgeLabel = "Step 3",
#                badgeColor = "maroon"
#       )
#     })
#   })
#   observeEvent(input$setCutoff,{
#     req(input$setCutoff)
#     output$step4 <- renderMenu({
#       menuItem("Step 4: Run BRINDA Adjustment",
#                tabName = "brinda",
#                icon =  icon("calculator"),
#                badgeLabel = "Step 4",
#                badgeColor = "maroon"
#       )
#     })
#   })
#   observeEvent(input$applyBrinda,{
#     req(input$applyBrinda)
#     output$step5 <- renderMenu({
#       menuItem("Step 5: Report",
#                tabName = "report",
#                icon =  icon("file"),
#                badgeLabel = "Step 5",
#                badgeColor = "fuchsia"
#       )
#     })
#   })

observe({
  observeEvent(1==1,{
    output$sidebarmenu <- renderMenu({
      sidebarMenu(
        menuItem("Step 1: Import Data",
                 tabName = "import",
                 icon = icon("cloud-upload"),
                 badgeLabel = "Step 1",
                 badgeColor = "yellow",
                 selected = T),
        menuItem("Step 2: Select Markers",
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
        menuItem("Step 1: Import Data",
                 tabName = "import",
                 icon = icon("cloud-upload"),
                 badgeLabel = "Step 1",
                 badgeColor = "yellow",
                 selected = T),
        menuItem("Step 2: Select Markers",
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
        menuItem("Step 1: Import Data",
                 tabName = "import",
                 icon = icon("cloud-upload"),
                 badgeLabel = "Step 1",
                 badgeColor = "yellow"),
        menuItem("Step 2: Select Markers",
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
        menuItem("Step 1: Import Data",
                 tabName = "import",
                 icon = icon("cloud-upload"),
                 badgeLabel = "Step 1",
                 badgeColor = "yellow"),
        menuItem("Step 2: Select Markers",
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
        menuItem("Step 1: Import Data",
                 tabName = "import",
                 icon = icon("cloud-upload"),
                 badgeLabel = "Step 1",
                 badgeColor = "yellow"),
        menuItem("Step 2: Select Markers",
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
## Updates Column Names Per User's Data
## Dynamically removes options based on 
## user's previous choice as to not 
## select the same column twice
  observe({
    updateSelectInput(session, "rbp",
                      label = "Retinol Binding Protein",
                      choices = c("",names(imported$data())[!(names(imported$data()) %in% c(input$rt,input$ft,input$stfr,input$zn,input$agp,input$crp))]),
                      selected = input$rbp)
    updateSelectInput(session, "rt",
                      label = "Retinol",
                      choices = c("",names(imported$data())[!(names(imported$data()) %in% c(input$rbp,input$ft,input$stfr,input$zn,input$agp,input$crp))]),
                      selected = input$rt)
    updateSelectInput(session, "ft",
                      label = "Ferritin",
                      choices = c("",names(imported$data())[!(names(imported$data()) %in% c(input$rt,input$rbp,input$stfr,input$zn,input$agp,input$crp))]),
                      selected = input$ft)
    updateSelectInput(session, "stfr",
                      label = "Soluble Transferrin Receptor",
                      choices = c("",names(imported$data())[!(names(imported$data()) %in% c(input$rt,input$ft,input$rbp,input$zn,input$agp,input$crp))]),
                      selected = input$stfr)
    updateSelectInput(session, "zn",
                      label = "Serum Zinc",
                      choices = c("",names(imported$data())[!(names(imported$data()) %in% c(input$rt,input$ft,input$stfr,input$rbp,input$agp,input$crp))]),
                      selected = input$zn)
    updateSelectInput(session, "agp",
                      label = "AGP",
                      choices = c("",names(imported$data())[!(names(imported$data()) %in% c(input$rt,input$ft,input$stfr,input$zn,input$rbp,input$crp))]),
                      selected = input$agp)
    updateSelectInput(session, "crp",
                      label = "CRP",
                      choices = c("",names(imported$data())[!(names(imported$data()) %in% c(input$rt,input$ft,input$stfr,input$zn,input$agp,input$rbp))]),
                      selected = input$crp)
  })
#############################################################################
## Density Plots to be generated
## each time the user selects what 
## columns in their data are the marker columns
  output$rbpPlot <- renderPlot({
    req(input$rbp)
    library(ggplot2)
    library(RColorBrewer)
    cols <- brewer.pal(10,"Set3")
    ggplot(imported$data(), aes(x=imported$data()[[as.character(input$rbp)]])) +
      geom_density(fill=cols[1],color="darkslategrey")+
      theme_minimal()+
      ggtitle("Retinol Binding Protein")+
      ylab("")+
      xlab("")
  })
  output$rtPlot <- renderPlot({
    req(input$rt)
    library(ggplot2)
    library(RColorBrewer)
    cols <- brewer.pal(10,"Set3")
    ggplot(imported$data(), aes(x=imported$data()[[as.character(input$rt)]])) +
      geom_density(fill=cols[2],color="darkslategrey")+
      theme_minimal()+
      ggtitle("Retinol")+
      ylab("")+
      xlab("")
  })
  output$ftPlot <- renderPlot({
    req(input$ft)
    library(ggplot2)
    library(RColorBrewer)
    cols <- brewer.pal(10,"Set3")
    ggplot(imported$data(), aes(x=imported$data()[[as.character(input$ft)]])) +
      geom_density(fill=cols[3],color="darkslategrey")+
      theme_minimal()+
      ggtitle("Ferritin")+
      ylab("")+
      xlab("")
  })
  output$stfrPlot <- renderPlot({
    req(input$stfr)
    library(ggplot2)
    library(RColorBrewer)
    cols <- brewer.pal(10,"Set3")
    ggplot(imported$data(), aes(x=imported$data()[[as.character(input$stfr)]])) +
      geom_density(fill=cols[5],color="darkslategrey")+
      theme_minimal()+
      ggtitle("Soluble Transferrin Receptor")+
      ylab("")+
      xlab("")
  })
  output$znPlot <- renderPlot({
    req(input$zn)
    library(ggplot2)
    library(RColorBrewer)
    cols <- brewer.pal(10,"Set3")
    ggplot(imported$data(), aes(x=imported$data()[[as.character(input$zn)]])) +
      geom_density(fill=cols[5],color="darkslategrey")+
      theme_minimal()+
      ggtitle("Zinc")+
      ylab("")+
      xlab("")
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
## Plot cutoff density plots
  output$rbpPlotC <- renderPlot({
    req(input$rbp)
    library(ggplot2)
    library(RColorBrewer)
    cols <- brewer.pal(10,"Set3")
    ggplot(imported$data(), aes(x=imported$data()[[as.character(input$rbp)]])) +
      geom_density(fill=cols[1],color="darkslategrey")+
      theme_minimal()+
      ggtitle("Retinol Binding Protein")+
      ylab("")+
      xlab("")+
      geom_vline(xintercept = as.numeric(input$rbpC),color="red")
  })
  output$rtPlotC <- renderPlot({
    req(input$rt)
    library(ggplot2)
    library(RColorBrewer)
    cols <- brewer.pal(10,"Set3")
    ggplot(imported$data(), aes(x=imported$data()[[as.character(input$rt)]])) +
      geom_density(fill=cols[2],color="darkslategrey")+
      theme_minimal()+
      ggtitle("Retinol")+
      ylab("")+
      xlab("")+
      geom_vline(xintercept = as.numeric(input$rtC),color="red")
  })
  output$ftPlotC <- renderPlot({
    req(input$ft)
    library(ggplot2)
    library(RColorBrewer)
    cols <- brewer.pal(10,"Set3")
    ggplot(imported$data(), aes(x=imported$data()[[as.character(input$ft)]])) +
      geom_density(fill=cols[3],color="darkslategrey")+
      theme_minimal()+
      ggtitle("Ferritin")+
      ylab("")+
      xlab("")+
      geom_vline(xintercept = as.numeric(input$ftC),color="red")
  })
  output$stfrPlotC <- renderPlot({
    req(input$stfr)
    library(ggplot2)
    library(RColorBrewer)
    cols <- brewer.pal(10,"Set3")
    ggplot(imported$data(), aes(x=imported$data()[[as.character(input$stfr)]])) +
      geom_density(fill=cols[5],color="darkslategrey")+
      theme_minimal()+
      ggtitle("Soluble Transferrin Receptor")+
      ylab("")+
      xlab("")+
      geom_vline(xintercept = as.numeric(input$stfrC),color="red")
  })
  output$znPlotC <- renderPlot({
    req(input$zn)
    library(ggplot2)
    library(RColorBrewer)
    cols <- brewer.pal(10,"Set3")
    ggplot(imported$data(), aes(x=imported$data()[[as.character(input$zn)]])) +
      geom_density(fill=cols[5],color="darkslategrey")+
      theme_minimal()+
      ggtitle("Zinc")+
      ylab("")+
      xlab("")+
      geom_vline(xintercept = as.numeric(input$znC),color="red")
  })
################################################################################ Apply BRINDA adjustment
  ## make brinda variable available outside observe statement
  # brinda <- NULL
  # makeReactiveBinding("brinda")
  # 
  observe({
    req(input$applyBrinda)
    if (as.character(input$pop) == "WRA"){
      brinda <<- reactive({
        req(input$applyBrinda)
        req(input$pop)
        rbp <- input$rbp
        sr <- input$sr
        sf <- input$sf
        stfr <- input$stfr
        zinc <- input$zinc
        crp <- input$crp
        agp <- input$agp
        brinda <- BRINDA(
          dataset = imported$data(),
          retinol_binding_protein_varname = rbp,
          retinol_varname = sr,
          ferritin_varname = sf,
          soluble_transferrin_receptor_varname = stfr,
          zinc_varname = zinc,
          crp_varname = crp,
          agp_varname = agp,
          population_group = "WRA",
          output_format = full)
        cutoffs <- c(as.numeric(input$rbpC),
                     as.numeric(input$srC),
                     as.numeric(input$sfC),
                     as.numeric(input$stfrC),
                     as.numeric(input$zincC))
        inputs <- c(as.character(input$rbp),
                    as.character(input$sr),
                    as.character(input$sf),
                    as.character(input$stfr),
                    as.character(input$zinc))
        adjusted <- c("rbp_adj",
                      "sr_adj",
                      "sf_adj",
                      "stfr_adj",
                      "zn_adj")
        for(i in 1:length(inputs)){
          brinda[adjusted[i]] = ifelse(brinda[[inputs[i]]] < cutoffs[i],brinda[[inputs[i]]],brinda[[adjusted[i]]] )
        }
        brinda
      })
    }else if (as.character(input$pop) == "PSC"){
      brinda <<- reactive({
        req(input$applyBrinda)
        req(input$pop)
        rbp <- input$rbp
        sr <- input$sr
        sf <- input$sf
        stfr <- input$stfr
        zinc <- input$zinc
        crp <- input$crp
        agp <- input$agp
        brinda <- BRINDA(
          dataset = imported$data(),
          retinol_binding_protein_varname = rbp,
          retinol_varname = sr,
          ferritin_varname = sf,
          soluble_transferrin_receptor_varname = stfr,
          zinc_varname = zinc,
          crp_varname = crp,
          agp_varname = agp,
          population_group = "PSC",
          output_format = full)
        cutoffs <- c(as.numeric(input$rbpC),
                     as.numeric(input$srC),
                     as.numeric(input$sfC),
                     as.numeric(input$stfrC),
                     as.numeric(input$zincC))
        inputs <- c(as.character(input$rbp),
                    as.character(input$sr),
                    as.character(input$sf),
                    as.character(input$stfr),
                    as.character(input$zinc))
        adjusted <- c("rbp_adj",
                      "sr_adj",
                      "sf_adj",
                      "stfr_adj",
                      "zn_adj")
        for(i in 1:length(inputs)){
          brinda[adjusted[i]] = ifelse(brinda[[inputs[i]]] < cutoffs[i],brinda[[inputs[i]]],brinda[[adjusted[i]]] )
        }
        brinda
      })
    } else if (as.character(input$pop) == "Manual") {
      brinda <<- reactive({
        req(input$applyBrinda)
        req(input$pop)
        rbp <- input$rbp
        sr <- input$sr
        sf <- input$sf
        stfr <- input$stfr
        zinc <- input$zinc
        crp <- input$crp
        agp <- input$agp
        manAgp <- input$manAgp
        manCrp <- input$manCrp
        brinda <- BRINDA(
          dataset = imported$data(),
          retinol_binding_protein_varname = rbp,
          retinol_varname = sr,
          ferritin_varname = sf,
          soluble_transferrin_receptor_varname = stfr,
          zinc_varname = zinc,
          crp_varname = crp,
          agp_varname = agp,
          population_group = "MANUAL" ,
          agp_ref_value_manual = manAgp,
          crp_ref_value_manual = manCrp,
          output_format = full)
        cutoffs <- c(as.numeric(input$rbpC),
                     as.numeric(input$srC),
                     as.numeric(input$sfC),
                     as.numeric(input$stfrC),
                     as.numeric(input$zincC))
        inputs <- c(as.character(input$rbp),
                    as.character(input$sr),
                    as.character(input$sf),
                    as.character(input$stfr),
                    as.character(input$zinc))
        adjusted <- c("rbp_adj",
                      "sr_adj",
                      "sf_adj",
                      "stfr_adj",
                      "zn_adj")
        for(i in 1:length(inputs)){
          brinda[adjusted[i]] = ifelse(brinda[[inputs[i]]] < cutoffs[i],brinda[[inputs[i]]],brinda[[adjusted[i]]] )
        }
        brinda
      })
    } else if (as.character(input$pop) == "Other") {
      brinda <<- reactive({
        req(input$applyBrinda)
        req(input$pop)
        rbp <- input$rbp
        sr <- input$sr
        sf <- input$sf
        stfr <- input$stfr
        zinc <- input$zinc
        crp <- input$crp
        agp <- input$agp
        brinda <- BRINDA(
          dataset = imported$data(),
          retinol_binding_protein_varname = rbp,
          retinol_varname = sr,
          ferritin_varname = sf,
          soluble_transferrin_receptor_varname = stfr,
          zinc_varname = zinc,
          crp_varname = crp,
          agp_varname = agp,
          population_group = "OTHER",
          output_format = full)
        cutoffs <- c(as.numeric(input$rbpC),
                     as.numeric(input$srC),
                     as.numeric(input$sfC),
                     as.numeric(input$stfrC),
                     as.numeric(input$zincC))
        inputs <- c(as.character(input$rbp),
                    as.character(input$sr),
                    as.character(input$sf),
                    as.character(input$stfr),
                    as.character(input$zinc))
        adjusted <- c("rbp_adj",
                      "sr_adj",
                      "sf_adj",
                      "stfr_adj",
                      "zn_adj")
        for(i in 1:length(inputs)){
          brinda[adjusted[i]] = ifelse(brinda[[inputs[i]]] < cutoffs[i],brinda[[inputs[i]]],brinda[[adjusted[i]]] )
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
    brks <- quantile(df[,sapply(df,is.numeric)], 
                     probs = seq(.05, .95, .05),
                     na.rm = TRUE)
    clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
    
    datatable(df,options = list(scrollX = T)) %>% 
      formatStyle(names(df)[sapply(df,is.numeric)],
                  backgroundColor = styleInterval(brks, clrs))
  })
  ############################################################################
  ## Barplot of Adjusted/Unadjusted Values
  bar <- function(){
    df <- brinda()
    tmp=reshape2::melt(
      df[c(
        as.character(input$rbp),
        "rbp_adj", 
        as.character(input$rt),
        "sr_adj", 
        as.character(input$ft),
        "sf_adj", 
        as.character(input$stfr),
        "stfr_adj", 
        as.character(input$zn),
        "zn_adj")])
    tmp$variable <- gsub("zinc","zn",tmp$variable)
    plottingDF <- tmp %>%
      group_by(variable) %>%
      summarize(
        sum = sum(value,na.rm = T),
        se = sd(value,na.rm = T)/sqrt(n())
      ) %>%
      mutate(gen=gsub("_.*","",variable)) %>%
      mutate(Adjusted = grepl("adj",variable))
    
    cols <- brewer.pal(10,"Set3")
    ggplot(plottingDF, aes(x = gen, y= sum, fill = Adjusted)) +
      geom_bar(stat="identity", width=.5, position = "dodge") +
      theme_minimal()+
      scale_fill_manual(values = cols[3:4])+
      geom_errorbar(aes(ymin=sum-se, 
                        ymax=sum+se),
                    width=.2, # Width of the error bars
                    position=position_dodge(.5), 
                    color="firebrick") +
      xlab("Micronutrient") +
      ylab("Sum Per Micronutrient")+
      scale_y_log10()
  }

  output$barplot <- renderPlot({
    print(bar())
  })
  density <- function(){
    cols <- brewer.pal(10,"Set3")
    df <- brinda()
    tmp=reshape2::melt(
      df[c(
        as.character(input$rbp),
        "rbp_adj", 
        as.character(input$rt),
        "sr_adj", 
        as.character(input$ft),
        "sf_adj", 
        as.character(input$stfr),
        "stfr_adj", 
        as.character(input$zn),
        "zn_adj")])
    tmp$variable <- gsub("zinc","zn",tmp$variable)
    tmp$Adjusted = grepl("adj",tmp$variable)
    ggplot(tmp, aes(y=variable,x=value,fill=Adjusted)) +
      geom_density_ridges(color="darkslategrey")+
      scale_fill_manual(values = cols[3:4],
                        labels=c("Unadjusted","Adjusted"))+
      theme_minimal()+
      scale_x_log10()+
      xlab("Micronutrient Value")+
      ylab("Micronutrient")
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
  output$downloadBar <- downloadHandler(
    filename = "BRINDA_barplot.png",
    content = function(file) {
      png(file)
      print(bar())
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
        params = list(data = imported$data()))
      file.rename(out, file)
    }
  )
}
###########################################################
## Shiny Server and UI Call
shinyApp(ui, server)
###########################################################