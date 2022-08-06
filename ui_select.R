
fluidPage(
  fluidRow(
    box(
      title = div(icon("list"),"Select Biomarkers"),
      width = 10,
      status = "primary",
      solidHeader = TRUE,
      div(
        selectInput("rbp","Retinol Binding Protein *",choices = NULL),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        selectInput("rbpU","Unit",choices=c("","umol/L","ug/L","mg/L","g/L")),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        plotOutput("rbpPlot",height = 115,width = 400),
        style="display:inline-block;vertical-align:top;"),
      br(),
      div(
        selectInput("rt","Retinol *",choices = NULL),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        selectInput("rtU","Unit",choices=c("","umol/L","ug/L","mg/L","g/L")),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        plotOutput("rtPlot",height = 115,width = 400),
        style="display:inline-block;vertical-align:top;"),
      br(),
      div(
        selectInput("ft","Ferritin *",choices = NULL),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        selectInput("ftU","Unit",choices=c("","umol/L","ug/L","mg/L","g/L")),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        plotOutput("ftPlot",height = 115,width = 400),
        style="display:inline-block;vertical-align:top;"),
      br(),
      div(
        selectInput("stfr","Soluble Transferrin Receptor *",choices = NULL),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        selectInput("stfrU","Unit",choices=c("","umol/L","ug/L","mg/L","g/L")),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        plotOutput("stfrPlot",height = 115,width = 400),
        style="display:inline-block;vertical-align:top;"),
      br(),
      div(
        selectInput("zn","Zinc *",choices = NULL),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        selectInput("znU","Unit",choices=c("","umol/L","ug/L","mg/L","g/L")),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        plotOutput("znPlot",height = 115,width = 400),
        style="display:inline-block;vertical-align:top;"),
      br(),
      div(
        selectInput("agp","AGP *",choices = NULL),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        selectInput("agpU","Unit *",choices=c("","umol/L","ug/L","mg/L","g/L")),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        plotOutput("agpPlot",height = 115,width = 400),
        style="display:inline-block;vertical-align:top;"),
      br(),
      div(
        selectInput("crp","CRP *",choices = NULL),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        selectInput("crpU","Unit *",choices=c("","umol/L","ug/L","mg/L","g/L")),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        plotOutput("crpPlot",height = 115,width = 400),
        style="display:inline-block;vertical-align:top;"),
      br(),
      div(
        selectInput("fasting","Fasting Status *",choices = NULL),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        selectInput("time","Time of Day *",choices = NULL),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        selectInput("pop","Population *",choices = c("","WRA", "PSC", "Other","Manual")),
        style="display:inline-block;vertical-align:top;width:200px;"),
      br(),
      uiOutput("manAgp"),
      uiOutput("manCrp"),
      actionButton("setMarkers",
                   "Set Biomarkers",
                   icon = icon("check"),
                   style="background-color: #301014;color:white;"),
      br(),
      br(),
      strong("* required")
    )
  ),
  fluidRow(
    div(
      style="display:inline-block;
            float:left;",
      actionButton("prevImport",
                   "Previous",
                   icon = icon("backward"), 
                   style="background-color: #301014;color:white;")),
    div(
      style="display:inline-block;
            float:right;",
      actionButton("nextCutoff",
                   "Next",
                   icon = icon("forward"), 
                   style="background-color: #301014;color:white;"))
  )
  
)
