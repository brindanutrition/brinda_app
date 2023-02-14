fluidPage(
  fluidRow(
    box(
      title = div(icon("list"),"Select Biomarkers"),
      width = 10,
      status = "primary",
      solidHeader = TRUE,
      #######################
      div(
        selectInput("ft","Ferritin",choices = ""),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        selectInput("ftU","Unit",choices=c("","\u03BCg/L","ng/mL")),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        plotOutput("ftPlot",height = 115,width = 400),
        style="display:inline-block;vertical-align:top;"),
      br(),
      div(
        selectInput("stfr","Soluble Transferrin Receptor",choices = ""),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        selectInput("stfrU","Unit",choices=c("","mg/L")),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        plotOutput("stfrPlot",height = 115,width = 400),
        style="display:inline-block;vertical-align:top;"),
      br(),
      div(
        selectInput("rt","Retinol",choices = ""),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        selectInput("rtU","Unit",choices=c("","\u03BCmol/L","\u03BCg/dL")),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        plotOutput("rtPlot",height = 115,width = 400),
        style="display:inline-block;vertical-align:top;"),
      br(),
      div(
        selectInput("rbp","Retinol Binding Protein",choices = ""),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        selectInput("rbpU","Unit",choices=c("","\u03BCmol/L","\u03BCg/dL")),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        plotOutput("rbpPlot",height = 115,width = 400),
        style="display:inline-block;vertical-align:top;"),
      br(),
      div(
        selectInput("zn","Zinc",choices = ""),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        selectInput("znU","Unit",choices=c("","\u03BCg/L","\u03BCmol/L")),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        plotOutput("znPlot",height = 115,width = 400),
        style="display:inline-block;vertical-align:top;"),
      br(),
      div(
        selectInput("agp","AGP *",choices = ""),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        selectInput("agpU","Unit *",choices=c("","g/L")),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        plotOutput("agpPlot",height = 115,width = 400),
        style="display:inline-block;vertical-align:top;"),
      br(),
      div(
        selectInput("crp","CRP *",choices = ""),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        selectInput("crpU","Unit *",choices=c("","mg/L")),
        style="display:inline-block;vertical-align:top;width:200px;"),
      div(
        plotOutput("crpPlot",height = 115,width = 400),
        style="display:inline-block;vertical-align:top;"),
      br(),
      #######################
      div(
        selectInput("fastingVar",
                    "What is the general fasting status of your data? *",
                    choices = c("fasted","non-fasted","unknown"))
      ),
      br(),
      div(
        selectInput("timeVar",
                    "What is the general time of blood draw for your data? *",
                    choices = c("morning",
                                "afternoon",
                                "evening",
                                "unknown"))
      ),
      br(),
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
