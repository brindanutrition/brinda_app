fluidPage(
  fluidRow(
    box(
      title = div(icon("calculator"),"BRINDA"),
      width = 10,
      status = "primary",
      solidHeader = TRUE,
      tabBox(
        width = 10,
        tabPanel(
          title = div(icon("calculator"),"BRINDA"),
          div(
            selectInput("outputType",
                        "Output Type",
                        choices = c("","simple","full")),
            style="display:inline-block;vertical-align:top;width:200px;"
          ),
          div(
            actionButton("applyBrinda",
                         "Apply BRINDA Adjustment",
                         icon = icon("check"),
                         style="background-color: #301014;color:white;"),
            style="display:inline-block;vertical-align:top;padding-top:25px;width:200px"
          ),
          dataTableOutput("brindaTbl"),
          downloadButton("downloadRes","Download BRINDA Adjusted Data",
                         style="background-color: #301014;color:white;align:right")),
        tabPanel(
          title = div(icon("chart-bar"),"Adjustment Bar Plot"),
          plotOutput("adjBarPlot"),
          downloadButton("downloadAdjBar","Download Adjustment Bar Plot",
                         style="background-color: #301014;color:white;align:right")),
        tabPanel(
          title = div(icon("chart-bar"),"Deficiency Bar Plot"),
          plotOutput("defBarPlot"),
          downloadButton("downloadDefBar","Download Deficiency Bar Plot",
                         style="background-color: #301014;color:white;align:right")),
        tabPanel(
          title = div(icon("chart-area"),"Density Plot"),
          plotOutput("densityplot"),
          downloadButton("downloadDensity","Download Density Plot",
                         style="background-color: #301014;color:white;align:right"))
      )
    )
  ),
  fluidRow(
    div(
      style="display:inline-block;
            float:left;",
      actionButton("prevCutoff",
                   "Previous",
                   icon = icon("backward"), 
                   style="background-color: #301014;color:white;")),
    div(
      style="display:inline-block;
            float:right;",
      actionButton("nextReport",
                   "Next",
                   icon = icon("forward"), 
                   style="background-color: #301014;color:white;"))
  )
)
