source("ui/styles/button.R")
ButtonStyle <- ButtonStyle
FooterNavigationButtonStyle <- FooterNavigationButtonStyle

source("ui/styles/dropdown.R")
SmallDropDownDivStyle <- SmallDropDownDivStyle

source("ui/styles/page.R")
MainBoxTitleStyle <- MainBoxTitleStyle
MainBoxWidth <- MainBoxWidth


fluidPage(
  fluidRow(
    box(
      title = div(icon("calculator"),"BRINDA", style = MainBoxTitleStyle),
      width = MainBoxWidth,
      status = "primary",
      solidHeader = TRUE,
      tabBox(
        width = MainBoxWidth,
        tabPanel(
          title = div(icon("calculator"),"BRINDA"),
          div(
            selectInput("outputType",
                        "Output Type",
                        choices = c("",
                                    "only output adjusted values",
                                    "output adjusted and intermediate parameters")),
            style=SmallDropDownDivStyle
          ),
          br(),
          h3("BRINDA Adjustment Table"),
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
      style=FooterNavigationButtonStyle,
      actionButton("clickPrevOnBrindaPage",
                   "Previous",
                   icon = icon("backward"), 
                   style=ButtonStyle)),
    div(
      style=FooterNavigationButtonStyle,
      actionButton("clickNextOnBrindaPage",
                   "Next",
                   icon = icon("forward"), 
                   style=ButtonStyle))
  )
)
