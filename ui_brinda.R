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
          actionButton("applyBrinda",
                       "Apply BRINDA Adjustment",
                       icon = icon("check"),
                       style="background-color: #301014;color:white;"),
          dataTableOutput("brindaTbl"),
          downloadButton("downloadRes","Download",
                         style="background-color: #301014;color:white;align:right")),
        tabPanel(
          title = div(icon("chart-bar"),"Bar Plot"),
          plotOutput("barplot"),
          downloadButton("downloadBar","Download",
                         style="background-color: #301014;color:white;align:right")),
        tabPanel(
          title = div(icon("chart-area"),"Density Plot"),
          plotOutput("densityplot"),
          downloadButton("downloadDensity","Download",
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
