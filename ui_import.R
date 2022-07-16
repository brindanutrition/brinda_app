fluidPage(
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
  ),
  fluidRow(
    div(
      style="display:inline-block;
            float:right;",
      actionButton("nextSelect",
                   "Next",
                   icon = icon("forward"), 
                   style="background-color: #301014;color:white;"))
  )
)
