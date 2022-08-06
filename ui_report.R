fluidPage(
  fluidRow(
    box( 
      title = div(icon("file"),"Generate Report"),
      width = 10,
      status = "primary",
      solidHeader = TRUE,
      radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                   inline = TRUE),
      downloadButton("report","Generate Report",
                     icon = icon("play"),
                     style="background-color: #301014;color:white;"),
      br(),
      downloadButton("downloadRes2","Download BRINDA Adjusted Data",
                     style="background-color: #301014;color:white;align:right"))
  ),
  fluidRow(
    div(
      style="display:inline-block;float:left;",
      actionButton("prevBrinda",
                   "Previous",
                   icon = icon("backward"), 
                   style="background-color: #301014;color:white;"))
  )
)
