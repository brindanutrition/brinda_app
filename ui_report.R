fluidPage(
  fluidRow(
    box( 
      title = div(icon("file"),"Generate Report"),
      width = 4,
      status = "primary",
      solidHeader = TRUE,
      radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                   inline = TRUE),
      downloadButton("report","Generate Report",
                     icon = icon("play"),
                     style="background-color: #301014;color:white;"))
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
