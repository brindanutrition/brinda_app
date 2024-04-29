source("ui/styles/button.R")
ButtonStyle <- ButtonStyle
FooterNavigationButtonStyle <- FooterNavigationButtonStyle

source("ui/styles/page.R")
MainBoxTitleStyle <- MainBoxTitleStyle
MainBoxWidth <- MainBoxWidth


fluidPage(
  fluidRow(
    box( 
      title = div(icon("file"),"Generate Report", style = MainBoxTitleStyle),
      width = MainBoxWidth,
      status = "primary",
      solidHeader = TRUE,
      radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                   inline = TRUE),
      downloadButton("report","Generate Report",
                     icon = icon("play"),
                     style=ButtonStyle),
      br(),
      downloadButton("downloadRes2","Download BRINDA Adjusted Data",
                     style="background-color: #301014;color:white;align:right"))
  ),
  fluidRow(
    div(
      style=FooterNavigationButtonStyle,
      actionButton("clickPrevOnReportPage",
                   "Previous",
                   icon = icon("backward"), 
                   style=ButtonStyle))
  )
)
