source("ui/styles/button.R")
ButtonStyle <- ButtonStyle
FooterNavigationButtonStyle <- FooterNavigationButtonStyle

source("ui/styles/page.R")
MainBoxTitleStyle <- MainBoxTitleStyle
MainBoxWidth <- MainBoxWidth



fluidPage(
  fluidRow(
    box(
      title=div(icon("table"),"Data Table", style = MainBoxTitleStyle),
      width = MainBoxWidth,
      color="#301014",
      status = "primary", solidHeader = TRUE,
      actionButton("importData",
                   "Step 1: Import Data",
                   icon = icon("cloud-upload"),
                   style=ButtonStyle),
      br(""),
      dataTableOutput("table")
    )
  ),
  fluidRow(
    div(
      style=FooterNavigationButtonStyle,
      actionButton("clickNextOnDataImportPage",
                   "Next",
                   icon = icon("forward"), 
                   style=ButtonStyle))
  )
)
