source("ui/styles/button.R")
ButtonStyle <- ButtonStyle
FooterNavigationButtonStyle <- FooterNavigationButtonStyle


fluidPage(
  fluidRow(
    box(
      title = div(icon("filter"),"Optional: Apply Cutoff"),
      width = 10,
      status = "primary",
      solidHeader = TRUE,
      tabBox(
        width = 10,
        tabPanel(
          title = div(icon("filter"),"Apply Cutoff"),
          column(
            width = 8,
            br(),
            div(
              textInput("rbpC","Retinol Binding Protein Cutoff",value = 0),
              style="display:inline-block;vertical-align:top;width:250px;"),
            div(
              strong("Suggested Cutoff"),
              verbatimTextOutput("rbpSug"),
              style="display:inline-block;vertical-align:top;width:120px;"),
            br(),
            div(
              textInput("rtC","Retinol Cutoff",value = 0),
              style="display:inline-block;vertical-align:top;width:250px;"),
            div(
              strong("Suggested Cutoff"),
              verbatimTextOutput("rtSug"),
              style="display:inline-block;vertical-align:top;width:120px;"),
            br(),
            div(
              textInput("ftC","Ferritin Cutoff" ,value = 0),
              style="display:inline-block;vertical-align:top;width:250px;"),
            div(
              strong("Suggested Cutoff"),
              verbatimTextOutput("ftSug"),
              style="display:inline-block;vertical-align:top;width:120px;"),
            br(),
            div(
              textInput("stfrC","Soluble Transferrin Receptor Cutoff",value = 0),
              style="display:inline-block;vertical-align:top;width:250px;"),
            div(
              strong("Suggested Cutoff"),
              verbatimTextOutput("stfrSug"),
              style="display:inline-block;vertical-align:top;width:120px;"),
            br(),
            ##############
            div(
              textInput("znC","Zinc Cutoff",value = 0),
              style="display:inline-block;vertical-align:top;width:250px;"),
            div(
              strong("Suggested Cutoff"),
              verbatimTextOutput("znSug"),
              style="display:inline-block;vertical-align:top;width:;"),
            br()
          ),
          column(
            width = 1
          ),
          column(
            width = 1,
            br(),
            uiOutput("defImage")
          )
        ),
        tabPanel(
          title =div(icon("chart-bar"),"Non-Adjusted Deficiency Bar Plot"),
          plotOutput("cutoffBar"),
          downloadButton("downloadNonAdjBar","Download Non-Adjusted Deficiency Bar Plot",
                         style="background-color: #301014;color:white;align:right")
        )
      )
    )
  ),
  fluidRow(
    div(
      style=FooterNavigationButtonStyle,
      actionButton("clickPrevOnCutoffPage",
                   "Previous",
                   icon = icon("backward"), 
                   style=ButtonStyle)),
    div(
      style=FooterNavigationButtonStyle,
      actionButton("clickNextOnCutoffPage",
                   "Next",
                   icon = icon("forward"), 
                   style=ButtonStyle))
  )
)
