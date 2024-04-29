source("ui/styles/button.R")
ButtonStyle <- ButtonStyle
FooterNavigationButtonStyle <- FooterNavigationButtonStyle

source("ui/styles/page.R")
MainBoxTitleStyle <- MainBoxTitleStyle
MainBoxWidth <- MainBoxWidth


TextInputColumnStyle <- "
  display:inline-block;
  vertical-align:top;
  width:250px;
  margin-left:15px;
"

SuggestedCutoffColumnStyle <- "
  display:inline-block;
  vertical-align:top;
  width:150px;
  margin-left:15px;
"

ApplySuggestedCutoffColumnStyle <- "
  display:inline-block;
  vertical-align:top;
  width:150px;
  margin-top:15px;
"


fluidPage(
  fluidRow(
    box(
      title = div(icon("filter"),"Optional: Apply Cutoff", style = MainBoxTitleStyle),
      width = MainBoxWidth,
      status = "primary",
      solidHeader = TRUE,
      tabBox(
        width = MainBoxWidth,
        tabPanel(
          title = div(icon("filter"),"Apply Cutoff"),
          column(
            width = 8,
            br(),
            div(
              div(
                textInput("rbpC","Retinol Binding Protein Cutoff",value = 0),
                style=TextInputColumnStyle),
              div(
                strong("Suggested Cutoff"),
                verbatimTextOutput("rbpSug"),
                style=SuggestedCutoffColumnStyle),
              div(
                style=ApplySuggestedCutoffColumnStyle,
                actionButton("applyRBPCutOff",
                    "Apply Suggestion",
                    icon = icon("copy"),
                    style=ButtonStyle)),
            ),
            br(),
            div(
              div(
                textInput("rtC","Retinol Cutoff",value = 0),
                style=TextInputColumnStyle),
              div(
                strong("Suggested Cutoff"),
                verbatimTextOutput("rtSug"),
                style=SuggestedCutoffColumnStyle),
              div(
                style=ApplySuggestedCutoffColumnStyle,
                actionButton("applyRTCutOff",
                    "Apply Suggestion",
                    icon = icon("copy"),
                    style=ButtonStyle)),
            ),
            br(),
            div(
              div(
                textInput("ftC","Ferritin Cutoff" ,value = 0),
                style=TextInputColumnStyle),
              div(
                strong("Suggested Cutoff"),
                verbatimTextOutput("ftSug"),
                style=SuggestedCutoffColumnStyle),
              div(
                style=ApplySuggestedCutoffColumnStyle,
                actionButton("applyFTCutOff",
                    "Apply Suggestion",
                    icon = icon("copy"),
                    style=ButtonStyle)),
            ),
            br(),
            div(
              div(
                textInput("stfrC","Soluble Transferrin Receptor Cutoff",value = 0),
                style=TextInputColumnStyle),
              div(
                strong("Suggested Cutoff"),
                verbatimTextOutput("stfrSug"),
                style=SuggestedCutoffColumnStyle),
              div(
                style=ApplySuggestedCutoffColumnStyle,
                actionButton("applySTFRCutOff",
                    "Apply Suggestion",
                    icon = icon("copy"),
                    style=ButtonStyle)),
            ),
            br(),
            ############## DELETE ME ################
            div(
              div(
                textInput("znC","Zinc Cutoff",value = 0),
                style=TextInputColumnStyle),
              div(
                strong("Suggested Cutoff"),
                verbatimTextOutput("znSug"),
                style=SuggestedCutoffColumnStyle),
              div(
                style=ApplySuggestedCutoffColumnStyle,
                actionButton("applyZNCutOff",
                    "Apply Suggestion",
                    icon = icon("copy"),
                    style=ButtonStyle)),
            ),
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
