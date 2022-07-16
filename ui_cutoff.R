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
            width = 4,
            br(),
            div(
              textInput("rbpC","Retinol Binding Protein Cutoff",value = 0),
              style="display:inline-block;vertical-align:top;width:250px;"),
            div(
              verbatimTextOutput("rbpUnitC"),
              style="display:inline-block;vertical-align:top;width:80px;padding-top:25px;"),
            br(),
            div(
              textInput("rtC","Retinol Cutoff",value = 0),
              style="display:inline-block;vertical-align:top;width:250px;"),
            div(
              verbatimTextOutput("rtUnitC"),
              style="display:inline-block;vertical-align:top;width:80px;padding-top:25px;"),
            br(),
            div(
              textInput("ftC","Ferritin Cutoff" ,value = 0),
              style="display:inline-block;vertical-align:top;width:250px;"),
            div(
              verbatimTextOutput("ftUnitC"),
              style="display:inline-block;vertical-align:top;width:80px;padding-top:25px;"),
            br(),
            div(
              textInput("stfrC","Soluble Transferrin Receptor Cutoff",value = 0),
              style="display:inline-block;vertical-align:top;width:250px;"),
            div(
              verbatimTextOutput("stfrUnitC"),
              style="display:inline-block;vertical-align:top;width:80px;padding-top:25px;"),
            br(),
            ##############
            div(
              textInput("zn_morn_fast_C","Zinc Morning/Fasting Cutoff",value = 0),
              style="display:inline-block;vertical-align:top;width:250px;"),
            div(
              verbatimTextOutput("zn_morn_fast_UnitC"),
              style="display:inline-block;vertical-align:top;width:80px;padding-top:25px;"),
            br(),
            div(
              textInput("zn_morn_nonfast_C","Zinc Morning/Non-Fasting Cutoff",value = 0),
              style="display:inline-block;vertical-align:top;width:250px;"),
            div(
              verbatimTextOutput("zn_morn_nonfast_UnitC"),
              style="display:inline-block;vertical-align:top;width:80px;padding-top:25px;"),
            br(),
            div(
              textInput("zn_after_nonfast_C","Zinc Afternoon/Non-Fasting Cutoff",value = 0),
              style="display:inline-block;vertical-align:top;width:250px;"),
            div(
              verbatimTextOutput("zn_after_nonfast_UnitC"),
              style="display:inline-block;vertical-align:top;width:80px;padding-top:25px;"),
            br(),
            ############
            actionButton("setCutoff",
                         "Set Cutoff",
                         icon = icon("check"),
                         style="background-color: #301014;color:white;")
          ),
          column(
            width = 6,
            br(),
            h4("Suggested Biomarker Cutoffs"),
            tableOutput("defTbl"),
            h4("Suggested Zinc Cutoffs"),
            tableOutput("znDefTbl")
          )
        ),
        tabPanel(
          title =div(icon("chart-bar"),"Bar Plot"),
          plotOutput("cutoffBar")
        )
      )
    )
  ),
  fluidRow(
    div(
      style="display:inline-block;
          float:left;",
      actionButton("prevSelect",
                   "Previous",
                   icon = icon("backward"), 
                   style="background-color: #301014;color:white;")),
    div(
      style="display:inline-block;
            float:right;",
      actionButton("nextBrinda",
                   "Next",
                   icon = icon("forward"), 
                   style="background-color: #301014;color:white;"))
  )
)