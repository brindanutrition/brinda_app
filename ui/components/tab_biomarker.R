source("ui/styles/button.R")
ButtonStyle <- ButtonStyle
FooterNavigationButtonStyle <- FooterNavigationButtonStyle

source("ui/styles/dropdown.R")
MediumDropDownDivStyle <- MediumDropDownDivStyle
LargeDropDownDivStyle <- LargeDropDownDivStyle

source("ui/styles/page.R")
MainBoxTitleStyle <- MainBoxTitleStyle
MainBoxWidth <- MainBoxWidth

source("ui/styles/plot.R")
SmallPlotDivStyle <- SmallPlotDivStyle


fluidPage(
  fluidRow(
    box(
      title = div(icon("list"),"Select Biomarkers", style = MainBoxTitleStyle),
      width = MainBoxWidth,
      status = "primary",
      solidHeader = TRUE,
      #######################
      div(
        selectInput("ft","Ferritin",choices = ""),
        style=SmallDropDownDivStyle),
      div(
        selectInput("ftU","Unit",choices=c("","\u03BCg/L","ng/mL")),
        style=SmallDropDownDivStyle),
      div(
        plotOutput("ftPlot",height = 115,width = 400),
        style=SmallPlotDivStyle),
      br(),
      div(
        selectInput("stfr","Soluble Transferrin Receptor",choices = ""),
        style=SmallDropDownDivStyle),
      div(
        selectInput("stfrU","Unit",choices=c("","mg/L")),
        style=SmallDropDownDivStyle),
      div(
        plotOutput("stfrPlot",height = 115,width = 400),
        style=SmallPlotDivStyle),
      br(),
      div(
        selectInput("rt","Retinol",choices = ""),
        style=SmallDropDownDivStyle),
      div(
        selectInput("rtU","Unit",choices=c("","\u03BCmol/L","\u03BCg/dL")),
        style=SmallDropDownDivStyle),
      div(
        plotOutput("rtPlot",height = 115,width = 400),
        style=SmallPlotDivStyle),
      br(),
      div(
        selectInput("rbp","Retinol Binding Protein",choices = ""),
        style=SmallDropDownDivStyle),
      div(
        selectInput("rbpU","Unit",choices=c("","\u03BCmol/L","\u03BCg/dL")),
        style=SmallDropDownDivStyle),
      div(
        plotOutput("rbpPlot",height = 115,width = 400),
        style=SmallPlotDivStyle),
      br(),
      div(
        selectInput("zn","Zinc",choices = ""),
        style=SmallDropDownDivStyle),
      div(
        selectInput("znU","Unit",choices=c("","\u03BCg/L","\u03BCmol/L")),
        style=SmallDropDownDivStyle),
      div(
        plotOutput("znPlot",height = 115,width = 400),
        style=SmallPlotDivStyle),
      br(),
      div(
        selectInput("agp","AGP *",choices = ""),
        style=SmallDropDownDivStyle),
      div(
        selectInput("agpU","Unit *",choices=c("","g/L")),
        style=SmallDropDownDivStyle),
      div(
        plotOutput("agpPlot",height = 115,width = 400),
        style=SmallPlotDivStyle),
      br(),
      div(
        selectInput("crp","CRP *",choices = ""),
        style=SmallDropDownDivStyle),
      div(
        selectInput("crpU","Unit *",choices=c("","mg/L")),
        style=SmallDropDownDivStyle),
      div(
        plotOutput("crpPlot",height = 115,width = 400),
        style=SmallPlotDivStyle),
      br(),
      #######################
      div(
        selectInput("fastingVar",
                    "What is the general fasting status of your data? *",
                    choices = c("fasted","non-fasted","unknown")),
        style=LargeDropDownDivStyle
      ),
      br(),
      div(
        selectInput("timeVar",
                    "What is the general time of blood draw for your data? *",
                    choices = c("morning",
                                "afternoon",
                                "evening",
                                "unknown")),
        style=LargeDropDownDivStyle
      ),
      br(),
      div(
        selectInput("pop","Population *",choices = c("",
                                                     "Women of Reproductive Age", 
                                                     "Preschool-age children", "Other (non-WRA, non-PSC population group)",
                                                     "User-defined AGP and CRP cutoffs")),
        style=MediumDropDownDivStyle),
      br(),
      uiOutput("manAgp"),
      uiOutput("manCrp"),
      br(),
      br()
    )
  ),
  fluidRow(
    div(
      style=FooterNavigationButtonStyle,
      actionButton("clickPrevOnBiomarkerPage",
                   "Previous",
                   icon = icon("backward"), 
                   style=ButtonStyle)),
    div(
      style=FooterNavigationButtonStyle,
      actionButton("clickNextOnBiomarkerPage",
                   "Next",
                   icon = icon("forward"), 
                   style=ButtonStyle))
  )
)
