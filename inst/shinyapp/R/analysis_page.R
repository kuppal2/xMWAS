library(shiny)

source("R/input.R")
source("R/preparation.R")
source("R/analysis.R")
source("R/graphopt.R")
source("R/communitydetection.R")
source("R/additionaloption.R")

# Define UI for data upload app ----

analysis_page <- fluidPage(
  
  # Make the Layout of parameter ----
  column(12,style="padding-top:10px;",navlistPanel(
    "Input Files",
    tabPanel("Choose Files (see help and support)",input),
    "Parameter Settings",
    tabPanel("1. Data preparation and filtering",preparation),
    tabPanel("2. Integration and association analysis", analysis),
    tabPanel("3. Centrality analysis",communitydetection),	
    tabPanel("4. Graphical options",graphopt),
    widths = c(3, 9)
  )),
  column(12,mainPanel(
    style='padding-left:0px;',
    tags$style(type='text/css', '#nText2 {background-color: silver; color: red;font-size: 12px}'),
    tags$style(type='text/css', '#nText {background-color: silver; color: blue;font-size: 12px}'),
    
    verbatimTextOutput("nText2"),
    verbatimTextOutput("nText"),
    
    actionButton("go","Start processing",icon=icon("play-circle")),
    downloadButton("downloadData", label = "Download results")
  )),
  
  column(12,tags$div(headerPanel(h4("Output")))),
  uiOutput("siderbar"),
  mainPanel(
    #htmlOutput('pdfviewer')
    #tags$iframe(style="height:600px; width:100%", src="Multiome_Network_corthresh0.8.pdf")
    imageOutput("myImage",width="400px",height="400px",inline=TRUE)

  )
)
