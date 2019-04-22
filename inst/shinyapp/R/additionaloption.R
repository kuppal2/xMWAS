library(shiny)

additionaloption<-fluidRow(
  column(12,tags$div(
    column(6,radioButtons("use_X_reference", "Use dataset A as reference?", inline=TRUE,c(True = "TRUE",False = "FALSE"),selected = "FALSE")),
    column(6,radioButtons("interactive", "Generate an interactive network?", inline=TRUE,c(True = "TRUE",False = "FALSE"),selected = "FALSE"))
  )),
  column(12,tags$div(
    column(6,radioButtons("removeRda", "Remove the intermediate files?", inline=TRUE,c(True = "TRUE",False = "FALSE"),selected = "TRUE"))
  ))
)