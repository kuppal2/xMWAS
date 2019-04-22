library(shiny)

graphopt<-fluidRow(
  column(12,tags$div(
         column(6,numericInput("label_cex", "Size of the Labels:", 0.25, min = 0, max = 100))
  )),
  column(12,tags$div(
         column(6,numericInput("vertex_size", "Size of the Nodes:", 7, min = 5, max = 100)),
         column(6,numericInput("seednum", "Seed for Random Number Generator:", 100, min = 1, max = 1000))
  )),
  column(12,tags$div(
         column(8,numericInput("max_connections", "Maximum number of associations to include in the network (any numeric value >0 or -1 to use all):", -1, width='400px'))
  )),
  column(12,tags$div(column(6,radioButtons("use_X_reference", "Use dataset A as reference?", inline=TRUE,c(True = "TRUE",False = "FALSE"),selected = "FALSE"))  
  ))
)
