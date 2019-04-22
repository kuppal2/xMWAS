library(shiny)

communitydetection<-fluidRow(
           column(6,selectInput("centrality_method","Method for centrality analysis:",c("eigenvector","betweenness","degree.count","degree.weight","closeness")))
)
