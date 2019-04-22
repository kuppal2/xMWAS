library(shiny)
library(shinyBS)

input<-fluidRow(
  column(12,tags$div(column(width=6,  
         fileInput("datasetA", "Input file for dataset A ('.csv' or '.txt', 100MB limit)",
                   multiple = FALSE,
                   width="350px",
                   accept = c("text/csv",
                              "text/comma-separated-values,text/plain",
                              ".txt"))
  ),
  column(width=6,  
         textInput("Aname", "Name for dataset A:", value="",placeholder="datasetA")
  ))),
  column(12,tags$div(column(width=6,  
                  fileInput("datasetB", "Input file for dataset B ('.csv' or '.txt', 100MB limit)",
                            multiple = FALSE,
                            width="350px",
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".txt"))
  ),
  column(width=6,  
         textInput("Bname", "Name for dataset B:", value="",placeholder="datasetB")
  ))),
  uiOutput("input_dataC"),
  uiOutput("input_dataD"),
  column(12,tags$div(style="padding-left:18px;padding-top:0px;padding-bottom:0px;",tags$strong(tags$span(style="font-size:100%;padding-right:10px","Add more datasets:")),actionButton("add", label="", icon=icon("plus")),actionButton("delete", label="", icon=icon("minus")))),
  column(style="padding-top:0px;padding-bottom:0px;",12,tags$hr(style="border-top: 1px solid #000000;")),
  column(12,tags$div(column(width=6,  
         fileInput("class_labels_file", "Choose a class labels file ('.csv' or '.txt'):",
                   multiple = FALSE,
                   width="350px",
                   accept = c("text/csv",
                              "text/comma-separated-values,text/plain",
                              ".txt"))
  ),
          
  column(style="margin-top:23px;",width=6,actionButton("moreoptions", "More Options")),
  bsModal("more_options", "More options", "moreoptions", size = "large",
          tags$div(
                 width=12,
                 style='height:200px;',
                 column(width=6,textInput("outloc", "Output folder name:","",placeholder="Default: xwasresults")),
                 column(6,radioButtons("pairedanalysis", "Are there repeated measurements?", inline=FALSE,c("True - Paired (repeated measures)" = "TRUE","False - Unpaired (case-control & multiclass)" = "FALSE"),selected = "FALSE")),
                 column(6,radioButtons("compare.classes","Compare classes?",inline=FALSE,c("True"="TRUE","False"="FALSE"),selected = "FALSE")),
                 column(6,radioButtons("exampledata","Use example data?",inline=FALSE,c("True"="TRUE","False"="FALSE"),selected = "FALSE"))
          )
  )
          #column(width=6,textInput("outloc", "Output folder name:","",placeholder="Default: xwasresults"))
  ))
  #column(12,tags$div(
  #  column(6,radioButtons("pairedanalysis", "Are there repeated measurements?", inline=FALSE,c("True - Paired (repeated measures)" = "TRUE","False - Unpaired (case-control & multiclass)" = "FALSE"),selected = "FALSE")),
  #  column(6,radioButtons("compare.classes","Compare classes?",inline=FALSE,c("True"="TRUE","False"="FALSE"),selected = "FALSE")),
  #  column(6,radioButtons("exampledata","Use example data?",inline=FALSE,c("True"="TRUE","False"="FALSE"),selected = "FALSE"))

  #))
)
