library(shiny)

analysis<-fluidRow(
          column(12,tags$h4('Pairwise integrative analysis')),
          column(12,tags$div(
column(6,selectInput(width="250px","xmwasmethod","Choose a data integration method:",c("PLS: Partial least squares regression","sPLS: sparse PLS","OPLS: orthogonal PLS","RCC: regularized canonical correlation"))),
                   column(10,selectInput(width="150px","plsmode","Choose PLS mode (not applicable to RCC option):",c("regression","canonical")))
                   )),
          column(12,tags$div(
                        column(6,numericInput(width="300px","numcomps", "Number of components to use in PLS model:", 5, min = 1, max = 100)),
column(6,radioButtons("optselect", "Find optimal number of PLS components? (Note: turning this option ON may increase run time)", inline=TRUE,c(True = "TRUE",False = "FALSE"),selected = "FALSE"))
                    )
          ),
          conditionalPanel(
            condition = "input.xmwasmethod=='sPLS: sparse PLS'",
            column(12,tags$div(
column(6,numericInput(width="380px","keepX", "Maximum #datasetA variables to select in sPLS (change according to your dataset):", 1000, min = 1, max = 100000)),
              column(6,numericInput(width="380px","keepY", "Maximum #datasetB variables to select in sPLS (change according to your dataset):", 1000, min = 1, max = 100000))
            )),
            column(12,tags$div(
              column(6,uiOutput("spls_dataC")),
              column(6,uiOutput("spls_dataD"))
            ))
          ),
          column(style="padding-top:0px;padding-bottom:0px;",12,tags$hr(style="margin-top:0;margin-bottom:3px;border-top: 1px solid #000000;")),
          column(12,tags$h4('Association analysis')),
          column(12,tags$div(
                 column(6,numericInput("corthresh", "Correlation Threshold:", 0.4, min = 0, max = 1)),
                 column(6,numericInput("rawPthresh", "P-value Threshold For Student’s T-test:", 0.05, min = 0, max = 1))
          ))
)
