library(shiny)

preparation<-fluidRow(
  column(12,
         column(6,numericInput(width="380px","rsd_filt_thresh", "Relative Standard Deviation (RSD) Threshold (rows):", 1, min = 0, max = 1000))
  ),
  column(12,tags$div(
    column(6,numericInput(width="380px","max_xvar", "Maximum #datasetA variables to select based on RSD:", 1000, min = 1, max = 1000000)),
    column(6,numericInput(width="380px","max_yvar", "Maximum #datasetB variables to select based on RSD:", 1000, min = 1, max = 1000000))
  )),
  column(12,tags$div(
    column(6,uiOutput("rsd_dataC")),
    column(6,uiOutput("rsd_dataD"))
  )),
   column(12,
          column(6,numericInput(width="380px","all_missing_thresh", "Minimum non-missing sample ratio (rows):", 0, min = 0, max = 1)),
         column(6,selectInput(width="380px","missing.val","How are the missing values represented in the data?:",c("0","NA")))
  )
)
