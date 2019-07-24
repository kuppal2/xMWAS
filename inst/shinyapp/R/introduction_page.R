library(shiny)

introduction_page <- fluidPage(
  column(12),
  column(12,style="",
          tags$div(id="introduction_paragraph2",tags$p(align="center",style="font-weight:bold;font-size:120%;color:darkblue","xMWAS provides an automated workflow for data integration, network visualization, 
                  clustering, and differential network analysis of up to four datasets from biochemical and phenotypic assays, and 
                   omics platforms."), tags$p(align="center",style="font-weight:bold;font-size:80%;color:black","For installing xMWAS locally in R run: "),tags$p(align="center",style="font-weight:bold;font-size:80%;color:darkred","library(devtools);install_github(\"kuppal2/xMWAS\")"))
         ),
  column(12,tags$img(style="margin-left:auto;margin-right:auto;display: block;",src="images/Figure_homepage.png",height='350px',width="60%"))
)
