library(shiny)

source("R/introduction_page.R")
source("R/analysis_page.R")
source("R/help_page.R")

ui<-fluidPage(
    tags$head(
      tags$meta(charset="utf-8"),
      tags$meta(name="description",content="Free Web tutorials"),
      tags$title("xMWAS - v1.0"),
      tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css")
    ),
    column(12,tags$div(headerPanel(h3("xMWAS - a data-driven integration and network analysis tool (v1.0)")))),
    column(12,tabsetPanel(
        tabPanel("Introduction", introduction_page), 
        tabPanel("Analysis", analysis_page), 
        tabPanel("Help and Support", help_page),
        type ="tabs"
      )),
    column(style="padding-top:0px;padding-bottom:0px;",12,tags$hr(style="margin-top:0px;margin-bottom:15px;border-top: 0.5px solid #ccccb3;")),
    column(12,  tags$div(style="margin-center",tags$footer(align="center",color="white",style="font-weight:normal;font-size:80%;color:black","Citation: Uppal K, Ma C, Go YM, Jones DP. xMWAS: a data-driven integration and differential network analysis tool. ",tags$span(style='font-style:italic;','Bioinformatics.')," 2018 Feb 15. PMID: ",tags$a(href="https://www.ncbi.nlm.nih.gov/pubmed/29069296",target="_blank","29069296")))),
 #column(12,  tags$div(style="margin-center",tags$footer(align="center",color="white",style="font-weight:normal;font-size:80%;color:black","Maintained by Karan Uppal (",tags$a(href="mailto:kuppal3gt@gmail.com","kuppal3gt@gmail.com"),"))
 column(12,  tags$div(style="margin-center",tags$footer(align="center",color="white",style="font-weight:normal;font-size:95%;color:black","Please ask questions or report any issues on the ",
                                                        tags$a(href='https://github.com/kuppal2/xMWAS/issues',target="_blank","GitHub"), " page"))),
 column(12,  tags$div(style="margin-center",tags$footer(align="center",color="white",style="font-weight:normal;font-size:95%;color:black","Release date: 11/09/2024")))
 )
