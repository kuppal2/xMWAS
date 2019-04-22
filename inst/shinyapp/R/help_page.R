library(shiny)

help_page <- fluidPage(
  column(12,tags$h4("User Manual:")),
  column(12,tags$p("Click ",tags$a(href='xMWAS-manual.pdf',target="_blank","here")," to see the user manual.")),
  column(12,tags$h4("Input File Format (no missing values allowed):")),
  column(12,tags$div(style="margin-left:auto;margin-right:auto;width:90%",includeHTML("html/dataset_format.html"))),
  column(12,tags$div(style="padding-bottom:10px;margin-left:auto;margin-right:auto;width:90%",tags$p("Each row is a feature and each column is filename"))),
  column(12,tags$div(style="margin-left:auto;margin-right:auto;width:90%",includeHTML("html/classlabels_format_multiclass.html"))),
  column(12,tags$div(style="padding-bottom:20px;margin-left:auto;margin-right:auto;width:90%",tags$p("Two columns: the first column is filename; the second column is class. Each row is the information of a file."))),
  #column(12,tags$hr(style="margin-top:0;margin-bottom:3px;border-top: 3px double #8c8b8b"))
  column(12,tags$div(style="margin-left:auto;margin-right:auto;width:90%",includeHTML("html/classlabels_format_repeatmeasure_with_1factor.html"))),
  column(12,tags$div(style="padding-bottom:20px;margin-left:auto;margin-right:auto;width:90%",tags$p("Three columns: the first column is filename; the second column is measure identifier; the third column is factor1. Each row is the information of a file."))),
  column(12,tags$div(style="margin-left:auto;margin-right:auto;width:90%",includeHTML("html/classlabels_format_repeatmeasure_with_2factors.html"))),
  column(12,tags$div(style="padding-bottom:20px;margin-left:auto;margin-right:auto;width:90%",tags$p("Four columns: the first column is filename; the second column is measure identifier; the third column is factor1; the fourth column is factor2. Each row is the information of a file."))),
  column(12,tags$h4("Output Files:")),
  column(12,tags$img(style="margin-left:auto;margin-right:auto;display: block;",src="images/outputfiles.png",height="260px",width="60%")),
  column(12,tags$div(
    tags$h5(style="font-weight:bold;","Description of output files:"),
    tags$ol(
      tags$li(type="1",tags$p(tags$span(class="underline","cluster_membership_centrality_table.txt"),": includes the community membership and centrality measures for nodes that meet the association criteria and included in the association networks.")),
      tags$li(type="1",tags$p(tags$span(class="underline","InputParameters.txt"),": includes list of input arguments provided to the software and version numbers of all packages to facilitate reproducibility of results.")),
      tags$li(type="1",tags$p(tags$span(class="underline","LogMon_Jul_10_01_52_08_2017.txt"),": includes runtime output messages.")),
      tags$li(type="1",tags$p(tags$span(class="underline","class-wise_matrix_centrality.txt"),": includes the centrality measures across different conditions for nodes that meet the association criteria and included in the association networks.")),
      tags$li(type="1",tags$p(tags$span(class="underline","Network_stats.csv"),": includes network statistics such as graph clustering coefficient and the modularity measure (based on the community structure).")),
      tags$li(type="1",tags$p(tags$span(class="underline","MultiOme_Network_corthreshX_communities.pdf"),": includes multiome network plot with the communities identified using the multilevel community detection algorithm. Members of each community are assigned colors based on community/module/cluster membership (1: orange; 2: light blue; 3: dark green, and so on). X labels correspond to xome_fname data, Y labels correspond to yome_fname data, Z labels correspond to zome_fname data, W labels correspond to wome_fname data."),
                       tags$h5(style="padding-left:20%;font-weight:bold;","Example:"),
                       tags$img(style="margin-left:auto;margin-right:auto;display: block;",src="images/MultiOme_Network_corthreshX_communities_example.png",width="45%")),
      tags$li(type="1",tags$p(tags$span(class="underline","Multiome_Network_corthreshX.pdf"),": includes multiome network plot using all significantly associated variables. X labels correspond to xome_fname data, Y labels correspond to yome_fname data, Z labels correspond to zome_fname data, W labels correspond to wome_fname data."),
                       tags$h5(style="padding-left:20%;font-weight:bold;","Example:"),
                       tags$img(style="margin-left:auto;margin-right:auto;display: block;",src="images/Multiome_Network_corthreshX_example.png",width="45%")),
      tags$li(type="1",tags$p(tags$span(class="underline","MultiOme_Network_corthreshXcytoscapeall.gml"),": includes GML file for all significantly associated variables that can be uploaded to Cytoscape.")),
      tags$li(type="1",tags$p(tags$span(class="underline","pairwise_results/"),tags$br(),
                              "Pairwise integrative analysis results are under pairwise_results folder. The files under this folder correspond to each pairwise comparison (X<->Y, X<->Z, Y<-Z,..):",
                              tags$ul(
                                tags$li(tags$p(tags$span(class="underline","XYassociation_matrix_corthresh*.txt"),": correlation matrix with mapping between node labels and original variable names.")),
                                tags$li(tags$p(tags$span(class="underline","XYassociation_networkthresholdX.pdf"),": includes the pairwise network plots.")),
                                tags$li(tags$p(tags$span(class="underline","XYBoolean_association_matrix_corthreshX.txt"),": same as correlation matrix but correlations meeting the threshold are represented 1, and 0 otherwise."))
                              ))),
      tags$li(type="1",tags$p(tags$span(class="underline","README.txt"),": includes description of output files and additional notes."))
    )
  )),
  column(12,tags$h4("Download Resources:")),
  column(12,tags$p("Download ",tags$a(href='https://github.com/kuppal2/xMWAS',target="_blank","xMWAS")," from github."))
  #column(12,tags$p("Download ",tags$a(href='https://sourceforge.net/projects/xmwas/files/?source=navbar',target="_blank","xMWAS")," from sourceforge.")),
  #column(12,tags$p("Download ",tags$a(href='https://sourceforge.net/projects/xmwas/files/Sample_input_files/',target="_blank","example data")," from sourceforge."))
  
)
