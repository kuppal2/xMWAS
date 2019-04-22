\name{run_xmwas}
\alias{run_xmwas}
\title{run_xmwas}
\description{
The function uses sPLS or PLS and network functions in mixOmics package to perform pairwise integrative and correlation analysis. 
The pairwise graphs are merged using igraph and community detection is performed using the Multilevel clustering algorithm.
Association networks can be visualized in R or using Cytoscape.
}
\usage{
run_xmwas(xome_fname = NA, yome_fname = NA, zome_fname = NA, wome_fname = NA,
 outloc = NA,
 class_fname = NA, Xome_data = NA, Yome_data = NA, Zome_data = NA, Wome_data = NA, 
 classlabels = NA, xmwasmethod = "spls", plsmode = "canonical", max_xvar = 5000,
  max_yvar = 5000, max_zvar = 5000, max_wvar = 5000, rsd.filt.thresh = 1,
   all.missing.thresh = 0.1, missing.val = 0, corthresh = 0.4, keepX = 1000,
    keepY = 1000, keepZ = 1000, keepW = 1000, pairedanalysis = FALSE,
     optselect = TRUE, rawPthresh = 0.05, numcomps = 10,
      net_edge_colors = c("blue", "red"), net_node_colors = c("orange", "green", "blue", "gold"),
       Xname = "X", Yname = "Y", Zname = "Z", Wname = "W", 
       net_node_shape = c("circle", "rectangle", "triangle", "star"), seednum = 100,
        label.cex = 0.3, vertex.size = 6, max_connections = NA, 
        centrality_method = "eigenvector", use.X.reference = FALSE,
         removeRda = TRUE, compare.classes = TRUE, class.comparison.allvar = TRUE, ...)
}
\arguments{
\item{xome_fname}{
Full path with filename for dataset A. 
Default: NA; The software uses
the value provided for Xome_data
when this is set to NA.
}
  \item{yome_fname}{
Full path with filename for dataset B
Default: NA; The software uses
the value provided for Yome_data
when this is set to NA.
}
  \item{zome_fname}{
Full path with filename for dataset C
Default: NA; The software uses
the value provided for Zome_data
when this is set to NA.
}
  \item{wome_fname}{
Full path with filename for dataset D
Default: NA; The software uses
the value provided for Wome_data
when this is set to NA.
}
  \item{Xome_data}{
Data matrix for dataset A.
Run: 
data(exnci60); head(exnci60$mrna) 
to see how to format data
matrices.
}
  \item{Yome_data}{
  Data matrix for dataset B
}
  \item{Zome_data}{
  Data matrix for dataset C
}
  \item{Wome_data}{
  Data matrix for dataset D
}
  \item{outloc}{
Output directory 
}
  \item{classlabels}{
Data matrix with phenotype information. Set to NA
if this information is not available.
see: data(classlabels_casecontrol) for case vs control design
or data(classlabels_repeatmeasures) for repeat measures
}
  \item{class_fname}{
File with phenotype information. Set to NA
if this information is not available.
see: data(classlabels_casecontrol) for case vs control design
or data(classlabels_repeatmeasures) for repeat measures
}
  \item{xmwasmethod}{
Method for data integration. 
eg: 
"pls": partial least squares regression
"spls": sparse partial least squares regression
"o1pls": orthogonal partial least squares regression
}
  \item{plsmode}{
"canonical" for bi-directional relationships;
"regression" for regression/predictive relationships
}
  \item{max_xvar}{
Maximum number of X variables to select based on
relative standard deviation (RSD). e.g. 10000
}
  \item{max_yvar}{
Maximum number of Y variables to select based on
relative standard deviation (RSD). e.g. 10000
}
  \item{max_zvar}{
Maximum number of Z variables to select based on
relative standard deviation (RSD). e.g. 10000
}
  \item{max_wvar}{
Maximum number of W variables to select based on
relative standard deviation (RSD). e.g. 10000
}
  \item{rsd.filt.thresh}{
Relative standard deviation (sd/mean) threshold
}
 \item{all.missing.thresh}{
Remove variables (rows) that do not meet the 
minimum threshold for presence of 
non-missing values. e.g. 0.8
}
 \item{missing.val}{
How are the missing values represented in the
input data files? Default: 0
}
  \item{corthresh}{
Correlation threshold. eg: 0.7
}
  \item{keepX}{
Maximum number of X variables to select in sPLS.
Note: keepX, keepY, keepZ, and keepW are only
used when xmwasmethod is set to "spls"
}
  \item{keepY}{
Maximum number of Y variables to select in sPLS.
Note: keepX, keepY, keepZ, and keepW are only
used when xmwasmethod is set to "spls"
}
  \item{keepZ}{
Maximum number of Z variables to select in sPLS.
Note: keepX, keepY, keepZ, and keepW are only
used when xmwasmethod is set to "spls"
}
  \item{keepW}{
Maximum number of W variables to select in sPLS.
Note: keepX, keepY, keepZ, and keepW are only
used when xmwasmethod is set to "spls"
}
  \item{pairedanalysis}{
Are their repeated measurements? TRUE or FALSE
}
  \item{optselect}{
Find optimal number of PLS components. TRUE or FALSE
}
  \item{rawPthresh}{
p-value threshold calculated using Student's t-test.
eg: 0.05
}
  \item{numcomps}{
Number of components to use in PLS model. eg: 3
}
  \item{net_edge_colors}{
Colors for edges.
}
  \item{net_node_colors}{
Colors for nodes.
}
  \item{Xname}{
Name for X dataset. eg: "Genes"
}
  \item{Yname}{
Name for Y dataset. eg: "Proteins"
}
  \item{Zname}{
Name for Z dataset. eg: "Metabolites"
}
  \item{Wname}{
Name for W dataset. eg: "EnvironmentalExposures"
}
  \item{net_node_shape}{
Shapes for nodes.
}
  \item{seednum}{
Seed for random number generator used for
plotting the network.
}
  \item{label.cex}{
Size of the labels. eg: 0.8
}
  \item{vertex.size}{
Size of the nodes.
}
  \item{max_connections}{
Maximum number of associations to include in the 
network. The connections between nodes 
are ranked based on the strength of association (+ve and -ve).
Only the top "max_connections" connections are shown and used for
centrality and community detection analyses. Set 
max_connections=NA if you want to use all connections.
e.g. 1e5, 1e6, or NA
}
 \item{centrality_method}{
Method for centrality analysis. 
Options: 
1) "eigenvector" for eigenvector centrality,
which is based on the number and quality of connections
	- centrality scores range from 0 to 1,
	where 1 means high centrality and 0 means 
	low or no centrality. Nodes/vertices with
	high centrality scores are connected to many other
	nodes, which are in turn connected to many 
	other nodes. Please see igraph::eigen_centrality
	function for more details.
	
2) "betweenness" for betweenness centrality,
which is based on the number of shortest paths going
through a node/vertex
	- centrality scores are normalized and scaled to
	0 to 1 range, where 1 means high betweenness
	 centrality and 0 means low or no 
	 betweenness centrality. 
	Please see igraph::betweenness function 
	for more details.
	
3) "degree.count" based on the number of connections of 
a node. 
	-centrality scores are normalized and scaled to
	0 to 1 range. High centrality means more connections.
	Please see igraph::degree function for more
	details.
	
4) "degree.weight" is based on the sum of absolute
 	weights of connections of a node.
	-centrality scores are normalized and scaled to 0
	to 1 range. High centrality means stronger connections.
	
5) "closeness" based on the reciprocal of the sum of the 
	distances of a node/vertex to all other nodes.
	-centrality scores are normalized. High centrality
	means the node is closer to all other nodes.

}
 \item{use.X.reference}{
TRUE or FALSE if you want to use Xome_data
as reference. If TRUE, only X<->Y, X<->Z,
and X<->W pairwise analysis will be performed.
}
\item{removeRda}{
TRUE or FALSE; set to TRUE if you want to remove the
intermediate files.
}
\item{compare.classes}{
TRUE or FALSE; set to TRUE if you want to compare
individual classes as provided in class labels file.
}
\item{class.comparison.allvar}{
TRUE or FALSE; set to TRUE if all nodes shown
	
}

}
\author{
Karan Uppal
kuppal2@emory.edu

}
