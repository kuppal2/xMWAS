#load package
library(xMWAS)

#example dataset that includes metabolome, transcriptome, and cytokine data from the H1N1 mice study (Chandler 2016)
data(exh1n1)
#data(classlabels) #example classlabels file for case vs control design
#data(classlabels_repeatmeasures) #example classlabels file for repeat measures design

xMat<-exh1n1$metabolome
yMat<-exh1n1$transcriptome
zMat<-exh1n1$cytokine
wMat<-NA
classlabels<-exh1n1$classlabels

#Code for reading tab-delimited text files as input data
#currently turned off:
if(FALSE)
{
    fname1<-"/Users/karanuppal/Downloads/OneDrive_1_11-3-2017/gene.txt"
    fname2<-"/Users/karanuppal/Downloads/OneDrive_1_11-3-2017/clinical.txt"
    fname3<-"/Users/karanuppal/Downloads/OneDrive_1_11-3-2017/metabolomics.txt"
    class_fname<-"/Users/karanuppal/Downloads/OneDrive_1_11-3-2017/Classfile.txt"
    xMat<-read.table(fname1,sep="\t",header=TRUE,row.names=1)
    yMat<-read.table(fname2,sep="\t",header=TRUE,row.names=1)
    zMat<-read.table(fname3,sep="\t",header=TRUE,row.names=1)
    classlabels<-read.table(class_fname,sep="\t",header=TRUE)
    xMat<-as.data.frame(xMat)
    yMat<-as.data.frame(yMat)
    zMat<-as.data.frame(zMat)
    wMat<-NA
}
###################

output<-"/Users/karanuppal/Downloads/xMWASv0.55output/" #change for your computer

#Please see user manual for description of arguments:
#https://github.com/kuppal2/xMWAS/blob/master/example_manual_tutorial/xMWAS-manual.pdf

#call the run_xmwas() function:
xmwas_res<-run_xmwas(Xome_data=xMat,Yome_data=yMat,Zome_data=zMat,Wome_data=NA,outloc=output,
classlabels=NA,class_fname=NA,xmwasmethod="pls",plsmode="regression",
max_xvar=10000, #e.g. select top 10000 of the variabels in X dataset based on relative standard deviation; change according to your dataset; you can also use proportion such as round(nrow(xMat)*0.3) to select top 30% of the variables.
max_yvar=10000, #select top 10000 of the variabels in Y dataset based on relative standard deviation;  change according to your dataset; you can also use proportion such as round(nrow(yMat)*0.3) to select top 30% of the variables.
max_zvar=10000, #select top 10000 variabels in Z dataset based on relative standard deviation;  change according to your dataset; you can also use proportion such as round(nrow(zMat)*0.3) to select top 30% of the variables.
max_wvar=10000, #select top 10000 variabels in W dataset based on relative standard deviation;  change according to your dataset; you can also use proportion such as round(nrow(wMat)*0.3) to select top 30% of the variables.
rsd.filt.thresh=1,
corthresh=0.4, #absolute correlation threshold
keepX=1000, #select up to top 1000 variables in the sPLS model; change according to your dataset
keepY=1000, #select up to top 1000 variables in the sPLS model; change according to your dataset
keepZ=1000, #select up to top 1000 variables in the sPLS model; change according to your dataset
keepW=1000, #select up to top 1000 variables in the sPLS model; change according to your dataset
pairedanalysis=FALSE, #set to TRUE if repeated measures study design
optselect=FALSE, #perform optimal PLS componenet selection; TRUE or FALSE; set to FALSE for exact Pearson correlation calculation using PLS regression
rawPthresh=0.05, #p-value threshold for correlation based on Student's t-test
numcomps=5, #max number of PLS components to use; set to N-1 (N: number of samples) for exact Pearson correlation calculation using PLS regression
net_edge_colors=c("blue","red"),
net_node_colors=c("orange", "green","cyan","pink"),
Xname="Metab", #change the name of dataset X
Yname="Gene", #change the name of dataset Y
Zname="Cytokine", #change the name of dataset Z
Wname="W", #change the name of dataset W
net_node_shape=c("square","circle","triangle","star"),
all.missing.thresh=0, #filter based on missing values: set to NA to turn it OFF; otherwise specify a value between: 0 to 1 (e.g. 0.8 to require that at least 80% of the samples have a non-missing value)
missing.val=0,
seednum=100,label.cex=0.2,vertex.size=6,
interactive=FALSE,max_connections=NA,
centrality_method="eigenvector", #centrality evaluation method
use.X.reference=FALSE,removeRda=TRUE,
compare.classes=FALSE, #compare classes: TRUE or FALSE
class.comparison.allvar=TRUE,
modularity.weighted=TRUE,
globalcomparison=TRUE,
plot.pairwise=FALSE, #plot results for pairwise comparisons: TRUE or FALSE
apply.sparse.class.comparison=FALSE, #perform variable selection in sPLS during class-wise comparison (default: FALSE)
layout.type="fr1")

suppressWarnings(try(sink(file=NULL),silent=TRUE))

