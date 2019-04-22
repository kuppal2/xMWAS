#load package
library(xMWAS)

#example dataset that includes metabolome, transcriptome, and cytokine data from the H1N1 mice study (Chandler 2016)
data(exh1n1)
data(classlabels_casecontrol) #example classlabels file for case vs control design
data(classlabels_repeatmeasures) #example classlabels file for repeat measures design
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
max_xvar=nrow(xMat)*0.3, #select top 30% of the variabels in X dataset based on relative standard deviation; change according to your dataset
max_yvar=nrow(yMat)*0.3, #select top 30% of the variabels in Y dataset based on relative standard deviation;  change according to your dataset
max_zvar=nrow(zMat)*1, #select all variabels in Z dataset based on relative standard deviation;  change according to your dataset
max_wvar=nrow(wMat)*1, #select all variabels in W dataset based on relative standard deviation;  change according to your dataset
rsd.filt.thresh=1,
corthresh=0.5, #absolute correlation threshold
keepX=0.1*nrow(xMat), #select top 10% of the variables in the sPLS model; change according to your dataset
keepY=0.1*nrow(yMat), #select top 10% of the variables in the sPLS model; change according to your dataset
keepZ=0.1*nrow(zMat), #select top 10% of the variables in the sPLS model; change according to your dataset
keepW=0.1*nrow(wMat), #select top 10% of the variables in the sPLS model; change according to your dataset
pairedanalysis=FALSE, #set to TRUE if repeated measures study design
optselect=TRUE, #perform optimal PLS componenet selection; TRUE or FALSE; set to FALSE for exact Pearson correlation calculation using PLS regression
rawPthresh=1, #p-value threshold for correlation based on Student's t-test
numcomps=10, #max number of PLS components to use; set to N-1 (N: number of samples) for exact Pearson correlation calculation using PLS regression
net_edge_colors=c("blue","red"),
net_node_colors=c("orange", "green","cyan","pink"),
Xname="Metab", #change the name of dataset X
Yname="Transcriptome", #change the name of dataset Y
Zname="Antibody", #change the name of dataset Z
Wname="W", #change the name of dataset W
net_node_shape=c("square","circle","triangle","star"),
all.missing.thresh=NA, #filter based on missing values: set to NA to turn it OFF; otherwise specify a value between: 0 to 1 (e.g. 0.8)
missing.val=0,
seednum=100,label.cex=0.2,vertex.size=6,
graphclustering=TRUE,
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

