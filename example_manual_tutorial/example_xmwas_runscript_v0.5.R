#load package
library(xMWAS)

#example dataset that includes metabolome, transcriptome, and cytokine data from the H1N1 mice study (Chandler 2016)
data(exh1n1)
data(classlabels_casecontrol) #example classlabels file for case vs control design
data(classlabels_repeatmeasures) #example classlabels file for repeat measures design
xMat<-exh1n1$metabolome
yMat<-exh1n1$transcrlptome
zMat<-exh1n1$cytokine
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
}
###################

<<<<<<< HEAD
output<-"/Users/karanuppal/Downloads/xMWASv0.53output/" #change for your computer
=======
output<-"/Users/karanuppal/Downloads/OneDrive_1_11-3-2017/xMWASv0.52output/" #change for your computer
>>>>>>> 8718699d6a3f396a337058138bd9b1983a87fd85

#Please see user manual for description of arguments:
#https://github.com/kuppal2/xMWAS/blob/master/example_manual_tutorial/xMWAS-manual.pdf

#call the run_xmwas() function:
xmwas_res<-run_xmwas(Xome_data=xMat,Yome_data=yMat,Zome_data=zMat,Wome_data=NA,outloc=output,
classlabels=classlabels,class_fname=NA,xmwasmethod="spls",plsmode="canonical",max_xvar=5000,max_yvar=5000,
max_zvar=5000,max_wvar=5000,rsd.filt.thresh=1,corthresh=0.4,keepX=1000,keepY=1000,keepZ=1000,keepW=1000,
pairedanalysis=FALSE,optselect=TRUE,rawPthresh=0.05,numcomps=10,net_edge_colors=c("blue","red"),
net_node_colors=c("orange", "green","cyan","pink"),Xname="X",Yname="Y",Zname="Z",Wname="W",
net_node_shape=c("rectangle","circle","triangle","star"),all.missing.thresh=0.1,missing.val=0,
seednum=100,label.cex=0.2,vertex.size=6,graphclustering=TRUE,interactive=FALSE,max_connections=100000,
<<<<<<< HEAD
centrality_method="eigenvector",use.X.reference=FALSE,removeRda=TRUE,compare.classes=TRUE,class.comparison.allvar=TRUE)
=======
centrality_method="eigenvector",use.X.reference=FALSE,removeRda=TRUE,compare.classes=TRUE)
>>>>>>> 8718699d6a3f396a337058138bd9b1983a87fd85
suppressWarnings(try(sink(file=NULL),silent=TRUE))

