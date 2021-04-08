#load package
library(xMWAS)
#/home/data/ExposomeBootCamp2019
#load TCE dataset that includes metabolome, TCE exposure, nephrotoxicity, and immune markers data (Walker 2016)
xMat<-read.table("https://raw.githubusercontent.com/kuppal2/xMWAS/master/example_manual_tutorial/ExposomeBootcamp2019/xMWAS_lab_TCEexposure_data.txt",sep="\t",header=TRUE,row.names=1)
yMat<-read.table("https://raw.githubusercontent.com/kuppal2/xMWAS/master/example_manual_tutorial/ExposomeBootcamp2019/xMWAS_lab_metab_data.txt",sep="\t",header=TRUE,row.names=1)
zMat<-read.table("https://raw.githubusercontent.com/kuppal2/xMWAS/master/example_manual_tutorial/ExposomeBootcamp2019/xMWAS_lab_immunemarkers_data.txt",sep="\t",header=TRUE,row.names=1)
wMat<-read.table("https://raw.githubusercontent.com/kuppal2/xMWAS/master/example_manual_tutorial/ExposomeBootcamp2019/xMWAS_lab_nephrotoxicity_data.txt",sep="\t",header=TRUE,row.names=1)

classlabels<-read.table("https://raw.githubusercontent.com/kuppal2/xMWAS/master/example_manual_tutorial/ExposomeBootcamp2019/xMWAS_lab_classlabels.txt",sep="\t",header=TRUE)

#assign output location
output<-"~/xMWASv0.553output/"

#call the run_xmwas() function:
xmwas_res<-run_xmwas(Xome_data=xMat,Yome_data=yMat,Zome_data=zMat,Wome_data=wMat,
outloc=output, classlabels=classlabels,class_fname=NA,xmwasmethod="pls",plsmode="regression",
max_xvar=10000,max_yvar=10000, max_zvar=10000,max_wvar=10000,rsd.filt.thresh=-1,corthresh=0.25,
keepX=1000,keepY=1000,keepZ=1000,keepW=1000, pairedanalysis=FALSE,optselect=TRUE,rawPthresh=0.05,
numcomps=10,net_edge_colors=c("blue","red"), net_node_colors=c("orange", "green","cyan","pink"),Xname="Exposure",
Yname="Metabolome",Zname="Immune",Wname="Nephrotoxicity", net_node_shape=c("square","circle","triangle","star"),all.missing.thresh=0,
missing.val=0, seednum=100,label.cex=0.2,vertex.size=6,max_connections=NA, centrality_method="eigenvector",
use.X.reference=FALSE,removeRda=TRUE,compare.classes=TRUE,class.comparison.allvar=FALSE,modularity.weighted=TRUE)
suppressWarnings(try(sink(file=NULL),silent=TRUE))

