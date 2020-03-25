
##############################1. Data Preparation: making sure all sample IDs match across different datasets#########################################

#load xMWAS package:  (https://github.com/kuppal2/xMWAS);
library(xMWAS)

#load mixOmics
library(mixOmics)

#load corrplot
library(corrplot)


#read data
Xmat<-read.delim("/Users/karanuppal/Downloads/input_data/Michigan_Neg_None_1.txt",row.names=1)
Ymat<-read.delim("/Users/karanuppal/Downloads/input_data/Broad_None_1.txt",row.names=1)

outloc="/Users/karanuppal/Documents/Emory/JonesLab/Projects/MoTrPAC/PASS1A_rat/compareintegmethods/"

############################## Compare different methods#########################################


#transpose data frame to meet the formatting requirements of mixOmics
X=t(Xmat)
Y=t(Ymat)


dir.create(outloc)
setwd(outloc)
pdf("network_res.pdf")
#1. Perform pairwise Pearson correlation analysis between variables in X and Y
pcor<-cor((cbind(X,Y)))
net_res<-pcor[1:dim(X)[2],(dim(X)[2]+1):(dim(X)[2]+dim(Y)[2])]
rnames1<-rownames(net_res)
cnames1<-colnames(net_res)
rnames1<-gsub(rnames1,pattern="_[0-9]*",replacement="")
cnames1<-gsub(cnames1,pattern="_[0-9]*",replacement="")
c1<-rnames1[which(rnames1%in%cnames1)]
rownames(net_res)<-rnames1
colnames(net_res)<-cnames1
d1com<-net_res[which(rnames1%in%cnames1),which(cnames1%in%rnames1)]
d1com<-d1com[match(colnames(d1com),rownames(d1com)),]
pcorcom<-round(d1com,3)


#2. Perform PLS in regression mode
cplsreg=pls(X,Y)
netplsreg=network(cplsreg,cutoff=0)
net_res<-netplsreg$M
rnames1<-rownames(net_res)
cnames1<-colnames(net_res)

rnames1<-gsub(rnames1,pattern="_[0-9]*",replacement="")
cnames1<-gsub(cnames1,pattern="_[0-9]*",replacement="")
c1<-rnames1[which(rnames1%in%cnames1)]
rownames(net_res)<-rnames1
colnames(net_res)<-cnames1
d1com<-net_res[which(rnames1%in%cnames1),which(cnames1%in%rnames1)]
d1com<-d1com[match(colnames(d1com),rownames(d1com)),]
netplsregcom<-round(d1com,3)


#3. Perform Regularized Canonical Correlation analysis using the shrinkage method
crccshrink=mixOmics::rcc(X,Y,method="shrinkage")
netrccshrink=network(crccshrink,cutoff=0)
net_res<-netrccshrink$M
rnames1<-rownames(net_res)
cnames1<-colnames(net_res)

rnames1<-gsub(rnames1,pattern="_[0-9]*",replacement="")
cnames1<-gsub(cnames1,pattern="_[0-9]*",replacement="")
c1<-rnames1[which(rnames1%in%cnames1)]
rownames(net_res)<-rnames1
colnames(net_res)<-cnames1
d1com<-net_res[which(rnames1%in%cnames1),which(cnames1%in%rnames1)]
d1com<-d1com[match(colnames(d1com),rownames(d1com)),]
netrccshrinkcom<-round(d1com,3)

#4. Perform PLS in canonical mode
cplscan=pls(X,Y,mode="canonical")
netplscan=network(cplscan,cutoff=0)
net_res<-netplscan$M
rnames1<-rownames(net_res)
cnames1<-colnames(net_res)

rnames1<-gsub(rnames1,pattern="_[0-9]*",replacement="")
cnames1<-gsub(cnames1,pattern="_[0-9]*",replacement="")
c1<-rnames1[which(rnames1%in%cnames1)]
rownames(net_res)<-rnames1
colnames(net_res)<-cnames1
d1com<-net_res[which(rnames1%in%cnames1),which(cnames1%in%rnames1)]
d1com<-d1com[match(colnames(d1com),rownames(d1com)),]
netplscancom<-round(d1com,3)


#5. Perform Sparse Generalized Canonical Correlation Analysis
block_M<-list(X=X,Y=Y)
netsgcca<-wrapper.sgcca(block_M)
netsgcca=network(netsgcca,cutoff=0)
rownames(netsgcca$M_X_Y)<-rownames(netplsreg$M)
net_res<-netsgcca$M_X_Y
rnames1<-rownames(net_res)
cnames1<-colnames(net_res)

rnames1<-gsub(rnames1,pattern="_[0-9]*",replacement="")
cnames1<-gsub(cnames1,pattern="_[0-9]*",replacement="")
#rnames1<-gsub(rnames1,pattern="\\.[0-9]*",replacement="")
#cnames1<-gsub(cnames1,pattern="\\.[0-9]*",replacement="")
c1<-rnames1[which(rnames1%in%cnames1)]
rownames(net_res)<-rnames1
colnames(net_res)<-cnames1
d1com<-net_res[which(rnames1%in%cnames1),which(cnames1%in%rnames1)]
d1com<-d1com[match(colnames(d1com),rownames(d1com)),]
netsgccacom<-round(d1com,3)



dev.off()

#generate bubble plots representing pairwise correlations between the first ten variables in each dataset
pdf("compare_correlations_first10rows.pdf")
par(mfrow=c(3,2),cex=0.8)

corrplot(round(pcor[1:10,(dim(X)[2]+1):(dim(X)[2]+10)],3),main="Pearson Correlation",cex.main=0.6,tl.cex=0.5,cl.ratio=0.3,mar=c(0,0,2,0),cl.align="c")

corrplot(round(netplsreg$M[1:10,1:10],3),main="PLS regression mode",cex.main=0.6,tl.cex=0.5,
cl.ratio=0.3,mar=c(0,0,1,0),cl.align="c")

corrplot(round(netrccshrink$M[1:10,1:10],3),main="Regularized Canonical Correlation (shrinkage)",cex.main=0.6,tl.cex=0.5,cl.ratio=0.3,mar=c(0,0,2,0),cl.align="c")

corrplot(round(netplscan$M[1:10,1:10],3),main="PLS canonical mode",cex.main=0.6,tl.cex=0.5,cl.ratio=0.3,
mar=c(0,0,2,0),cl.align="c")

corrplot(round(netsgcca$M_X_Y[1:10,1:10],3),main="sparse generalized canonical \ncorrelation analysis (SGCCA)",cex.main=0.6,tl.cex=0.5,cl.ratio=0.3,mar=c(0,1,2,0),cl.align="c")


dev.off()




maxnum=41

#generate bubble plots representing pairwise correlations between the first ten variables in each dataset
pdf("compare_correlations_common.pdf")
par(mfrow=c(5,2),cex=0.8)

corrplot::corrplot(round(pcorcom[1:maxnum,1:maxnum],3),main=paste("Pearson Correlation "),cex.main=0.6,tl.cex=0.5,cl.ratio=0.3,mar=c(0,0,2,0),cl.align="c",ylab="Broad",xlab="Duke")

corrplot::corrplot(round(netplsregcom[1:maxnum,1:maxnum],3),main=paste("PLS regression mode"),cex.main=0.6,tl.cex=0.5,
cl.ratio=0.3,mar=c(0,0,1,0),cl.align="c")


corrplot::corrplot(round(netrccshrinkcom[1:maxnum,1:maxnum],3),main=paste("Regularized Canonical Correlation"),cex.main=0.6,tl.cex=0.5,cl.ratio=0.3,mar=c(0,0,2,0),cl.align="c")

corrplot::corrplot(round(netplscancom[1:maxnum,1:maxnum],3),main=paste("PLS canonical mode "),cex.main=0.6,tl.cex=0.5,cl.ratio=0.3,
mar=c(0,0,2,0),cl.align="c")


corrplot::corrplot(round(netsgccacom[1:maxnum,1:maxnum],3),main=paste("sparse generalized canonical \ncorrelation analysis (SGCCA) "),cex.main=0.6,tl.cex=0.5,cl.ratio=0.3,mar=c(0,1,2,0),cl.align="c")

dev.off()


maxnum=41

#generate bubble plots representing pairwise correlations between the first ten variables in each dataset
pdf("histcorrelations_common.pdf")
par(mfrow=c(3,2),cex=0.8)

hist(pcorcom,xlim=c(-1,1),col="orange",main="Pearson correlation",cex.main=0.8)


hist(netplsregcom,xlim=c(-1,1),col="orange",main="PLS regression mode",cex.main=0.8)


hist(netrccshrinkcom,xlim=c(-1,1),col="orange",main="Regularized Canonical Correlation",cex.main=0.8)


hist(netplscancom,xlim=c(-1,1),col="orange",main="PLS canonical mode",cex.main=0.8)


hist(netsgccacom,xlim=c(-1,1),col="orange",main="SGCCA",cex.main=0.8)


dev.off()


####################END Comparison of different methods


