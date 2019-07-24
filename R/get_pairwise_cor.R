get_pairwise_cor <-
function(Xome_data,Yome_data,max_xvar=100,max_yvar=100,rsd.filt.thresh=3,corthresh=0.4,keepX=100,keepY=100,pairedanalysis=FALSE,optselect=TRUE,classlabels=NA,rawPthresh=0.05,plsmode="regression",xmwasmethod="pls",numcomps=10,net_edge_colors=c("blue","red"),net_node_colors=c("orange", "green"),outloc=NA,Xname="X",Yname="Y",num_nodes=2,net_node_shape=c("rectangle","circle","triangle","star","square","csquare","crectangle","vrectangle"),seednum=100,tempXname="X",tempYname="Y",all.missing.thresh=0.1,missing.val=0,plot.pairwise=TRUE){


filename<-paste(Xname,Yname,sep="_x_")

print(paste("Performing ",filename," integrative analysis",sep=""))


Xome_data<-as.data.frame(Xome_data)
Yome_data<-as.data.frame(Yome_data)
numsampX<-dim(Xome_data)[2]

cor_thresh_check=seq(corthresh,1,0.01)

min_cor_thresh=0.9

cor_thresh_check=rev(cor_thresh_check)



for(c in cor_thresh_check){
    
    p1=corPvalueStudent(cor=c,n=numsampX)
    if(p1<rawPthresh){
        
        min_cor_thresh=c
    }
}

if(corthresh<min_cor_thresh){
    
    corthresh=min_cor_thresh
}


cl<-makeSOCKcluster(num_nodes)
clusterExport(cl,"do_rsd")



feat_rsds<-parApply(cl,Xome_data,1,do_rsd)
			
stopCluster(cl)

abs_feat_rsds<-abs(feat_rsds)

if(is.na(max_xvar)==FALSE){

if(dim(Xome_data)[1]>max_xvar){

Xome_data<-Xome_data[order(abs_feat_rsds,decreasing=TRUE)[1:max_xvar],]

abs_feat_rsds<-abs_feat_rsds[order(abs_feat_rsds,decreasing=TRUE)[1:max_xvar]]

}

}


	
good_metabs<-which(abs_feat_rsds>rsd.filt.thresh)


Xome_data<-Xome_data[good_metabs,]

if(nrow(Xome_data)<1){
    
    stop(paste("None of the ",Xname," variables meet the rsd threshold in the pairwise analysis.",sep=""))
}

print(paste("Filtering variables in ",Xname," matrix based on missing values:",sep=""))
Xome_data<-filter.by.missing.values(Xome_data,missing.val=missing.val,all.missing.thresh=all.missing.thresh)

if(nrow(Xome_data)<1){
    
    stop(paste("None of the ",Xname," variables meet the all.missing.thresh threshold in the pairwise analysis.",sep=""))
}


cl<-makeSOCKcluster(num_nodes)

clusterExport(cl,"do_rsd")
feat_rsds<-parApply(cl,Yome_data,1,do_rsd)
			
stopCluster(cl)

abs_feat_rsds<-abs(feat_rsds)

if(is.na(max_yvar)==FALSE){
if(dim(Yome_data)[1]>max_yvar){

Yome_data<-Yome_data[order(abs_feat_rsds,decreasing=TRUE)[1:max_yvar],]

abs_feat_rsds<-abs_feat_rsds[order(abs_feat_rsds,decreasing=TRUE)[1:max_yvar]]
}
}
					
good_metabs<-which(abs_feat_rsds>rsd.filt.thresh)
					
Yome_data<-Yome_data[good_metabs,]



if(nrow(Yome_data)<1){
    
    stop(paste("None of the ",Yname," variables meet the rsd threshold in the pairwise analysis.",sep=""))
}

print(paste("Filtering variables in ",Yname," matrix based on missing values:",sep=""))
Yome_data<-filter.by.missing.values(Yome_data,missing.val=missing.val,all.missing.thresh=all.missing.thresh)

if(nrow(Yome_data)<1){
    
    stop(paste("None of the ",Yname," variables meet the all.missing.thresh threshold in the pairwise analysis.",sep=""))
}

setwd(outloc)

X=Xome_data
Y=Yome_data


X<-as.data.frame(X)
Y<-as.data.frame(Y)


numsampX<-dim(X)[2]

numsampY<-dim(Y)[2]


if(numsampX!=numsampY){

	stop("Number of samples do not match between X and Y matrices.")
}





matrixA_1<-rownames(Xome_data) 
matrixB_1<-rownames(Yome_data) 


matrixA<-paste(tempXname,seq(1,dim(Xome_data)[1]),sep="")
matrixB<-paste(tempYname,seq(1,dim(Yome_data)[1]),sep="")

id_mapping_mat1<-cbind(matrixA_1,matrixA)
id_mapping_mat2<-cbind(matrixB_1,matrixB)

id_mapping_mat<-rbind(id_mapping_mat1,id_mapping_mat2)

id_mapping_mat<-as.data.frame(id_mapping_mat)

colnames(id_mapping_mat)<-c("Name","Node")



if(is.na(keepX)==TRUE){
    
    keepX<-dim(X)[1]
}

if(is.na(keepY)==TRUE){
    
    keepY<-dim(Y)[1]
}

if(keepX>dim(X)[1]){
	keepX<-dim(X)[1]
}

if(keepY>dim(Y)[1]){
	keepY<-dim(Y)[1]
}


X<-t(X)
Y<-t(Y)


numcomps_parent=numcomps

numdims_min=min(c(dim(X)[2],dim(Y)[2]))

if(is.na(numcomps)==TRUE){

	numcomps=numdims_min
}


colnames(X)<-matrixA
colnames(Y)<-matrixB

save(numcomps,file="numcomps.Rda")

if(xmwasmethod=="o1pls" || xmwasmethod=="opls"){
    
    xmwasmethod="o1pls"
}

{



		if(xmwasmethod=="spls"){
		linn.pls<-try(do_plsda(X=X,Y=Y,oscmode="spls",numcomp=numcomps,keepX=keepX[1],keepY=keepY[1],sparseselect=TRUE,analysismode=plsmode,pairedanalysis=pairedanalysis,optselect=optselect,design=classlabels),silent=TRUE)

		}else{
		    
		    if(xmwasmethod=="pls"){
			linn.pls<-try(do_plsda(X=X,Y=Y,oscmode="pls",numcomp=numcomps,keepX=keepX[1],keepY=keepY[1],sparseselect=FALSE,analysismode=plsmode,pairedanalysis=pairedanalysis,optselect=optselect,design=classlabels),silent=TRUE)
		    }else{
			
			
			if(xmwasmethod=="o1pls"){
			    linn.pls<-try(do_plsda(X=X,Y=Y,oscmode="o1pls",numcomp=numcomps,keepX=keepX[1],keepY=keepY[1],sparseselect=FALSE,analysismode=plsmode,pairedanalysis=pairedanalysis,optselect=optselect,design=classlabels),silent=TRUE)
			}else{
			    
			    
			    if(xmwasmethod=="o1spls"){
				linn.pls<-try(do_plsda(X=X,Y=Y,oscmode="o1pls",numcomp=numcomps,keepX=keepX[1],keepY=keepY[1],sparseselect=TRUE,analysismode=plsmode,pairedanalysis=pairedanalysis,optselect=optselect,design=classlabels),silent=TRUE)
			    }else{
			    
                
					
							if(xmwasmethod=="splsopt"){
							
									numcomps_try=numcomps
									if(numcomps_try>1){
									
									numcomps_try=min(ncol(X),ncol(Y))
									#linn.pls=spls::spls(X, Y, eta=cv1$eta.opt, K=cv1$K.opt)
									if(length(keepX)>1){
										if(pairedanalysis==FALSE){
											tune.spl.res<-tune.spls(X=X,Y=Y,ncomp=numcomps_try,test.keepX=keepX)
										}else{
										
											tune.spl.res<-tune.spls(X=X,Y=Y,ncomp=numcomps_try,test.keepX=keepX,multilevel=classlabels)
										}
										
										keepX=tune.spl.res$choice.keepX[1]
									
									
										}
									}
									linn.pls<-try(do_plsda(X=X,Y=Y,oscmode="spls",numcomp=numcomps_try,keepX=keepX[1],keepY=keepY[1],sparseselect=TRUE,analysismode=plsmode,pairedanalysis=pairedanalysis,optselect=optselect,design=classlabels),silent=TRUE)

							}else{
								if(xmwasmethod=="rcc"){
								
									if(numcomps>numdims_min){
										
										numcomps=numdims_min
									}
										
									#using shrinkage method;
									linn.pls<-mixOmics::rcc((X),(Y),ncomp=numcomps,method="shrinkage")
									#save(linn.pls,file="linn.pls1.Rda")
                                    
								}else{
                                    
												print(paste("Invalid option for argument xmwasmethod: ",xmwasmethod,sep=""))
                                    
									}
								
							}
					
			    }
			}
		    }
		}



			if(is(linn.pls,"try-error")){
				    
					print(paste("Error using ",xmwasmethod,sep=""))
					
                   
			}else{
                    if(xmwasmethod=="rcc"){
                        
                    }else{
                        linn.pls<-linn.pls$model
                    }
					
					
			}
}




save(linn.pls,file="linn.pls.Rda")


print(paste("Number of optimal (s)PLS components: ",numcomps,sep=""))

print_message<-paste("Generating global network plot at threshold: ",corthresh,sep="")
print(print_message)

set.seed(seednum)


p1=corPvalueStudent(cor=corthresh,n=numsampX)



#calculate the association score
net_result<-network_custom(mat=linn.pls, analysismode=plsmode,comp=numcomps)


#print(paste("1. Network analysis could not be performed: ",net_result,sep=""))
if(is(net_result,"try-error")){
    # net_result<-try(network(linn.pls, threshold=corthresh,row.names = TRUE, col.names = TRUE, block.var.names = TRUE,color.node = net_node_colors,shape.node = net_node_shape,
    #  color.edge = net_edge_colors,lty.edge = "solid", lwd.edge = 1,show.edge.labels = FALSE, interactive = FALSE,cex.node.name=0.7,show.color.key = FALSE),silent=TRUE)
    
    #print(paste("2. Network analysis could not be performed: ",net_result,sep=""))
    if(is(net_result,"try-error")){
        
        net_result<-try(network(linn.pls, cutoff=corthresh,row.names = TRUE, col.names = TRUE, block.var.names = TRUE,color.node = net_node_colors,shape.node = net_node_shape,
        color.edge = net_edge_colors,lty.edge = "solid", lwd.edge = 1,show.edge.labels = FALSE, interactive = FALSE,cex.node.name=0.7,show.color.key = FALSE),silent=TRUE)

        if(is(net_result,"try-error")){
            
            save(linn.pls,file="debug.Rda")
            stop(paste("Network analysis could not be performed: ",net_result,sep=""))
            }
        
        
        
        }
    
}
fname<-paste(filename,"netresult",".Rda",sep="")

save(net_result,file=fname)

fname<-paste(filename,"pls",".Rda",sep="")
save(linn.pls,file=fname)


x<-net_result$M

nrow_sim_mat<-dim(x)[1]
ncol_sim_mat<-dim(x)[2]

if(length(x)==0){
    
    stop("Network analysis could not be performed.")
}



if(ncol(x)>1){
simmat_colnames<-colnames(x)
}else{
	
	simmat_colnames<-colnames(linn.pls$Y)
}
simmat_col_ind<-gsub(simmat_colnames,pattern=tempYname,replacement="")

matrixB_1_simmat<-matrixB_1[as.numeric(as.character(simmat_col_ind))]

colnames(x)<-as.character(matrixB_1_simmat)

rnames1<-rownames(x)

rnames_ind<-gsub(rnames1,pattern=tempXname,replacement="")

rnames_ind<-as.numeric(as.character(rnames_ind))

rnames_ind2<-matrixA_1[rnames_ind]

rownames(x)<-as.character(rnames_ind2)


maxcor<-apply(abs(x),1,max)
maxcor1<-apply(abs(x),2,max)


rnames<-rnames1 	#paste("X",seq(1,dim(x)[1]),sep="")
cnames<-simmat_colnames #paste("Y",seq(1,dim(x)[2]),sep="")


colnames(x)<-as.character(cnames)
rownames(x)<-as.character(rnames)



if(length(which(x>1))>0){
    
    x[which(x>1)]<-1
}

if(length(which(x<(-1)))>0){
    
x[which(x<(-1))]<-(-1)
}

x=as.matrix(x)



	if(p1>rawPthresh){
		print(paste("correlation threshold ",corthresh," did not pass significance test.",sep=""))
        }
    #else
    {

rownames(x)<-as.character(rnames1)
colnames(x)<-as.character(simmat_colnames)
#highcorsimMat=x


rownames(x)<-as.character(rnames_ind2)
colnames(x)<-as.character(matrixB_1_simmat)



x<-round(x,4)

fname<-paste(filename,"_association_matrix_all.txt",sep="")
write.table(x,file=fname,sep="\t")

xtemp<-x[which(maxcor>=corthresh),which(maxcor1>=corthresh),drop=FALSE]



xtemp<-cbind(rnames1[which(maxcor>=corthresh)],xtemp)


xtemp1<-rbind(c("xName",simmat_colnames[which(maxcor1>=corthresh)]),xtemp)


#net_result$M<-cor2pcor(net_result$M)

fname<-paste(filename,"_association_matrix_threshold",corthresh,".txt",sep="")
write.table(xtemp1,file=fname,sep="\t")

xtemp<-abs(x[which(maxcor>=corthresh),which(maxcor1>=corthresh),drop=FALSE])

xtemp[which(xtemp>=corthresh)]<-1
xtemp[which(xtemp<corthresh)]<-0
#save(x,file="x.Rda")
#save(xtemp,file="xtemp.Rda")
if(length(which(maxcor>=corthresh))>1){
    
   
NumConnections<-apply(xtemp,1,sum)
}else{
	NumConnections<-sum(xtemp)
}

xtemp<-cbind(rnames1[which(maxcor>=corthresh)],xtemp)
            xtemp1<-rbind(c("xName",simmat_colnames[which(maxcor1>=corthresh)]),xtemp)


rownames(x)<-as.character(rnames1)
colnames(x)<-as.character(simmat_colnames)



xtemp1<-cbind(xtemp,NumConnections)

fname<-paste(filename,"Boolean_association_matrix_threshold",corthresh,".txt",sep="")
write.table(xtemp1,file=fname,sep="\t")



#fname<-paste(filename,"association_matrix_corthresh",corthresh,"rowcollabels.txt",sep="")
#write.table(xtempA,file=fname,sep="\t")

print(length(which(maxcor>=corthresh)))


if(length(which(maxcor>=corthresh))==1){

	net_res_temp=net_result$M[which(maxcor>=corthresh),]
	
	temp_row_names<-rownames(net_result$M)

	net_res_temp=as.data.frame(net_res_temp)
	
	net_res_temp<-t(net_res_temp)
	
	rownames(net_res_temp)<-temp_row_names[which(maxcor>=corthresh)]

	#save(net_res_temp,file="net_res_temp.Rda")

	
	
}else{

	net_res_temp=net_result$M[which(maxcor>=corthresh),]
	
}


nrow_mat<-nrow(net_res_temp)



if(ncol_sim_mat>1){
	if(nrow(net_res_temp)<0){
		break
	}

	edge_matrix<-apply(net_res_temp,1,function(x){which(abs(x)>corthresh)})


}else{
    
    
    if(length(net_res_temp)<0){
        break
    }
    
    edge_matrix<-which(abs(x)>corthresh)

}

#edge_matrix<-apply(netsub,1,function(x){which(abs(x)>corthresh)})

#save(net_result,file="net_result.Rda")
save(edge_matrix,file="edge_matrix.Rda")
save(ncol_sim_mat,file="ncol_sim_mat.Rda")


mat_cnames<-colnames(net_result$M)

#print(is.matrix(edge_matrix))

df={}


if(length(edge_matrix)>0){

	if(ncol_sim_mat>1){
		    
			if(is.matrix(edge_matrix)==FALSE){
				col_A<-names(edge_matrix)

				edge_matrix_1<-{}


				edge_matrix_1<-ldply(1:length(edge_matrix),function(r){
				   
				    col_B<-mat_cnames[edge_matrix[[r]]]
				    temp_edge_matrix<-{}
				    if(length(col_B)>0){
				       
					
					
					temp_edge_matrix<-rbind(temp_edge_matrix,cbind(col_A[r],col_B))
					
					
					temp_edge_matrix<-as.data.frame(temp_edge_matrix)
				       
					return(temp_edge_matrix)
				    }
				})


			}else{
			    
			    col_A<-colnames(edge_matrix)
			    
			    edge_matrix_1<-{}
			    for(r in 1:length(col_A)){
							
				for(s in 1:nrow(edge_matrix)){
				    
				    col_B<-mat_cnames[edge_matrix[s,r]]
				    edge_matrix_1<-rbind(edge_matrix_1,cbind(col_A[r],col_B))
				}
			    }
			    
			}

		}else{
		    
		    col_A<-rownames(net_result$M)
		    col_A<-col_A[edge_matrix]
			col_B<-colnames(linn.pls$Y)
		    
		    edge_matrix_1<-{}
	
		    
			edge_matrix_1<-ldply(1:length(edge_matrix),function(r){
			   
			  
			    temp_edge_matrix<-{}
			    if(length(col_B)>0){
			       
				
				
				temp_edge_matrix<-rbind(temp_edge_matrix,cbind(col_A[r],col_B))
				
				
				temp_edge_matrix<-as.data.frame(temp_edge_matrix)
			       
				return(temp_edge_matrix)
			    }
			})
		    
		}


		if(nrow(edge_matrix_1)<1){

			stop("No connections found.")
		}

	
			
				
		   # save(edge_matrix_1,file="edge_matrix_1.Rda")
		  #  save(rnames1,file="rnames1.Rda")
		   # save(simmat_colnames,file="simmat_colnames.Rda")
		  #  save(net_result,file="net_result.Rda")
		    
		 
		    g1<-graph.data.frame(edge_matrix_1,directed=FALSE) #net_result$gR
		   
		    
		    g2<-get.edgelist(g1)
		    
		    weight_vec<-{}
		  
		    
		    if(ncol_sim_mat>1){
			    weight_vec<-lapply(1:dim(g2)[1],function(rnum){
				
				X_name<-which(rnames1==g2[rnum,1]) #as.numeric(as.character(gsub(g2[rnum,1],pattern=Xname,replacement="")))
				Y_name<-which(simmat_colnames==g2[rnum,2]) #as.numeric(as.character(gsub(g2[rnum,2],pattern=Yname,replacement="")))
				
				
				return(net_result$M[X_name,Y_name])
			    })
		    }else{
			
				  weight_vec<-lapply(1:dim(g2)[1],function(rnum){
				
				X_name<-which(rnames1==g2[rnum,1]) #as.numeric(as.character(gsub(g2[rnum,1],pattern=Xname,replacement="")))
				Y_name<-which(simmat_colnames==g2[rnum,2]) #as.numeric(as.character(gsub(g2[rnum,2],pattern=Yname,replacement="")))
				
				
				return(net_result$M[X_name,Y_name])
			    })
		    }
		    weight_vec<-unlist(weight_vec)
		    
		    df<-data.frame(from=g2[,1],to=g2[,2],weight=weight_vec)
		    
		    rownames(x)<-as.character(rnames_ind2)
		    colnames(x)<-as.character(matrixB_1_simmat)
		    
    


    

    
    if(plot.pairwise==TRUE)
    {
        
        fname<-paste(filename,"_association_network_threshold",corthresh,".png",sep="")
        
        png(fname,width=8,height=8,res=600,type="cairo",units="in")
        
        save(df,net_node_colors,net_node_shape,fname,seednum,Xname,Yname,file="graphobj.Rda")
        #pdf("network_all.png")
        par_rows=1
        par(mfrow=c(par_rows,1))
        
        res1<-plot_graph(df=df,net_node_colors=net_node_colors,net_node_shape=net_node_shape,graphmethod="radial",filename=fname,maxnodesperclass=NA,seednum=seednum,Xname=Xname,Yname=Yname,classname=NA)
        #mtext("(Edges) Red: +ve correlation; Blue: -ve correlation",line=1,side=1,cex=0.8,adj=0)
        
        #mtext_community<-paste("(Nodes) Rectangle: ",Xname,"; Circle: ",Yname,sep="")
        
        #mtext(mtext_community,side=1,cex=0.8,line=2,adj=0)
        
        
        try(mtext(fname,line=3,cex=0.6,col="brown",side=1,adj=0),silent=TRUE)
        
        dev.off()
    }
    
    rm(g1)
    rm(g2)
    
    }
}


    rm(xtemp)
    rm(xtemp1)

    
    
	return(list(graphobject=df,rownames_vec=rnames1,colnames_vec=simmat_colnames,cormatrix=x,corthresh=corthresh,id_mapping_mat=id_mapping_mat,numcomps=numcomps))

}
