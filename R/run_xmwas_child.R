run_xmwas_child <-
function(Xome_data=NA,Yome_data=NA,Zome_data=NA,Wome_data=NA,outloc=NA,classlabels=NA,class_fname=NA,xome_fname=NA,yome_fname=NA,zome_fname=NA,wome_fname=NA,xmwasmethod="pls",plsmode="regression",max_xvar=10000,max_yvar=10000,max_zvar=10000,max_wvar=10000,rsd.filt.thresh=1,corthresh=0.4,keepX=100,keepY=100,keepZ=100,keepW=100,pairedanalysis=FALSE,optselect=TRUE,rawPthresh=0.05,numcomps=10,net_edge_colors=c("blue","red"),net_node_colors=c("orange", "green","blue","purple"),Xname="X",Yname="Y",Zname="Z",Wname="W",net_node_shape=c("circle","rectangle","triangle","star"),all.missing.thresh=0.3,maxnodesperclass=100,seednum=100,label.cex=0.3,vertex.size=6,graphclustering=FALSE,interactive=TRUE,max_connections=1000,classname=NA,centrality_method="betweenness",use.X.reference=FALSE,removeRda=TRUE,design=NA){

suppressWarnings(dir.create(outloc))
setwd(outloc)

if(is.na(xome_fname)==FALSE){
    Xome_data<-read.table(xome_fname,sep="\t",header=TRUE)
}

    rnames<-rownames(Xome_data) #Xome_data[,c(1)]
    if(length(which(duplicated(rnames)==TRUE))>0){
  
	Xome_data<-Xome_data[-which(duplicated(rnames)==TRUE),] 
	rnames<-rnames[-which(duplicated(rnames)==TRUE)]
   }
    rownames(Xome_data)<-rnames #Xome_data[,c(1)]
    #Xome_data<-Xome_data[,-c(1)]
    Xome_data<-as.data.frame(Xome_data)
    if(nrow(Xome_data)>1){
    Xome_data<-apply(Xome_data,2,as.numeric)
    rownames(Xome_data)<-rnames
	}else{
		Xome_data<-as.numeric(Xome_data)
		Xome_data<-as.data.frame(Xome_data)
	}
    	rownames(Xome_data)<-rnames
	 
num_replicates<-1

if(is.na(yome_fname)==FALSE){
Yome_data<-read.table(yome_fname,sep="\t",header=TRUE)
}

#print(dim(Yome_data))
	rnames<-rownames(Yome_data) #Yome_data[,c(1)]
    if(length(which(duplicated(rnames)==TRUE))>0){
   
	Yome_data<-Yome_data[-which(duplicated(rnames)==TRUE),] 
        rnames<-rnames[-which(duplicated(rnames)==TRUE)]
     }


    rownames(Yome_data)<-rnames
#Yome_data<-Yome_data[,-c(1)]
Yome_data<-as.data.frame(Yome_data)
if(nrow(Yome_data)>1){
    Yome_data<-apply(Yome_data,2,as.numeric)
        }else{
                Yome_data<-as.numeric(Yome_data)
                Yome_data<-as.data.frame(Yome_data)
        }
	rownames(Yome_data)<-rnames
if(is.na(zome_fname)==FALSE){
    
    Zome_data<-read.table(zome_fname,sep="\t",header=TRUE)
}
suppressWarnings(
   if(is.na(Zome_data)==FALSE){ 
    rnames<-rownames(Zome_data) #Zome_data[,c(1)]
    if(length(which(duplicated(rnames)==TRUE))>0){
   	Zome_data<-Zome_data[-which(duplicated(rnames)==TRUE),] 
        rnames<-rnames[-which(duplicated(rnames)==TRUE)]
   }
    rownames(Zome_data)<-rnames
    #Zome_data<-Zome_data[,-c(1)]
    Zome_data<-as.data.frame(Zome_data)
		
	if(nrow(Zome_data)>1){
   		 Zome_data<-apply(Zome_data,2,as.numeric)
        }else{
                Zome_data<-as.numeric(Zome_data)
                Zome_data<-as.data.frame(Zome_data)
        }
	rownames(Zome_data)<-rnames

    })

if(is.na(wome_fname)==FALSE){
    
    Wome_data<-read.table(wome_fname,sep="\t",header=TRUE)
}

suppressWarnings(
	if(is.na(Wome_data)==FALSE){    
    rnames<-rownames(Wome_data) #Wome_data[,c(1)]
    if(length(which(duplicated(rnames)==TRUE))>0){
   
	Wome_data<-Wome_data[-which(duplicated(rnames)==TRUE),] 
        rnames<-rnames[-which(duplicated(rnames)==TRUE)]
     }
    rownames(Wome_data)<-rnames
    #rownames(Wome_data)<-Wome_data[,c(1)]
    #Wome_data<-Wome_data[,-c(1)]
    Wome_data<-as.data.frame(Wome_data)
		
	if(nrow(Wome_data)>1){
    		Wome_data<-apply(Wome_data,2,as.numeric)
        }else{
                Wome_data<-as.numeric(Wome_data)
                Wome_data<-as.data.frame(Wome_data)
        }	
	rownames(Wome_data)<-rnames
    })

suppressWarnings(
if(is.na(Xome_data)==TRUE){
    
    stop("X data matrix is required.")
})

if(is.na(class_fname)==FALSE){

classlabels<-read.table(class_fname,sep="\t",header=TRUE)
cnames_class<-colnames(classlabels)
cnames_class<-tolower(cnames_class)

colnames(classlabels)<-cnames_class




}

cnames<-colnames(classlabels)
#classlabels_vec<-classlabels$class

class_check1<-which(cnames=="class")

if(length(class_check1)<1){
    
    class_levels<-levels(classlabels$factor1)
    
    classlabels_vec<-classlabels$factor1
    

}else{
    class_levels<-levels(classlabels$class)
    classlabels_vec<-classlabels$class
    

}




classlabels<-classlabels[,-c(1)]

print("###################")
print("###################")

if(is.na(classname)==TRUE){
    print(paste("Analyzing all samples",sep=""))
    #print(classname)
}else{
    print(paste("Analyzing class: ",classname,sep=""))
    
}




classlabels<-as.data.frame(classlabels)

if(pairedanalysis==TRUE){
    
    classlabels<-design

}
print("run_xmwas_child")
print(head(classlabels))

suppressWarnings(
if(is.na(classname)==FALSE && is.na(classlabels)==FALSE){
    
    Xome_data<-Xome_data[,which(classlabels_vec==classname)]
    Yome_data<-Yome_data[,which(classlabels_vec==classname)]
    
    classlabels<-classlabels[which(classlabels_vec==classname),]

    
    suppressWarnings(
    if(is.na(Zome_data)==FALSE){
    Zome_data<-Zome_data[,which(classlabels_vec==classname)]
    })
    
    suppressWarnings(
    if(is.na(Wome_data)==FALSE){
    Wome_data<-Wome_data[,which(classlabels_vec==classname)]
    })
})



if(num_replicates>1){
avg_data5<-data_preprocess(feature_table_file=xome_fname,class_labels_file=classdesign,parentoutput_dir=outloc,
num_replicates=num_replicates,feat.filt.thresh=NA,summarize.replicates=TRUE,summary.method="median",
all.missing.thresh=all.missing.thresh,group.missing.thresh=NA,log2transform=TRUE,medcenter=FALSE,znormtransform=FALSE,
quantile_norm=FALSE,lowess_norm=FALSE,madscaling=FALSE,missing.val=0,samplermindex=NA, 
rep.max.missing.thresh=0.5,summary.na.replacement="zeros")

Xome_data<-avg_data5$data_matrix_afternorm_scaling
Yome_data<-Yome_data[,c(1,seq(2,ncol(Yome_data),num_replicates))]
write.table(Yome_data,file="../Stage1/Ymatrix_averaged.txt",sep="\t")
    if(is.na(zome_fname)==FALSE){
        Zome_data<-Zome_data[,c(1,seq(2,ncol(Zome_data),num_replicates))]
        write.table(Zome_data,file="../Stage1/Zmatrix_averaged.txt",sep="\t")
    }
    if(is.na(wome_fname)==FALSE){
        Wome_data<-Wome_data[,c(1,seq(2,ncol(Wome_data),num_replicates))]
        write.table(Wome_data,file="../Stage1/Wmatrix_averaged.txt",sep="\t")
    }
}
colors_sel_vec<-net_node_colors


#############################Parameters to change#########################

outloc_pairwise<-paste(outloc,"/pairwise_results/",sep="")
suppressWarnings(dir.create(outloc_pairwise))

suppressWarnings(
check_file<-try(load("g1.Rda"),silent=TRUE)
)
if(is(check_file,"try-error")){

g1<-get_pairwise_cor(Xome_data,Yome_data,max_xvar=max_xvar,max_yvar=max_yvar,rsd.filt.thresh=rsd.filt.thresh,corthresh=corthresh,keepX=keepX,keepY=keepY,pairedanalysis=pairedanalysis,optselect=optselect,classlabels=classlabels,rawPthresh=rawPthresh,plsmode=plsmode,xmwasmethod=xmwasmethod,numcomps=numcomps,net_edge_colors=net_edge_colors,net_node_colors=net_node_colors[c(1,2)],outloc=outloc_pairwise,Xname=Xname,Yname=Yname,net_node_shape=net_node_shape[c(1,2)],seednum=seednum)

save(g1,file="g1.Rda")
}else{
    
    load("g1.Rda")
}

    id_mapping_mat<-g1$id_mapping_mat
    
    suppressWarnings(
    if(is.na(Zome_data)==FALSE){

suppressWarnings(check_file<-try(load("g2.Rda"),silent=TRUE))
    if(is(check_file,"try-error")){
        g2<-get_pairwise_cor(Xome_data,Zome_data,max_xvar=max_xvar,max_yvar=max_zvar,rsd.filt.thresh=rsd.filt.thresh,corthresh=corthresh,keepX=keepX,keepY=keepZ,pairedanalysis=pairedanalysis,optselect=optselect,classlabels=classlabels,rawPthresh=rawPthresh,plsmode=plsmode,xmwasmethod=xmwasmethod,numcomps=numcomps,net_edge_colors=net_edge_colors,net_node_colors=net_node_colors[c(1,3)],outloc=outloc_pairwise,Xname=Xname,Yname=Zname,net_node_shape=net_node_shape[c(1,2)],seednum=seednum)
    
        save(g2,file="g2.Rda")
    }else{
    
        load("g2.Rda")
    }

        id_mapping_mat<-rbind(id_mapping_mat,g2$id_mapping_mat)
        
        if(use.X.reference==FALSE){
            
            suppressWarnings(
        check_file<-try(load("g3.Rda"),silent=TRUE)
        )
        if(is(check_file,"try-error")){
            g3<-get_pairwise_cor(Yome_data,Zome_data,max_xvar=max_xvar,max_yvar=max_wvar,rsd.filt.thresh=rsd.filt.thresh,corthresh=corthresh,keepX=keepX,keepY=keepW,pairedanalysis=pairedanalysis,optselect=optselect,classlabels=classlabels,rawPthresh=rawPthresh,plsmode=plsmode,xmwasmethod=xmwasmethod,numcomps=numcomps,net_edge_colors=net_edge_colors,net_node_colors=net_node_colors[c(2,3)],outloc=outloc_pairwise,Xname=Yname,Yname=Zname,net_node_shape=net_node_shape[c(1,2)],seednum=seednum)
            save(g3,file="g3.Rda")
    
        }else{
            
            load("g3.Rda")
        }
        id_mapping_mat<-rbind(id_mapping_mat,g3$id_mapping_mat)
        
        } ###


    })

suppressWarnings(
    if(is.na(Wome_data)==FALSE){
        
        suppressWarnings(
        check_file<-try(load("g4.Rda"),silent=TRUE)
        )
        
        if(is(check_file,"try-error")){
         g4<-get_pairwise_cor(Xome_data,Wome_data,max_xvar=max_xvar,max_yvar=max_wvar,rsd.filt.thresh=rsd.filt.thresh,corthresh=corthresh,keepX=keepX,keepY=keepW,pairedanalysis=pairedanalysis,optselect=optselect,classlabels=classlabels,rawPthresh=rawPthresh,plsmode=plsmode,xmwasmethod=xmwasmethod,numcomps=numcomps,net_edge_colors=net_edge_colors,net_node_colors=net_node_colors[c(1,4)],outloc=outloc_pairwise,Xname=Xname,Yname=Wname,net_node_shape=net_node_shape[c(1,2)],seednum=seednum)
         save(g4,file="g4.Rda")
        }else{
            
            load("g4.Rda")
        }

         id_mapping_mat<-rbind(id_mapping_mat,g4$id_mapping_mat)


if(use.X.reference==FALSE){
    
    suppressWarnings(
check_file<-try(load("g5.Rda"),silent=TRUE)
)
if(is(check_file,"try-error")){
    g5<-get_pairwise_cor(Yome_data,Wome_data,max_xvar=max_xvar,max_yvar=max_wvar,rsd.filt.thresh=rsd.filt.thresh,corthresh=corthresh,keepX=keepX,keepY=keepW,pairedanalysis=pairedanalysis,optselect=optselect,classlabels=classlabels,rawPthresh=rawPthresh,plsmode=plsmode,xmwasmethod=xmwasmethod,numcomps=numcomps,net_edge_colors=net_edge_colors,net_node_colors=net_node_colors[c(2,4)],outloc=outloc_pairwise,Xname=Yname,Yname=Wname,net_node_shape=net_node_shape[c(1,2)],seednum=seednum)
    save(g5,file="g5.Rda")
}else{
    
    load("g5.Rda")
}

id_mapping_mat<-rbind(id_mapping_mat,g5$id_mapping_mat)


suppressWarnings(
check_file<-try(load("g6.Rda"),silent=TRUE)
)
if(is(check_file,"try-error")){
    g6<-get_pairwise_cor(Zome_data,Wome_data,max_xvar=max_xvar,max_yvar=max_wvar,rsd.filt.thresh=rsd.filt.thresh,corthresh=corthresh,keepX=keepX,keepY=keepW,pairedanalysis=pairedanalysis,optselect=optselect,classlabels=classlabels,rawPthresh=rawPthresh,plsmode=plsmode,xmwasmethod=xmwasmethod,numcomps=numcomps,net_edge_colors=net_edge_colors,net_node_colors=net_node_colors[c(3,4)],outloc=outloc_pairwise,Xname=Zname,Yname=Wname,net_node_shape=net_node_shape[c(1,2)],seednum=seednum)
    save(g6,file="g6.Rda")
}else{
    
    load("g6.Rda")
}

id_mapping_mat<-rbind(id_mapping_mat,g6$id_mapping_mat)
}

    })
    
    setwd(outloc)
    
    id_mapping_mat<-unique(id_mapping_mat)
    if(corthresh<g1$corthresh){
        corthresh=g1$corthresh
    }
    suppressWarnings(
	#4-way 
    if(is.na(Zome_data)==FALSE && is.na(Wome_data)==FALSE && is.na(Yome_data)==FALSE)
    {
        
        if(use.X.reference==TRUE){
            
            df_matrix<-rbind(g1$graphobject,g2$graphobject,g4$graphobject)
            #meta_cor_matrix<-cbind(g1$cormatrix,g2$cormatrix,g3$cormatrix)
            rownames_vec<-g1$rownames_vec
            colnames_vec<-c(g1$colnames_vec,g2$colnames_vec,g4$colnames_vec)
            
        }else{
        df_matrix<-rbind(g1$graphobject,g2$graphobject,g3$graphobject,g4$graphobject,g5$graphobject,g6$graphobject)
        #meta_cor_matrix<-cbind(g1$cormatrix,g2$cormatrix,g3$cormatrix)
        rownames_vec<-g1$rownames_vec
        colnames_vec<-c(g1$colnames_vec,g2$colnames_vec,g3$colnames_vec,g4$colnames_vec,g5$colnames_vec,g6$colnames_vec)
        
        }
        
    }else{
    
    #3-way
    if(is.na(Zome_data)==FALSE && is.na(Yome_data)==FALSE && is.na(Wome_data)==TRUE){
        
        if(use.X.reference==TRUE){
            
            df_matrix<-rbind(g1$graphobject,g2$graphobject)
            #meta_cor_matrix<-cbind(g1$cormatrix,g2$cormatrix)
            rownames_vec<-g1$rownames_vec
            colnames_vec<-c(g1$colnames_vec,g2$colnames_vec)
            Wname=NA
            
        }else{
            df_matrix<-rbind(g1$graphobject,g2$graphobject,g3$graphobject)
            #meta_cor_matrix<-cbind(g1$cormatrix,g2$cormatrix)
            rownames_vec<-g1$rownames_vec
            colnames_vec<-c(g1$colnames_vec,g2$colnames_vec,g3$colnames_vec)
            Wname=NA
        }

    }else{
        
        #2-way
        if(is.na(Zome_data)==TRUE && is.na(Yome_data)==FALSE && is.na(Wome_data)==TRUE){
        df_matrix<-rbind(g1$graphobject)
        #meta_cor_matrix<-cbind(g1$cormatrix)
        rownames_vec<-g1$rownames_vec
        colnames_vec<-c(g1$colnames_vec)
        Zname=NA
        Wname=NA
       
        }

        
        
    }
    
})
    
    
    
    corweight_temp<-abs(df_matrix[,3])
    
    if(is.na(max_connections)==FALSE){
        
    if(nrow(df_matrix)>max_connections){
        ind_top_2000<-order(corweight_temp,decreasing=TRUE)[1:max_connections]
        print(paste("Only ",max_connections," connections can be plotted.",sep=""))
        df_matrix<-df_matrix[ind_top_2000,]
        
    }
    
    }
    
    if(is.na(corthresh)==FALSE){
        
        df_matrix<-df_matrix[which(corweight_temp>corthresh),]
        
        if(nrow(df_matrix)<1){
            stop("Please lower the correlation threshold.")
        }
    }
    
	   save(df_matrix,file="df_matrix.Rda")
       
       fname<-paste("res",corthresh,".Rda",sep="")
       suppressWarnings(
       check_file<-try(load(fname),silent=TRUE))
       
       
       if(FALSE){
       fname<-paste("Multiome_Network_corthresh",corthresh,"top",maxnodesperclass,".png",sep="")
       
       pdf(fname)
       plot_graph(df=df_matrix,net_node_colors=net_node_colors,graphmethod="radial",filename=paste("MultiOme_Network_corthresh",corthresh,sep=""),maxnodesperclass=maxnodesperclass,seednum=seednum,label.cex=label.cex,vertex.size=vertex.size,interactive=FALSE)
       
       dev.off()
       
       }

       if(is(check_file,"try-error")){
       fname<-paste("Multiome_Network_corthresh",corthresh,".png",sep="")
       
       png(fname,width=8,height=8,res=600,type="cairo",units="in")
       
       #pdf(fname)
       res1<-plot_graph(df=df_matrix,net_node_colors=net_node_colors,graphmethod="radial",filename=paste("MultiOme_Network_corthresh",corthresh,sep=""),maxnodesperclass=NA,seednum=seednum,label.cex=label.cex,vertex.size=vertex.size,interactive=interactive)
       dev.off()
       
       save(res1,file="res1.Rda")
       }else{
           load("res1.Rda")
       }
       
       clust_id_mapping_mat=NA
       
       if(FALSE){
        rnames<-rownames(Xome_data)
       clust_id_mapping_mat<-cbind(rnames,-1)
       
       clust_id_mapping_mat<-as.data.frame(clust_id_mapping_mat)
       
       clust_id_mapping_mat<-colnames("Name","Centrality")
       }
       
       if(graphclustering=="highlyconnSG"){
           get_sub_clusters(df=df_matrix,net_node_colors=net_node_colors,rownames_vec=rownames_vec,colnames_vec=colnames_vec,Xname=Xname,Yname=Yname,Zname=Zname,Wname=Wname,sat=1,cormatrix=NA,seednum=seednum,corthresh=corthresh)
       }else{
           if(graphclustering==TRUE){
               
               print("Performing community detection analysis")
               
               sg<-res1$sg
               wc <- multilevel.community(sg,weights=NA) #walktrap.community(sg)
               
               
               print(paste("Modularity measure: ",round(modularity(wc),2),sep=""))
               clust_membership<-membership(wc)
               clust_membership<-ldply(clust_membership,rbind)
               clust_membership<-as.data.frame(clust_membership)
               
               fname<-paste("MultiOme_Network_corthresh",corthresh,"_communities.png",sep="")
               
               
               save(wc,file="wc.Rda")
               save(sg,file="sg1.Rda")
               colnames(id_mapping_mat)<-c("Name","Node")
               colnames(clust_membership)<-c("Node","Cluster")
               clust_id_mapping_mat<-merge(clust_membership,id_mapping_mat,by="Node")
               #write.table(clust_id_mapping_mat,file="cluster_membership_mapped.txt",sep="\t",row.names=FALSE)
               
               #pdf(fname)
               png(fname,width=8,height=8,res=600,type="cairo",units="in")
               
               # clips as a circle
               add_shape("triangle",plot=mytriangle) #, clip=shapes("circle")$clip,plot=mytriangle)
               # no clipping, edges will be below the vertices anyway
               add_shape("star", clip=shape_noclip,plot=mystar, parameters=list(vertex.norays=5))
               #try(plot(wc,sg,layout=layout_with_fr,vertex.size=vertex.size),silent=TRUE)
               
               set.seed(seednum)
               sg$layout <- layout_with_fr
               set.seed(seednum)
               
               set.seed(seednum)
               try(plot(sg,vertex.color=membership(wc),layout=layout_with_fr,vertex.size=vertex.size),silent=TRUE)
               
               mtext("Red: +ve correlation; Blue: -ve correlation",side=1)
               dev.off()
               #write.table(clust_membership,file="cluster_membership.txt",sep="\t",row.names=FALSE)
               
               if(centrality_method=="eigenvector"){
               eigenvector_centrality<-eigen_centrality(sg,directed=FALSE,weights=E(sg)$weight)
               centrality_vec<-eigenvector_centrality$vector
               
               cent_vec_mat<-cbind(names(centrality_vec),centrality_vec)
               
               cent_vec_mat<-as.data.frame(cent_vec_mat)
               
               cent_vec_mat<-cent_vec_mat[order(cent_vec_mat[,1]),]
               
               cent_vec_mat$centrality_vec<-as.numeric(as.character(cent_vec_mat$centrality_vec))
               
               
               centrality_vec<-round(cent_vec_mat$centrality_vec,3)
               }else{
                    if(centrality_method=="betweenness"){
                        centrality_vec<-betweenness(sg,directed=FALSE,weights=abs(E(sg)$weight),normalized=TRUE)
                        #save(centrality_vec,file="centrality_vec.Rda")
                        #centrality_vec1<-centrality_vec/max(centrality_vec,na.rm=TRUE)
                        
                        #centrality_vec<-as.numeric(as.character(cent_vec_mat$centrality_vec))
                        
                        centrality_vec<-scale(centrality_vec)
                        
                        cent_vec_mat<-cbind(names(centrality_vec),centrality_vec)
                        
                        cent_vec_mat<-as.data.frame(cent_vec_mat)

                        cent_vec_mat<-cent_vec_mat[order(cent_vec_mat[,1]),]
                        
                        cent_vec_mat$centrality_vec<-as.numeric(as.character(cent_vec_mat$centrality_vec))
                        
                        centrality_vec<-round(cent_vec_mat$centrality_vec,3)
                    }
               }
               colnames(id_mapping_mat)<-c("Name","Node")
               colnames(cent_vec_mat)<-c("Node","Centrality")
               #cent_vec_mat<-merge(cent_vec_mat,id_mapping_mat,by="Node")
               #write.table(cent_vec_mat,file="cent_vec_mat.txt",sep="\t",row.names=FALSE)
               
               #save(eigenvector_centrality,file="centrality_mat.Rda")
               clust_id_mapping_mat<-clust_id_mapping_mat[order(clust_id_mapping_mat$Node),]
               
               clust_id_mapping_mat<-cbind(clust_id_mapping_mat,centrality_vec)
               
               clust_id_mapping_mat<-as.data.frame(clust_id_mapping_mat)
               
               write.table(clust_id_mapping_mat,file="cluster_membership_centrality_table.txt",sep="\t",row.names=FALSE)
               
               
           }
           
       }


#colnames(id_mapping_mat)<-c("Name","Node")
#colnames(clust_membership)<-c("Node","Cluster")
#clust_id_mapping_mat<-merge(clust_membership,id_mapping_mat,by="Node")
#write.table(clust_id_mapping_mat,file="cluster_membership_mapped.txt",sep="\t",row.names=FALSE)



if(is.na(classname)==TRUE){
rnames<-rownames(Xome_data)
if(length(which(rnames%in%clust_id_mapping_mat$Name))>0){
        Xome_data<-Xome_data[which(rnames%in%clust_id_mapping_mat$Name),]
}

rnames<-rownames(Yome_data)
if(length(which(rnames%in%clust_id_mapping_mat$Name))>0){
    Yome_data<-Yome_data[which(rnames%in%clust_id_mapping_mat$Name),]
}
suppressWarnings(
if(is.na(Zome_data)==FALSE){
rnames<-rownames(Zome_data)
if(length(which(rnames%in%clust_id_mapping_mat$Name))>0){
    Zome_data<-Zome_data[which(rnames%in%clust_id_mapping_mat$Name),]
}

})
suppressWarnings(
if(is.na(Wome_data)==FALSE){
    rnames<-rownames(Wome_data)
    if(length(which(rnames%in%clust_id_mapping_mat$Name))>0){
        Wome_data<-Wome_data[which(rnames%in%clust_id_mapping_mat$Name),]
    }
    
})

}
#return(sg)

if(removeRda==TRUE){
unlink("*.Rda",force=TRUE,recursive=TRUE)
unlink("pairwise_results/*.Rda",force=TRUE,recursive=TRUE)

}

return(list("graph"=sg,"network_analysis"=clust_id_mapping_mat,"Xome_data"=Xome_data,"Yome_data"=Yome_data,"Zome_data"=Zome_data,"Wome_data"=Wome_data,"classname"=classname))
}
