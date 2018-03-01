run_xmwas_child <-
function(Xome_data=NA,Yome_data=NA,Zome_data=NA,Wome_data=NA,outloc=NA,classlabels=NA,class_fname=NA,xome_fname=NA,yome_fname=NA,zome_fname=NA,wome_fname=NA,xmwasmethod="pls",plsmode="regression",max_xvar=10000,max_yvar=10000,max_zvar=10000,max_wvar=10000,rsd.filt.thresh=1,corthresh=0.4,keepX=100,keepY=100,keepZ=100,keepW=100,pairedanalysis=FALSE,optselect=TRUE,rawPthresh=0.05,numcomps=10,net_edge_colors=c("blue","red"),net_node_colors=c("orange", "green","blue","pink"),Xname="X",Yname="Y",Zname="Z",Wname="W",net_node_shape=c("circle","rectangle","triangle","star"),all.missing.thresh=0.3,maxnodesperclass=100,seednum=100,label.cex=0.3,vertex.size=6,graphclustering=FALSE,interactive=TRUE,max_connections=1000,classname=NA,centrality_method="eigenvector",use.X.reference=FALSE,removeRda=TRUE,design=NA,community.node.color.palette="categorical",missing.val=0){

suppressWarnings(dir.create(outloc))
setwd(outloc)

if(is.na(classname)==TRUE){
    print(paste("Analyzing all samples",sep=""))
    #print(classname)
    
    fname_id_mapping<-paste("NodeID_Name_mapping.txt",sep="")
    
}else{
    print(paste("Analyzing class: ",classname,sep=""))
    fname_id_mapping<-paste("NodeID_Name_mapping_",classname,".txt",sep="")
}


if(is.na(xome_fname)==FALSE){
    Xome_data<-read.table(xome_fname,sep="\t",header=TRUE)
}

    rnames<-rownames(Xome_data)
    if(length(which(duplicated(rnames)==TRUE))>0){
  
	Xome_data<-Xome_data[-which(duplicated(rnames)==TRUE),,drop=FALSE]
	rnames<-rnames[-which(duplicated(rnames)==TRUE)]
   }
    rownames(Xome_data)<-rnames
    
    Xome_data<-as.data.frame(Xome_data)
    
    if(nrow(Xome_data)>1){
        Xome_data<-apply(Xome_data,2,as.numeric)
        rownames(Xome_data)<-rnames
	
    }
    
    rownames(Xome_data)<-rnames
    
    
    num_samps<-dim(Xome_data)[2]
    print(paste("Number of samples: ",num_samps),sep="")
   
   
  
	 
        num_replicates<-1

        if(is.na(yome_fname)==FALSE){
                Yome_data<-read.table(yome_fname,sep="\t",header=TRUE)
        }


	rnames<-rownames(Yome_data)
    
    if(length(which(duplicated(rnames)==TRUE))>0){
   
        Yome_data<-Yome_data[-which(duplicated(rnames)==TRUE),,drop=FALSE]
        rnames<-rnames[-which(duplicated(rnames)==TRUE)]
        
     }


    rownames(Yome_data)<-rnames

Yome_data<-as.data.frame(Yome_data)
if(nrow(Yome_data)>1){
    Yome_data<-apply(Yome_data,2,as.numeric)
        }
	rownames(Yome_data)<-rnames


    
if(is.na(zome_fname)==FALSE){
    
    Zome_data<-read.table(zome_fname,sep="\t",header=TRUE)
}
suppressWarnings(
   if(is.na(Zome_data)==FALSE){ 
    rnames<-rownames(Zome_data)
    if(length(which(duplicated(rnames)==TRUE))>0){
   	Zome_data<-Zome_data[-which(duplicated(rnames)==TRUE),,drop=FALSE]
    rnames<-rnames[-which(duplicated(rnames)==TRUE)]
   }
    rownames(Zome_data)<-rnames
    
    Zome_data<-as.data.frame(Zome_data)
		
	if(nrow(Zome_data)>1){
   		 Zome_data<-apply(Zome_data,2,as.numeric)
        }
	rownames(Zome_data)<-rnames
 
   
    })

if(is.na(wome_fname)==FALSE){
    
    Wome_data<-read.table(wome_fname,sep="\t",header=TRUE)
}

suppressWarnings(
	if(is.na(Wome_data)==FALSE){    
    rnames<-rownames(Wome_data)
    if(length(which(duplicated(rnames)==TRUE))>0){
   
	Wome_data<-Wome_data[-which(duplicated(rnames)==TRUE),,drop=FALSE]
        rnames<-rnames[-which(duplicated(rnames)==TRUE)]
     }
    rownames(Wome_data)<-rnames
    
    Wome_data<-as.data.frame(Wome_data)
		
	if(nrow(Wome_data)>1){
    		Wome_data<-apply(Wome_data,2,as.numeric)
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






classlabels<-as.data.frame(classlabels)

if(pairedanalysis==TRUE){
    
    classlabels<-design

}

suppressWarnings(
if(is.na(classname)==FALSE && is.na(classlabels)==FALSE){
    
    rnames_x<-rownames(Xome_data)
    Xome_data<-Xome_data[,which(classlabels_vec==classname),drop=FALSE]
    rownames(Xome_data)<-rnames_x
    
    Yome_data<-Yome_data[,which(classlabels_vec==classname),drop=FALSE]
    
    classlabels<-classlabels[which(classlabels_vec==classname),,drop=FALSE]

    
    suppressWarnings(
    if(is.na(Zome_data)==FALSE){
    Zome_data<-Zome_data[,which(classlabels_vec==classname),drop=FALSE]
    })
    
    suppressWarnings(
    if(is.na(Wome_data)==FALSE){
    Wome_data<-Wome_data[,which(classlabels_vec==classname),drop=FALSE]
    })
})




colors_sel_vec<-net_node_colors


#############################Parameters to change#########################

outloc_pairwise<-paste(outloc,"/pairwise_results/",sep="")
suppressWarnings(dir.create(outloc_pairwise))

suppressWarnings(
check_file<-try(load("g1.Rda"),silent=TRUE)
)
if(is(check_file,"try-error")){

#XxY
g1<-get_pairwise_cor(Xome_data,Yome_data,max_xvar=max_xvar,max_yvar=max_yvar,rsd.filt.thresh=rsd.filt.thresh,corthresh=corthresh,keepX=keepX,keepY=keepY,pairedanalysis=pairedanalysis,optselect=optselect,classlabels=classlabels,rawPthresh=rawPthresh,plsmode=plsmode,xmwasmethod=xmwasmethod,numcomps=numcomps,net_edge_colors=net_edge_colors,net_node_colors=net_node_colors[c(1,2)],outloc=outloc_pairwise,Xname=Xname,Yname=Yname,net_node_shape=net_node_shape[c(1,2)],seednum=seednum,tempXname="X",tempYname="Y",all.missing.thresh=all.missing.thresh,missing.val=missing.val)

save(g1,file="g1.Rda")
}else{
    
    load("g1.Rda")
}

    id_mapping_mat<-g1$id_mapping_mat
    
    suppressWarnings(
    if(is.na(Zome_data)==FALSE){

suppressWarnings(check_file<-try(load("g2.Rda"),silent=TRUE))
    if(is(check_file,"try-error")){
        
        #XxZ
        g2<-get_pairwise_cor(Xome_data,Zome_data,max_xvar=max_xvar,max_yvar=max_zvar,rsd.filt.thresh=rsd.filt.thresh,corthresh=corthresh,keepX=keepX,keepY=keepZ,pairedanalysis=pairedanalysis,optselect=optselect,classlabels=classlabels,rawPthresh=rawPthresh,plsmode=plsmode,xmwasmethod=xmwasmethod,numcomps=numcomps,net_edge_colors=net_edge_colors,net_node_colors=net_node_colors[c(1,3)],outloc=outloc_pairwise,Xname=Xname,Yname=Zname,net_node_shape=net_node_shape[c(1,2)],seednum=seednum,tempXname="X",tempYname="Z",all.missing.thresh=all.missing.thresh,missing.val=missing.val)
    
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
            
            #YxZ
            g3<-get_pairwise_cor(Yome_data,Zome_data,max_xvar=max_yvar,max_yvar=max_zvar,rsd.filt.thresh=rsd.filt.thresh,corthresh=corthresh,keepX=keepY,keepY=keepZ,pairedanalysis=pairedanalysis,optselect=optselect,classlabels=classlabels,rawPthresh=rawPthresh,plsmode=plsmode,xmwasmethod=xmwasmethod,numcomps=numcomps,net_edge_colors=net_edge_colors,net_node_colors=net_node_colors[c(2,3)],outloc=outloc_pairwise,Xname=Yname,Yname=Zname,net_node_shape=net_node_shape[c(1,2)],seednum=seednum,tempXname="Y",tempYname="Z",all.missing.thresh=all.missing.thresh,missing.val=missing.val)
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
  
    #XxW
  g4<-get_pairwise_cor(Xome_data,Wome_data,max_xvar=max_xvar,max_yvar=max_wvar,rsd.filt.thresh=rsd.filt.thresh,corthresh=corthresh,keepX=keepX,keepY=keepW,pairedanalysis=pairedanalysis,optselect=optselect,classlabels=classlabels,rawPthresh=rawPthresh,plsmode=plsmode,xmwasmethod=xmwasmethod,numcomps=numcomps,net_edge_colors=net_edge_colors,net_node_colors=net_node_colors[c(1,4)],outloc=outloc_pairwise,Xname=Xname,Yname=Wname,net_node_shape=net_node_shape[c(1,2)],seednum=seednum,tempXname="X",tempYname="W",all.missing.thresh=all.missing.thresh,missing.val=missing.val)
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

#YxW
g5<-get_pairwise_cor(Yome_data,Wome_data,max_xvar=max_yvar,max_yvar=max_wvar,rsd.filt.thresh=rsd.filt.thresh,corthresh=corthresh,keepX=keepY,keepY=keepW,pairedanalysis=pairedanalysis,optselect=optselect,classlabels=classlabels,rawPthresh=rawPthresh,plsmode=plsmode,xmwasmethod=xmwasmethod,numcomps=numcomps,net_edge_colors=net_edge_colors,net_node_colors=net_node_colors[c(2,4)],outloc=outloc_pairwise,Xname=Yname,Yname=Wname,net_node_shape=net_node_shape[c(1,2)],seednum=seednum,tempXname="Y",tempYname="W",all.missing.thresh=all.missing.thresh,missing.val=missing.val)
    save(g5,file="g5.Rda")
}else{
    
    load("g5.Rda")
}

id_mapping_mat<-rbind(id_mapping_mat,g5$id_mapping_mat)


suppressWarnings(
check_file<-try(load("g6.Rda"),silent=TRUE)
)
if(is(check_file,"try-error")){
    #ZxW
g6<-get_pairwise_cor(Zome_data,Wome_data,max_xvar=max_zvar,max_yvar=max_wvar,rsd.filt.thresh=rsd.filt.thresh,corthresh=corthresh,keepX=keepZ,keepY=keepW,pairedanalysis=pairedanalysis,optselect=optselect,classlabels=classlabels,rawPthresh=rawPthresh,plsmode=plsmode,xmwasmethod=xmwasmethod,numcomps=numcomps,net_edge_colors=net_edge_colors,net_node_colors=net_node_colors[c(3,4)],outloc=outloc_pairwise,Xname=Zname,Yname=Wname,net_node_shape=net_node_shape[c(1,2)],seednum=seednum,tempXname="Z",tempYname="W",all.missing.thresh=all.missing.thresh,missing.val=missing.val)
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
            
            rownames_vec<-g1$rownames_vec
            colnames_vec<-c(g1$colnames_vec,g2$colnames_vec,g4$colnames_vec)
            
        }else{
        df_matrix<-rbind(g1$graphobject,g2$graphobject,g3$graphobject,g4$graphobject,g5$graphobject,g6$graphobject)
        
        rownames_vec<-g1$rownames_vec
        colnames_vec<-c(g1$colnames_vec,g2$colnames_vec,g3$colnames_vec,g4$colnames_vec,g5$colnames_vec,g6$colnames_vec)
        
        }
        
    }else{
    
    #3-way
    if(is.na(Zome_data)==FALSE && is.na(Yome_data)==FALSE && is.na(Wome_data)==TRUE){
        
        if(use.X.reference==TRUE){
            
            df_matrix<-rbind(g1$graphobject,g2$graphobject)
            
            rownames_vec<-g1$rownames_vec
            colnames_vec<-c(g1$colnames_vec,g2$colnames_vec)
            Wname=NA
            
        }else{
            df_matrix<-rbind(g1$graphobject,g2$graphobject,g3$graphobject)
            
            rownames_vec<-g1$rownames_vec
            colnames_vec<-c(g1$colnames_vec,g2$colnames_vec,g3$colnames_vec)
            Wname=NA
        }

    }else{
        
        #2-way
        if(is.na(Zome_data)==TRUE && is.na(Yome_data)==FALSE && is.na(Wome_data)==TRUE){
        df_matrix<-rbind(g1$graphobject)
        
        rownames_vec<-g1$rownames_vec
        colnames_vec<-c(g1$colnames_vec)
        Zname=NA
        Wname=NA
       
        }

        
        
    }
    
})
  
    
     colnames(id_mapping_mat)<-c("Name","Node")
     #colnames(id_mapping_mat)<-c("Name","Node")
     
     
    
    write.table(id_mapping_mat,file=fname_id_mapping,sep="\t",row.names=FALSE)
    
    corweight_temp<-abs(df_matrix[,3])
    
    print(paste("Total number of connections: ",nrow(df_matrix),sep=""))
    
    if(is.na(max_connections)==FALSE){
        
    if(nrow(df_matrix)>max_connections){
        ind_top_2000<-order(corweight_temp,decreasing=TRUE)[1:max_connections]
        print(paste("Only ",max_connections," connections ranked by strength of association will be plotted.",sep=""))
        df_matrix<-df_matrix[ind_top_2000,,drop=FALSE]
        
    }
    
    }
    
    if(is.na(corthresh)==FALSE){
        
        df_matrix<-df_matrix[which(corweight_temp>corthresh),,drop=FALSE]
        
        if(nrow(df_matrix)<1){
            stop("Please lower the correlation threshold.")
        }
    }
    
        df_matrix<-na.omit(df_matrix)
    
	   save(df_matrix,file="df_matrix.Rda")
       
       fname<-paste("res",corthresh,".Rda",sep="")
       suppressWarnings(
       check_file<-try(load(fname),silent=TRUE))
       
       
     


        
       if(is(check_file,"try-error")){
           
           if(is.na(classname)==TRUE){
           
               fname<-paste("Multidata_Network_threshold",corthresh,".png",sep="")
           }else{
               fname<-paste("Multidata_Network_threshold",corthresh,"_",classname,".png",sep="")
               
           }
           
      
       png(fname,width=8,height=8,res=600,type="cairo",units="in")
       
       
       if(is.na(classname)==TRUE){
           
           fname<-paste("Multidata_Network_threshold",corthresh,sep="")
       }else{
           fname<-paste("Multidata_Network_threshold",corthresh,"_",classname,sep="")
           
       }
       
       #pdf(fname)
       res1<-plot_graph(df=df_matrix,net_node_colors=net_node_colors,graphmethod="radial",filename=fname,maxnodesperclass=NA,seednum=seednum,label.cex=label.cex,vertex.size=vertex.size,interactive=interactive,Xname=Xname,Yname=Yname,Zname=Zname,Wname=Wname,classname=classname)
       dev.off()
       
       save(res1,file="res1.Rda")
       }else{
           load("res1.Rda")
       }
       
       clust_id_mapping_mat=NA
       
  
       
       graphclustering=TRUE
       
       if(graphclustering=="highlyconnSG"){
           get_sub_clusters(df=df_matrix,net_node_colors=net_node_colors,rownames_vec=rownames_vec,colnames_vec=colnames_vec,Xname=Xname,Yname=Yname,Zname=Zname,Wname=Wname,sat=1,cormatrix=NA,seednum=seednum,corthresh=corthresh)
       }else{
           if(graphclustering==TRUE){
               
               print("Performing community detection analysis")
               
               sg<-res1$sg
               wc1 <- multilevel.community(sg,weights=NA) #multilevel community detection
               mc_modularity_measure<-round(modularity(wc1),2)
               
               wc2 <- walktrap.community(sg,weights=NA) #walktrap community detection
               walktrap_modularity_measure<-round(modularity(wc2),2)
               
               if(is.na(mc_modularity_measure)==TRUE){
                   
                   mc_modularity_measure=0
               }
               
               if(is.na(walktrap_modularity_measure)==TRUE){
                   
                   walktrap_modularity_measure=0
               }
               
               
               if(mc_modularity_measure>=walktrap_modularity_measure){
                   wc<-wc1
                   modularity_measure<-round(mc_modularity_measure,2)
                   modularity_method<-"multilevel.community"
               }else{
                   wc<-wc2
                   modularity_measure<-round(walktrap_modularity_measure,2)
                   modularity_method<-"walktrap.community"
               }
               
            
               
               print(paste(modularity_method," modularity measure: ",modularity_measure,sep=""))
               
               cc <- igraph::transitivity(sg)
               
               network_stats<-cbind(cc, modularity_measure)
               colnames(network_stats)<-c("Clustering coefficient (graph level)",paste("Modularity measure (based on community membership) using ",modularity_method,sep=""))
               
               if(is.na(classname)==TRUE){
               mod_measure_fname<-paste("Network_stats.csv",sep="")
               
               fname<-paste("Multidata_Network_threshold",corthresh,"_communities.png",sep="")
               
               community_membership_fname<-"cluster_membership_centrality_table.txt"
               }else{
                   mod_measure_fname<-paste("Network_stats_",classname,".csv",sep="")
                   
                   fname<-paste("Multidata_Network_threshold",corthresh,"_communities_",classname,".png",sep="")
                   
                   community_membership_fname<-paste("cluster_membership_centrality_",classname,"_table.txt",sep="")
               }
               
               write.csv(network_stats,file=mod_measure_fname,row.names=FALSE)
               
               clust_membership<-membership(wc)
               clust_membership<-ldply(clust_membership,rbind)
               clust_membership<-as.data.frame(clust_membership)
               
               #fname<-paste("Multidata_Network_threshold",corthresh,"_communities.png",sep="")
               
               
               save(wc,file="wc.Rda")
               save(sg,file="sg1.Rda")
              
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
               
               l = layout.fruchterman.reingold(sg, weights = (1 -abs(E(sg)$weight)))
               
               set.seed(seednum)
               sg$layout <- l #layout_with_fr
               set.seed(seednum)
               
               set.seed(seednum)
               
               if(length(wc)>1){
               if(community.node.color.palette=="categorical"){
                   #sg$palette <- c(categorical_pal(length(wc)))
                   sg$palette <- c(categorical_pal(8),diverging_pal(11),sequential_pal(9))[1:length(wc)]
               }else{
                   if(community.node.color.palette=="heat"){
                       sg$palette <- heat.colors(n=length(wc),alpha=1)
                   }else{
                       if(community.node.color.palette=="topo"){
                           sg$palette <- topo.colors(n=length(wc),alpha=1)
                       }else{
                           
                           if(community.node.color.palette=="rainbow"){
                               sg$palette <- rainbow(n=length(wc),alpha=1) #c(categorical_pal(8),sequential_pal(9),diverging_pal(11))[1:length(wc)]
                               }
                       }
                       
                   }
               }
               
               }else{
                   sg$palette <-c(categorical_pal(8))
                   
               } #c(categorical_pal(length(wc))[1],categorical_pal(length(cl_k))[1])
               #sg$palette <- heat.colors(n=length(wc),alpha=1) #c(categorical_pal(8),diverging_pal(11),sequential_pal(9))  #c(categorical_pal(length(wc))[1],categorical_pal(length(cl_k))[1])
               #diverging_pal’, ‘r_pal’, ‘sequential_pal
               
               V(sg)$label.cex<-label.cex
               #try(plot.igraph(sg,vertex.color=membership(wc),layout=l,vertex.size=vertex.size),silent=TRUE)
               plot.igraph(sg,vertex.color=membership(wc),layout=l,vertex.size=vertex.size)
               
               # mtext("(Edges) Red: +ve correlation; Blue: -ve correlation",side=0,line=0,cex=0.8,adj=(-0.2))
               
               if(is.na(classname)==TRUE){
               mtext("Using all samples",side=3,line=1,cex=0.6,adj=NA)
               }else{
                   
               mtext(paste("Using samples in class ",classname,sep=""),side=3,line=1,cex=0.6,adj=NA)
               }
               
               
               mtext("(Edges) Red: +ve correlation; Blue: -ve correlation",side=3,line=3,cex=0.6,adj=NA)
               
               mtext_community<-paste("(Nodes) Rectangle: ",Xname,"; Circle: ",Yname,sep="")
               if(is.na(Zname)==FALSE){
                   mtext_community<-paste(mtext_community,"; Triangle: ",Zname,sep="")
                   
               }
               if(is.na(Wname)==FALSE){
                   mtext_community<-paste(mtext_community,"; Star: ",Wname,sep="")
                   
               }
               
               # mtext(mtext_community,side=1,cex=0.8,line=1,adj=(-0.2))
               mtext(mtext_community,side=3,cex=0.6,line=2,adj=NA)
                
               #mtext("Each community (cluster) is represented by a different color",side=1,line=2,cex=0.8,adj=0)
               #mtext("Each community (cluster) is represented by a different color:",side=1,line=2,cex=0.7,adj=(-0.2))
               mtext("Each community (C) is represented by a different color:",side=1,line=(-1),cex=0.6,adj=0)
               line_num=(-0.25)
               start_at=(0)
               max_count_per_row<-25
               count_per_row<-1
               for(s in 1:length(wc)){
                   
                   if(count_per_row>max_count_per_row){
                       count_per_row=1
                       line_num=line_num+0.6
                       start_at=(0)
                       #start_at<-count_per_row
                       #mtext(paste("C",s,";",sep=""),col=sg$palette[s],side=1,line=line_num,cex=0.6,adj=(-2),at=((0.1*count_per_row)+0.02),font=2)
                       mtext(paste("C",s,";",sep=""),col=sg$palette[s],side=1,line=line_num,cex=0.65,adj=0,at=(-1.5+(0.1*count_per_row)+0.02),font=2)
                       
                       count_per_row<-count_per_row+1
                   }else{
                       
                       start_at<-count_per_row
                       # mtext(paste("C",s,";",sep=""),col=sg$palette[s],side=1,line=line_num,cex=0.6,adj=(-2),at=((0.1*count_per_row)+0.01),font=2) #eq(+ve correlation; Blue: -ve correlation",side=1,line=0,cex=0.8,adj=0)
                       mtext(paste("C",s,";",sep=""),col=sg$palette[s],side=1,line=line_num,cex=0.7,adj=0,at=(-1.5+(0.1*count_per_row)+0.02),font=2)
                       count_per_row<-count_per_row+1
                   }
               }
               
               try(mtext(fname,line=3,cex=0.6,col="brown",side=1,adj=0),silent=TRUE)
               dev.off()
              
              
              
                
               if(centrality_method=="eigenvector"){
                   
                  
               eigenvector_centrality<-eigen_centrality(sg,directed=FALSE,weights=E(sg)$weight)
               centrality_vec<-eigenvector_centrality$vector
               
               save(centrality_vec,file="centrality_vec.Rda")
               
               cent_vec_mat<-cbind(names(centrality_vec),centrality_vec)
               
               cent_vec_mat<-as.data.frame(cent_vec_mat)
               
               cent_vec_mat<-cent_vec_mat[order(cent_vec_mat[,1]),]
               
               cent_vec_mat<-as.data.frame(cent_vec_mat)
               
              
               
               cent_vec_mat$centrality_vec<-as.numeric(as.character(cent_vec_mat$centrality_vec))
               
               
               centrality_vec<-round(cent_vec_mat$centrality_vec,3)
               }else{
                    if(centrality_method=="betweenness"){
                        centrality_vec<-betweenness(sg,directed=FALSE,weights=abs(E(sg)$weight),normalized=TRUE)
                        save(centrality_vec,file="centrality_vec.Rda")
                        
                        #normalize to 0 to 1 scale
                        centrality_vec<-(centrality_vec-min(centrality_vec,na.rm=TRUE))/(max(centrality_vec,na.rm=TRUE)-min(centrality_vec,na.rm=TRUE))
                        
                        #centrality_vec/max(centrality_vec,na.rm=TRUE)
                        
                        cent_vec_mat<-cbind(names(centrality_vec),centrality_vec)
                        
                        cent_vec_mat<-as.data.frame(cent_vec_mat)

                        cent_vec_mat<-cent_vec_mat[order(cent_vec_mat[,1]),]
                        
                        cent_vec_mat$centrality_vec<-as.numeric(as.character(cent_vec_mat$centrality_vec))
                        
                        centrality_vec<-round(cent_vec_mat$centrality_vec,3)
                    }else{
                        if(centrality_method=="degree.count"){
                            
                            centrality_vec<-igraph::degree(sg,normalized=TRUE)
                            
                            save(centrality_vec,file="centrality_vec.Rda")
                            #normalize to 0 to 1 scale
                            centrality_vec<-(centrality_vec-min(centrality_vec,na.rm=TRUE))/(max(centrality_vec,na.rm=TRUE)-min(centrality_vec,na.rm=TRUE))
                            
                            
                            #centrality_vec<-centrality_vec/max(centrality_vec,na.rm=TRUE)
                            
                            cent_vec_mat<-cbind(names(centrality_vec),centrality_vec)
                            
                            cent_vec_mat<-as.data.frame(cent_vec_mat)
                            
                            cent_vec_mat<-cent_vec_mat[order(cent_vec_mat[,1]),]
                            
                            cent_vec_mat$centrality_vec<-as.numeric(as.character(cent_vec_mat$centrality_vec))
                            
                            centrality_vec<-round(cent_vec_mat$centrality_vec,3)
                        }else{
                            
                                #degree of a node is defined by the sum of its edge weights, where weight=abs(correlation)
                                #degree.weight values are normalized by maximum degree.weight in the network
                                if(centrality_method=="degree.weight"){
                                    
                                    node_names<-names(V(sg))
                                    
                                    centrality_vec<-lapply(1:length(node_names),function(i){
                                        
                                                sub_mat<-df_matrix[c(which(df_matrix$from==node_names[i]),which(df_matrix$to==node_names[i])),]
                                                if(nrow(sub_mat)>0){
                                                    
                                                    res<-sum(abs(sub_mat$weight))
                                                    
                                                }else{
                                                    
                                                    res<-0
                                                }
                                                return(res)
                                    })
                                    centrality_vec<-unlist(centrality_vec)
                                    save(centrality_vec,file="centrality_vec.Rda")
                                    
                                    #normalize to 0 to 1 scale
                                    centrality_vec<-(centrality_vec-min(centrality_vec,na.rm=TRUE))/(max(centrality_vec,na.rm=TRUE)-min(centrality_vec,na.rm=TRUE))
                                    
                                    
                                    #centrality_vec<-centrality_vec/max(centrality_vec,na.rm=TRUE)
                                    
                                    cent_vec_mat<-cbind(node_names,centrality_vec)
                                    cent_vec_mat<-as.data.frame(cent_vec_mat)
                                    
                                    cent_vec_mat<-cent_vec_mat[order(cent_vec_mat[,1]),]
                                    
                                    cent_vec_mat$centrality_vec<-as.numeric(as.character(cent_vec_mat$centrality_vec))
                                    
                                    centrality_vec<-round(cent_vec_mat$centrality_vec,3)
                                    
                                }else{
                                    
                                    if(centrality_method=="closeness"){
                                        
                                        centrality_vec<-igraph::closeness(sg,normalized=TRUE,mode="all",weights=abs(E(sg)$weight))
                                        save(centrality_vec,file="centrality_vec.Rda")
                                    
                                        #normalize to 0 to 1 scale
                                        centrality_vec<-(centrality_vec-min(centrality_vec,na.rm=TRUE))/(max(centrality_vec,na.rm=TRUE)-min(centrality_vec,na.rm=TRUE))
                                        
                                        
                                        #centrality_vec<-centrality_vec/max(centrality_vec,na.rm=TRUE)
                                        
                                        cent_vec_mat<-cbind(names(centrality_vec),centrality_vec)
                                        
                                        cent_vec_mat<-as.data.frame(cent_vec_mat)
                                        
                                        cent_vec_mat<-cent_vec_mat[order(cent_vec_mat[,1]),]
                                        
                                        cent_vec_mat$centrality_vec<-as.numeric(as.character(cent_vec_mat$centrality_vec))
                                        
                                        centrality_vec<-round(cent_vec_mat$centrality_vec,3)
                                    }
                                    
                                }
                            
                        }
                        
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
               
               
               write.table(clust_id_mapping_mat,file=community_membership_fname,sep="\t",row.names=FALSE)
               
               
           }
           
       }


#colnames(id_mapping_mat)<-c("Name","Node")
#colnames(clust_membership)<-c("Node","Cluster")
#clust_id_mapping_mat<-merge(clust_membership,id_mapping_mat,by="Node")
#write.table(clust_id_mapping_mat,file="cluster_membership_mapped.txt",sep="\t",row.names=FALSE)


#save(list=ls(),file="debug.Rda")


if(is.na(classname)==TRUE){
    #if(FALSE)
    {
rnames<-rownames(Xome_data)
if(length(which(rnames%in%clust_id_mapping_mat$Name))>1){
        Xome_data<-Xome_data[which(rnames%in%clust_id_mapping_mat$Name),,drop=FALSE]
}else{
    if(length(which(rnames%in%clust_id_mapping_mat$Name))>0){
        Xome_data<-Xome_data[which(rnames%in%clust_id_mapping_mat$Name),,drop=FALSE]
        # Xome_data<-t(Xome_data)
        rownames(Xome_data)<-rnames[which(rnames%in%clust_id_mapping_mat$Name)]
    }else{
        
        Xome_data<-NA
    }
    
}

rnames<-rownames(Yome_data)
if(length(which(rnames%in%clust_id_mapping_mat$Name))>1){
    Yome_data<-Yome_data[which(rnames%in%clust_id_mapping_mat$Name),,drop=FALSE]
}else{
    if(length(which(rnames%in%clust_id_mapping_mat$Name))>0){
        Yome_data<-Yome_data[which(rnames%in%clust_id_mapping_mat$Name),,drop=FALSE]
        #Yome_data<-t(Yome_data)
        rownames(Yome_data)<-rnames[which(rnames%in%clust_id_mapping_mat$Name)]
    }else{
        
        Yome_data<-NA
    }
    
}

suppressWarnings(
if(is.na(Zome_data)==FALSE){
rnames<-rownames(Zome_data)
if(length(which(rnames%in%clust_id_mapping_mat$Name))>1){
    Zome_data<-Zome_data[which(rnames%in%clust_id_mapping_mat$Name),,drop=FALSE]
    
    
}else{
    if(length(which(rnames%in%clust_id_mapping_mat$Name))>0){
        Zome_data<-Zome_data[which(rnames%in%clust_id_mapping_mat$Name),,drop=FALSE]
        #Zome_data<-t(Zome_data)
        rownames(Zome_data)<-rnames[which(rnames%in%clust_id_mapping_mat$Name)]
    }else{
        
        Zome_data<-NA
    }
    
    }

})
suppressWarnings(
if(is.na(Wome_data)==FALSE){
    rnames<-rownames(Wome_data)
    if(length(which(rnames%in%clust_id_mapping_mat$Name))>1){
        Wome_data<-Wome_data[which(rnames%in%clust_id_mapping_mat$Name),,drop=FALSE]
    }else{
        if(length(which(rnames%in%clust_id_mapping_mat$Name))>0){
            Wome_data<-Wome_data[which(rnames%in%clust_id_mapping_mat$Name),,drop=FALSE]
            # Wome_data<-t(Wome_data)
            rownames(Wome_data)<-rnames[which(rnames%in%clust_id_mapping_mat$Name)]
        }else{
            
            Wome_data<-NA
        }
        
    }
    
})

}


}
#return(sg)

if(removeRda==TRUE){
unlink("*.Rda",force=TRUE,recursive=TRUE)
unlink("pairwise_results/*.Rda",force=TRUE,recursive=TRUE)

}

return(list("graph"=sg,"network_analysis"=clust_id_mapping_mat,"Xome_data"=Xome_data,"Yome_data"=Yome_data,"Zome_data"=Zome_data,"Wome_data"=Wome_data,"classname"=classname))
}
