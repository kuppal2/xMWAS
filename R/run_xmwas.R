run_xmwas <-
function(xome_fname=NA,yome_fname=NA,zome_fname=NA,wome_fname=NA,outloc=NA,class_fname=NA,Xome_data=NA,Yome_data=NA,Zome_data=NA,Wome_data=NA,classlabels=NA,xmwasmethod="spls",plsmode="canonical",max_xvar=5000,max_yvar=5000,max_zvar=5000,max_wvar=5000,rsd.filt.thresh=1,all.missing.thresh=NA,missing.val=0,corthresh=0.4,keepX=1000,keepY=1000,keepZ=1000,keepW=1000,pairedanalysis=FALSE,optselect=TRUE,rawPthresh=0.05,numcomps=10,net_edge_colors=c("blue","red"),net_node_colors=c("orange", "green","blue","gold"),Xname="X",Yname="Y",Zname="Z",Wname="W",net_node_shape=c("circle","rectangle","triangle","star"),seednum=100,label.cex=0.3,vertex.size=6,max_connections=NA,centrality_method="eigenvector",use.X.reference=FALSE,removeRda=TRUE,compare.classes=TRUE,class.comparison.allvar=TRUE,...){
    
    
    #defaults
    classname=NA
    maxnodesperclass=10000000
    graphclustering=TRUE
    interactive=FALSE
    suppressWarnings(dir.create(outloc))
    setwd(outloc)
    print("#########################Starting processing now######################")
    
    sink(file="InputParameters.txt")
    
    print("######xMWAS v0.54 Parameters##########")
    print(paste("xmwasmethod: ",xmwasmethod,sep=""))
    print(paste("plsmode: ",plsmode,sep=""))
    print(paste("max_xvar: ",max_xvar,sep=""))
    print(paste("max_yvar: ",max_yvar,sep=""))
    print(paste("max_zvar: ",max_zvar,sep=""))
    print(paste("max_wvar: ",max_wvar,sep=""))
    print(paste("rsd.filt.thresh: ",rsd.filt.thresh,sep=""))
    print(paste("all.missing.thresh: ",all.missing.thresh,sep=""))
    print(paste("missing.val: ",missing.val,sep=""))
    print(paste("corthresh: ",corthresh,sep=""))
    print(paste("keepX: ",keepX,sep=""))
    print(paste("keepY: ",keepY,sep=""))
    print(paste("keepZ: ",keepZ,sep=""))
    print(paste("keepW: ",keepW,sep=""))
    print(paste("pairedanalysis: ",pairedanalysis,sep=""))
    print(paste("optselect: ",optselect,sep=""))
    print(paste("rawPthresh: ",rawPthresh,sep=""))
    print(paste("numcomps: ",numcomps,sep=""))
    print(paste("seednum: ",seednum,sep=""))
    print(paste("graphclustering: ",graphclustering,sep=""))
    print(paste("max_connections: ",max_connections,sep=""))
    print(paste("centrality_method: ",centrality_method,sep=""))
    print(paste("use.X.reference: ",use.X.reference,sep=""))
    print(paste("compare.classes: ",compare.classes,sep=""))
    print(paste("class.comparison.allvar: ",class.comparison.allvar,sep=""))
    
    print("################")
    print("######Loaded packages in the current session##########")
    print(sessionInfo())
    sink(file=NULL)
    
    d1<-date()
    d2<-gsub(d1,pattern=":| ",replacement="_")
    d3<-gsub(d2,pattern="__",replacement="_")
    log_fname<-paste("Log",d3,".txt",sep="")
    
    print(paste("Program is running. Please check the logfile for runtime status: ",log_fname,sep=""))
    
    sink(file=log_fname)
     
     #all.missing.thresh=0.1
    
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
        #  Xome_data<-as.numeric(Xome_data)
        #Xome_data<-as.data.frame(Xome_data)
    }
    
    
    rownames(Xome_data)<-rnames
    
   
    num_replicates<-1
    
    
    if(is.na(yome_fname)==FALSE){
        Yome_data<-read.table(yome_fname,sep="\t",header=TRUE)
        
       
        
       
    }
    
   
   
    
    
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
        #Yome_data<-as.numeric(Yome_data)
        #Yome_data<-as.data.frame(Yome_data)
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
            # Zome_data<-as.numeric(Zome_data)
            #Zome_data<-as.data.frame(Zome_data)
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
            #   Wome_data<-as.numeric(Wome_data)
            #Wome_data<-as.data.frame(Wome_data)
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
        
        # classlabels<-classlabels[,-c(1)]
        
        
    }
    suppressWarnings(
    if(is.na(classlabels)==TRUE){
        
        classlabels<-rep("A",dim(Xome_data)[2])
        
        classlabels<-as.data.frame(classlabels)
        
        colnames(classlabels)<-c("class")
        
    }else{
        
        cnames_class<-colnames(classlabels)
        cnames_class<-tolower(cnames_class)
        
        colnames(classlabels)<-cnames_class
        
        

        
    })
    classlabels<-as.data.frame(classlabels)
    
    
    
    
    
    
    colors_sel_vec<-net_node_colors
    
    res<-new("list")
    
    suppressWarnings(
    if(is.na(classlabels)==FALSE){
    cnames<-colnames(classlabels)
    cnames<-tolower(cnames)

    colnames(classlabels)<-cnames
        
        
    
    class_check<-which(cnames=="class") || which(cnames=="factor1")
    if(length(class_check)<1){
        
        
        stop("No Class column found in the class labels file. Please label the class column as \"Class\" or \"Factor1\".")
    }
    
    class_check1<-which(cnames=="class")
    
    if(length(class_check1)<1){
        
        class_levels<-levels(classlabels$factor1)
    }else{
        class_levels<-levels(classlabels$class)
    }


    classlabels<-as.data.frame(classlabels)
    
    print("Class levels:")
    print(class_levels)

    
    }else{
        
        class_levels<-{}
    }
    )
    
    if(pairedanalysis==TRUE){
        
        
        classlabels_temp<-classlabels[,-c(2)] #[,c(class_check)]
        classlabels_temp<-as.data.frame(classlabels_temp)
        
        
        design<-classlabels[,-c(1)]
        
        
        
        suppressWarnings(
        res[[1]]<-run_xmwas_child(Xome_data=Xome_data,Yome_data=Yome_data,Zome_data=Zome_data,Wome_data=Wome_data,outloc=outloc,classlabels=classlabels_temp,xmwasmethod="spls",plsmode=plsmode,max_xvar=max_xvar,max_yvar=max_yvar,max_zvar=max_zvar,max_wvar=max_wvar,rsd.filt.thresh=rsd.filt.thresh,corthresh=corthresh,keepX=keepX,keepY=keepY,keepZ=keepZ,keepW=keepW,pairedanalysis=TRUE,optselect=optselect,rawPthresh=rawPthresh,numcomps=numcomps,net_edge_colors=net_edge_colors,net_node_colors=net_node_colors,Xname=Xname,Yname=Yname,Zname=Zname,Wname=Wname,net_node_shape=net_node_shape,all.missing.thresh=all.missing.thresh,maxnodesperclass=maxnodesperclass,seednum=seednum,label.cex=label.cex,vertex.size=vertex.size,graphclustering=graphclustering,interactive=interactive,max_connections=max_connections,classname=NA,centrality_method=centrality_method,use.X.reference=use.X.reference,removeRda=removeRda,design=design,missing.val=missing.val)
        )
    }else{
        
    suppressWarnings(
    res[[1]]<-run_xmwas_child(Xome_data=Xome_data,Yome_data=Yome_data,Zome_data=Zome_data,Wome_data=Wome_data,outloc=outloc,classlabels=classlabels,xmwasmethod=xmwasmethod,plsmode=plsmode,max_xvar=max_xvar,max_yvar=max_yvar,max_zvar=max_zvar,max_wvar=max_wvar,rsd.filt.thresh=rsd.filt.thresh,corthresh=corthresh,keepX=keepX,keepY=keepY,keepZ=keepZ,keepW=keepW,pairedanalysis=pairedanalysis,optselect=optselect,rawPthresh=rawPthresh,numcomps=numcomps,net_edge_colors=net_edge_colors,net_node_colors=net_node_colors,Xname=Xname,Yname=Yname,Zname=Zname,Wname=Wname,net_node_shape=net_node_shape,all.missing.thresh=all.missing.thresh,maxnodesperclass=maxnodesperclass,seednum=seednum,label.cex=label.cex,vertex.size=vertex.size,graphclustering=graphclustering,interactive=interactive,max_connections=max_connections,classname=NA,centrality_method=centrality_method,use.X.reference=use.X.reference,removeRda=removeRda,design=NA,missing.val=missing.val)
)

    }
    
    if(class.comparison.allvar==FALSE){
        
    Xome_data<-res[[1]]$Xome_data
    Yome_data<-res[[1]]$Yome_data
    Zome_data<-res[[1]]$Zome_data
    Wome_data<-res[[1]]$Wome_data
    
    }

#save(res[[1]],file="allclasses.Rda")
    
    # rm(list=ls())
    if(compare.classes==TRUE){
if(length(class_levels)>1){

for(i in 1:length(class_levels)){
    
    outloctemp<-paste(outloc,"/",class_levels[i],sep="")
    
    if(pairedanalysis==TRUE){
        
       
        classlabels_temp<-classlabels[,-c(2)]  #[,c(class_check)]
        classlabels_temp<-as.data.frame(classlabels_temp)
        
        design<-classlabels[,-c(1)]
        
        print(head(classlabels_temp))
        
        suppressWarnings(
        res[[(i+1)]]<-run_xmwas_child(Xome_data=Xome_data,Yome_data=Yome_data,Zome_data=Zome_data,Wome_data=Wome_data,outloc=outloctemp,classlabels=classlabels_temp,xmwasmethod="spls",plsmode=plsmode,max_xvar=max_xvar,max_yvar=max_yvar,max_zvar=max_zvar,max_wvar=max_wvar,rsd.filt.thresh=rsd.filt.thresh,corthresh=corthresh,keepX=NA,keepY=NA,keepZ=NA,keepW=NA,pairedanalysis=FALSE,optselect=optselect,rawPthresh=rawPthresh,numcomps=numcomps,net_edge_colors=net_edge_colors,net_node_colors=net_node_colors,Xname=Xname,Yname=Yname,Zname=Zname,Wname=Wname,net_node_shape=net_node_shape,all.missing.thresh=all.missing.thresh,maxnodesperclass=maxnodesperclass,seednum=seednum,label.cex=label.cex,vertex.size=vertex.size,graphclustering=graphclustering,interactive=interactive,max_connections=max_connections,classname=class_levels[i],centrality_method=centrality_method,use.X.reference=use.X.reference,removeRda=removeRda,design=design,missing.val=missing.val)
        )
    }else{
        
        if(dim(classlabels)[2]>2){
            
            stop("More than two columns detected in the class labels file. Please set paired analysis=TRUE for repeated measures.")
        }
        suppressWarnings(
    res[[(i+1)]]<-run_xmwas_child(Xome_data=Xome_data,Yome_data=Yome_data,Zome_data=Zome_data,Wome_data=Wome_data,outloc=outloctemp,classlabels=classlabels,xmwasmethod=xmwasmethod,plsmode=plsmode,max_xvar=max_xvar,max_yvar=max_yvar,max_zvar=max_zvar,max_wvar=max_wvar,rsd.filt.thresh=rsd.filt.thresh,corthresh=corthresh,keepX=NA,keepY=NA,keepZ=NA,keepW=NA,pairedanalysis=pairedanalysis,optselect=optselect,rawPthresh=rawPthresh,numcomps=numcomps,net_edge_colors=net_edge_colors,net_node_colors=net_node_colors,Xname=Xname,Yname=Yname,Zname=Zname,Wname=Wname,net_node_shape=net_node_shape,all.missing.thresh=all.missing.thresh,maxnodesperclass=maxnodesperclass,seednum=seednum,label.cex=label.cex,vertex.size=vertex.size,graphclustering=graphclustering,interactive=interactive,max_connections=max_connections,classname=class_levels[i],centrality_method=centrality_method,use.X.reference=use.X.reference,removeRda=removeRda,design=NA,missing.val=missing.val)
    )
    }


}

}

    }
outloctemp<-paste(outloc,"/",sep="")
setwd(outloctemp)


s1<-"1:	 X labels correspond to xome_fname data, Y labels correspond to yome_fname data, Z labels correspond to zome_fname data, W labels correspond to wome_fname data"
s2<-"2:  Pairwise integrative analysis results are under pairwise_results. The files corresponding to each pairwise comparison (X<->Y, X<->Z, Y<-Z,..) are: XYassociation_matrix_threshold0.9.txt (correlation matrix with mapping between node labels and original variable names), XYassociation_networkthresholdX.pmg that includes the pairwise network plots, XYBoolean_association_matrix_thresholdX.txt (same as correlation matrix but correlations meeting the threshold are represented 1, and 0 otherwise)"
s3<-"3:  Multidata_Network_thresholdx.png: includes Multidata network plot using all significantly associated variables."
#s5<-"5:  Multidata_Network_corthreshx100: includes Multidata network plot using top 100 significantly associated variables from each layer (Y,Z, and W)"
s4<-"4:  Multidata_Network_thresholdx_communities.png: includes Multidata network plot with the communities identified using the multilevel community detection algorithm. Members of each community are assigned colors based on community/module/cluster membership (1: orange; 2: light blue; 3: dark green, and so on)."

s5<-"5:  Multidata_Network_thresholdxcytoscape.gml: GML file for all significantly associated variables that can be uploaded to Cytoscape"

#s7<-"7:  Multidata_Network_corthreshxcytoscape.gml: GML file for top 100 significantly associated variables from each layer (Y,Z, and W) that can be uploaded to Cytoscape"
s6<-"6: The cluster_membership_centrality_mapped.txt file includes community detection results using the multilevel community detection algorithm and the centrality measures."
s7<-"7: The class-wise_centrality_matrix.txt file includes the centrality measures across different conditions for nodes that meet the association criteria and included in the association networks."
s8<-"8:  If the classlabels are provided, network analysis is performed for samples from each class. The results ar written in individual subfolders."
sm<-rbind(s1,s2,s3,s4,s5,s6,s7,s8)
sm<-as.data.frame(sm)
colnames(sm)<-"Description of files"
write.table(sm,file="README.txt",sep="\t",row.names=FALSE)

graphclustering=TRUE


if(graphclustering==TRUE){

#if multiple classes
if(length(res)>1){

node_names_vec<-{}
for(r1 in 1:length(res)){
    temp1<-res[[r1]]$network_analysis[order(res[[r1]]$network_analysis$Name),]
   
    if(nrow(temp1)>0){
        
        node_names_vec<-c(node_names_vec,as.character(res[[r1]]$network_analysis$Name))
    }
}
node_names_vec<-unique(node_names_vec)


matrix_centrality<-matrix(NA,nrow=length(node_names_vec),ncol=length(res))

  rownames(matrix_centrality)<-as.character(node_names_vec)

  colnames(matrix_centrality)<-c("allClasses",class_levels)


    for(i in 1:length(res)){
    
    atemp<-res[[i]]$network_analysis[order(res[[i]]$network_analysis$Name),]
    
    
    for(rownum in 1:dim(atemp)[1]){
        index_1<-which(node_names_vec%in%(atemp$Name[rownum]))
        
        matrix_centrality[index_1,i]<-atemp$centrality_vec[rownum]
        
        }
    }

}else{
    
    a1<-res[[1]]$network_analysis[order(res[[1]]$network_analysis$Name),]
    
    a1<-as.data.frame(a1)
    
    matrix_centrality<-matrix(NA,nrow=dim(a1)[1],ncol=length(res))
    
    matrix_centrality[,1]<-a1$centrality_vec
    
    rownames(matrix_centrality)<-as.character(a1$Name)

    colnames(matrix_centrality)<-c("allClasses")

    
}
if(length(res)>1){
    delta_centrality<-{}
    name_vec<-{}
    for(i in 3:length(res)){
        delta_centrality<-cbind(delta_centrality,abs(matrix_centrality[,i]-matrix_centrality[,2]))
        name_vec<-c(name_vec,paste(class_levels[(i-1)],"_vs_",class_levels[(1)],sep=""))
        
        

    }
    colnames(delta_centrality)<-name_vec
    
    matrix_centrality<-cbind(matrix_centrality,delta_centrality)
    
    
}
setwd(outloctemp)
write.table(matrix_centrality,file="class-wise_centrality_matrix.txt",sep="\t")
}else{
    matrix_centrality=NA
}

 sink(file=NULL)
 
 print("Processing complete!")
 
return(list("xmwas.res"=res,"centrality_matrix"=matrix_centrality))
    
}
