get_sub_clusters <-
function(df=NA,net_node_colors=c("orange", "green","blue"),rownames_vec=NA,colnames_vec=NA,sat=0.00001,Xname="X",Yname="Y",Zname=NA,Wname=NA,cormatrix=NA,corthresh=0.4,seednum=100){
    
   # df[,3]<-abs(df[,3])

   #gtemp <- graph.data.frame(d = df, directed = FALSE)
    gtemp<-graphBAM(df)
    g4<-highlyConnSG(gtemp,sat=0.00001)
    
    clust_len<-{}
    for(cnum in 1:length(g4$clusters)){
        clust_len<-c(clust_len,length(g4$clusters[[cnum]]))
        
    }
    
    sub_graphs<-which(clust_len>1)
    
    
    clustnum=0
    for(s in sub_graphs){
        
        sg<-subGraph(c(g4$clusters[s][[1]]),gtemp)
        
        nodes_vec<-nodes(sg)
        nodes_vec_class<-gsub(nodes_vec,pattern="[0-9]*",replacement="")
        
        nodes_col_vec<-rep("blue",length(nodes_vec))
        
        nodes_shape_vec<-rep("circle",length(nodes_vec))
        
        nodes_size_vec<-rep(3,length(nodes_vec))
        
        #t1<-table(nodes_vec_class)
        t1<-levels(as.factor(nodes_vec_class))
        
        e1<-edgeWeights(sg)
        
        shapes_sel_vec<-c("circle","ellipse","box","rectangle")
        
        for(t1_ind in 1:length(t1)){
            
        nodes_col_vec[which(nodes_vec_class==t1[t1_ind])]<-net_node_colors[t1_ind]
        
        nodes_shape_vec[which(nodes_vec_class==t1[t1_ind])]<-shapes_sel_vec[t1_ind]
        
        nodes_size_vec[which(nodes_vec_class==t1[t1_ind])]<-3/t1_ind
       
        }
        
        nAttrs<-list()
        nAttrs$fillcolor<-nodes_col_vec
        names(nAttrs$fillcolor)<-nodes(sg)
        nAttrs$shape<-nodes_shape_vec
        names(nAttrs$shape)<-nodes(sg)
        # nAttrs$size<-nodes_size_vec
        #names(nAttrs$size)<-nodes(sg)
        
	 clustnumtemp<-clustnum+1
	filename=""
	fname<-paste(filename,"cor",corthresh,"cluster",clustnumtemp,".png",sep="")
      	dfsub<-{}
	names_e1<-names(e1)
	
	check_x_names<-gregexpr(names(e1),pattern="X[0-9]+")
	check_x_names<-which(check_x_names==1)
	
	for(i in check_x_names){
			
			
			temp1<-as.data.frame(e1[i])
			for(j in 1:dim(temp1)[1]){
				dfsubtemp<-cbind(names_e1[i],rownames(temp1)[j],temp1[j,1])	
				
				dfsub<-rbind(dfsub,dfsubtemp)
			}
	} 
	dfsub<-as.data.frame(dfsub)
	colnames(dfsub)<-c("from","to","weight")

	dfsub$weight<-as.numeric(as.character(dfsub$weight))
	dfsub<-dfsub[order(dfsub$weight,decreasing=FALSE),]
	fname<-paste("SubNetwork_corthresh",corthresh,"_cluster",clustnumtemp,".png",sep="")
	fname1<-paste("SubNetwork_corthresh",corthresh,"_cluster",clustnumtemp,sep="")

	set.seed(555)
    #pdf(fname)
    png(fname,width=8,height=8,res=600,type="cairo",units="in")
    
    
	#png("plot1.png", height=6, width=12, units="in", res=200)
	plot_graph(df=dfsub,net_node_colors=net_node_colors,graphmethod="radial",label.cex=0.3,filename=fname1,seednum=seednum)
	

	#pdf(fname)
        #plot(sg)
        
        #plot(sg,nodeAttrs=nAttrs)
        dev.off()
      	rnames1=rownames_vec
	simmat_colnames<-colnames_vec 
        if(length(sg@nodes)>1){
            clustnum=clustnum+1
             subexp<-gregexpr(sg@nodes,pattern=paste(Xname,"[0-9]+",sep=""))
            xrow_ind<-sg@nodes[which(subexp>0)]
            rnamesA<-xrow_ind
            rnamesB<-rnames1
            sorted_index<-{}
            for(i in 1:length(rnamesA)){
                ctext<-paste("^",rnamesA[i],"$",sep="")
                sorted_index<-c(sorted_index,grep(ctext,rnamesB))
            }
           xrow_ind<-sorted_index
           xcol_ind_final<-{} 
            subexp<-gregexpr(sg@nodes,pattern=paste(Yname,"[0-9]+",sep=""))
            xcol_ind<-sg@nodes[which(subexp>0)]
           rnamesA<-xcol_ind
           rnamesB<-simmat_colnames
           sorted_index<-{}
           for(i in 1:length(rnamesA)){
               ctext<-paste("^",rnamesA[i],"$",sep="")
               sorted_index<-c(sorted_index,grep(ctext,rnamesB))
           }
	   xcol_ind_final<-c(xcol_ind_final,sorted_index) 
	if(is.na(Zname)==FALSE){ 
	  subexp<-gregexpr(sg@nodes,pattern=paste(Zname,"[0-9]+",sep=""))
            xcol_ind<-sg@nodes[which(subexp>0)]
           rnamesA<-xcol_ind
           rnamesB<-simmat_colnames
           sorted_index<-{}
           for(i in 1:length(rnamesA)){
               ctext<-paste("^",rnamesA[i],"$",sep="")
               sorted_index<-c(sorted_index,grep(ctext,rnamesB))
           }
	   xcol_ind_final<-c(xcol_ind_final,sorted_index)
	}
	if(is.na(Wname)==FALSE){
	   subexp<-gregexpr(sg@nodes,pattern=paste(Wname,"[0-9]+",sep=""))
            xcol_ind<-sg@nodes[which(subexp>0)]
           rnamesA<-xcol_ind
           rnamesB<-simmat_colnames
           sorted_index<-{}
           for(i in 1:length(rnamesA)){
               ctext<-paste("^",rnamesA[i],"$",sep="")
               sorted_index<-c(sorted_index,grep(ctext,rnamesB))
           }
           xcol_ind_final<-c(xcol_ind_final,sorted_index)
          }

	    cormatrix1<-cbind(rownames_vec,cormatrix)
	   cormatrix1<-rbind(c("xName",colnames_vec),cormatrix1)
	submat<-cormatrix1[c(1,(xrow_ind+1)),c(1,(xcol_ind_final+1))]
           
	    fname<-paste("SubNetworkAssociation_matrix_corthresh",corthresh,"_cluster",clustnumtemp,".txt",sep="")
            
            
            write.table(submat,file=fname,sep="\t")


	    xtemp<-abs(cormatrix)

	    

maxcor<-apply(xtemp,1,max)



xtemp[which(xtemp>=corthresh)]<-1
xtemp[which(xtemp<corthresh)]<-0


if(length(which(maxcor>=corthresh))>1){
NumConnections<-apply(xtemp,1,sum)
}else{
	NumConnections<-sum(xtemp)
}

	    xtemp<-cbind(rownames_vec,xtemp)
            xtemp<-rbind(c("xName",colnames_vec),xtemp)
            submat<-xtemp[c(1,(xrow_ind+1)),c(1,(xcol_ind_final+1))]

 	    submat<-cbind(submat,c(NA,NumConnections[(xrow_ind+1)]))


            fname<-paste("SubNetworkBoolean_matrix_corthresh",corthresh,"_cluster",clustnumtemp,".txt",sep="")
	    write.table(submat,file=fname,sep="\t")

	    
        }
        
        
    }
    



}
