generate.custom.network <-
function(df,nodes,colors,fname=NA,vertex.size=5,seednum=100,net_edge_colors=c("blue","red"),net_node_colors=c("orange", "green","blue","purple"),net_node_shape=c("circle","rectangle","triangle","star"),label.cex=0.3){
    
    
    if(is.na(fname)==TRUE){
        fname<-paste("custom_network.png",sep="")
    }
    
    df<-df[c(which(df$from%in%nodes),which(df$to%in%nodes)),]
  
  
    sg <- graph.data.frame(d = df, directed = FALSE)
    
    #save(sg,file="sgcustom.Rda")
    
        #sg <- graph.adjacency(df, mode="undirected", weighted=TRUE)
    nodes_vec<-V(sg)$name
    
    nodes_vec_class<-gsub(nodes_vec,pattern="[0-9]*",replacement="")
    nodes_col_vec<-rep("blue",length(nodes_vec))
    nodes_shape_vec<-rep("circle",length(nodes_vec))
    nodes_size_vec<-rep(vertex.size,length(nodes_vec))
    #t1<-table(nodes_vec_class)
    
    t1<-levels(as.factor(nodes_vec_class))
    #       e1<-edgeWeights(sg)

        # clips as a circle
        add_shape("triangle",plot=mytriangle) #, clip=shapes("circle")$clip,plot=mytriangle)
        # no clipping, edges will be below the vertices anyway
        add_shape("star", clip=shape_noclip,plot=mystar, parameters=list(vertex.norays=5))
        
        shapes_sel_vec<-net_node_shape  #c("triangle","sphere","rectangle","star")
    
    
    for(t1_ind in 1:length(t1)){
        
        nodes_col_vec[which(nodes_vec_class==t1[t1_ind])]<-net_node_colors[t1_ind]
        
        nodes_shape_vec[which(nodes_vec_class==t1[t1_ind])]<-shapes_sel_vec[t1_ind]
        
        nodes_size_vec[which(nodes_vec_class==t1[t1_ind])]<-3/t1_ind
        
    }
    
    nAttrs<-list()
    nAttrs$fillcolor<-nodes_col_vec
    names(nAttrs$fillcolor)<-nodes_vec #(sg)
    nAttrs$shape<-nodes_shape_vec
    names(nAttrs$shape)<-nodes_vec #(sg)
    #nAttrs$size<-nodes_size_vec
    #names(nAttrs$size)<-nodes_vec #(sg)
    
    
        edge_colors<-rep("blue",length(E(sg)$weight))
        edge_colors[which(E(sg)$weight>0)]<-"red"
        
        V(sg)$color<-nodes_col_vec
        V(sg)$shape<-nodes_shape_vec
        V(sg)$label.cex<-label.cex
        
        E(sg)$color=edge_colors
        
        set.seed(seednum)
        #plot(sg, vertex.label = V(sg)$name)
        #plot(sg, vertex.label = V(sg)$name,edge.color=edge_colors,layout = layout.fruchterman.reingold,vertex.size = 10,main="Integrative network")
        
        
        #mtext("Blue: -ve correlation",side=1,col="blue")
        #mtext("layout.fruchterman.reingold, area = vcount^2", side=1)
        
                    cytoscape_fname<-paste("cytoscapecustom.gml",sep="")
                    #rda_fname<-paste(filename,"top",maxnodesperclass,".Rda",sep="")
        
       
        write.graph(sg, file =cytoscape_fname, format = "gml")
        
        nodes<-V(sg)$name
        nodes<-as.data.frame(nodes)
        links<-df[,c(1:2)]
        links<-as.data.frame(links)
      
    
    
    
    
    # pdf(fname)
    
    l = layout.fruchterman.reingold(sg, weights = (1 -abs(E(sg)$weight)))
      png(fname,width=8,height=8,res=600,type="cairo",units="in")
    
    # clips as a circle
    add_shape("triangle",plot=mytriangle) #, clip=shapes("circle")$clip,plot=mytriangle)
    # no clipping, edges will be below the vertices anyway
    add_shape("star", clip=shape_noclip,plot=mystar, parameters=list(vertex.norays=5))
    #try(plot(wc,sg,layout=layout_with_fr,vertex.size=vertex.size),silent=TRUE)
    set.seed(seednum)
    #  try(plot(sg,vertex.color=clust_membership$Cluster,layout=layout_with_fr,vertex.size=vertex.size),silent=TRUE)
    
    sg$layout <- l #ayout_with_fr
    plot.igraph(sg,vertex.size=vertex.size,vertex.label=V(sg)$name,edge.color=edge_colors,layout=l)
    mtext("Red: +ve correlation; Blue: -ve correlation",side=1)
    try(mtext(fname,line=3,cex=0.6,col="brown",side=1,adj=0),silent=TRUE)

    dev.off()
    #write.table(clust_membership,file="cluster_membership.txt",sep="\t",row.names=FALSE)
    
    
}
