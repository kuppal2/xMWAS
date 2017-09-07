highlight.network <-
function(graph,colors,membership=NA,fname=NA,vertex.size=5,seednum=100){

    clust_membership<-as.data.frame(membership)
    
    if(is.na(fname)==TRUE){
    fname<-paste("highlight_network.png",sep="")
    }
    
   
    colnames(clust_membership)<-c("Node","Cluster")
    sg<-graph
    l = layout.fruchterman.reingold(sg, weights = (1 -abs(E(sg)$weight)))
    #pdf(fname)
    
    png(fname,width=8,height=8,res=600,type="cairo",units="in")
    
    
    #V(sg)$label.cex<-0.25
    
    # clips as a circle
    add_shape("triangle",plot=mytriangle) #, clip=shapes("circle")$clip,plot=mytriangle)
    # no clipping, edges will be below the vertices anyway
    add_shape("star", clip=shape_noclip,plot=mystar, parameters=list(vertex.norays=5))
    #try(plot(wc,sg,layout=layout_with_fr,vertex.size=vertex.size),silent=TRUE)
    set.seed(seednum)
    try(plot(sg,vertex.color=clust_membership$Cluster,layout=l,vertex.size=vertex.size),silent=TRUE)
    dev.off()
  
}
