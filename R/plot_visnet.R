plot_visnet <-
function(sg,df_matrix,main_text="Integrative network",Xname="X",Yname="Y",Zname=NA,Wname=NA,communitymembership=NA,footer=NA,id_mapping_mat=NA,html.selfcontained = TRUE,max.connections.interactive=100,seednum=100,layoutmatrix=NA){

    save(id_mapping_mat,file="id.Rda")


    if(identical(communitymembership, NA)==TRUE){
        #vis.nodes$color <- V(sg)$color

        node_attr_matrix<-cbind(V(sg)$name,V(sg)$shape,V(sg)$color,V(sg)$label.cex,V(sg)$vertex.size)

    }else{


        node_attr_matrix<-cbind(V(sg)$name,V(sg)$shape,sg$palette[communitymembership],V(sg)$label.cex,V(sg)$vertex.size)

    }


    node_attr_matrix<-as.data.frame(node_attr_matrix)
    colnames(node_attr_matrix)<-c("name","shape","color","label.cex","vertex.size")

    nrow_df_matrix<-nrow(df_matrix)

    if(FALSE){
            if(nrow(df_matrix)>100){


                corweight_temp<-abs(df_matrix[,3])
                ind_top_2000<-order(corweight_temp,decreasing=TRUE)[1:max.connections.interactive]
                # print(paste("Only ",max_connections," connections ranked by strength of association will be plotted.",sep=""))

                e=df_matrix[ind_top_2000,c(1:2),drop=FALSE]


                e <- apply(e, 1, paste, collapse=" --")

                df_matrix<-df_matrix[ind_top_2000,,drop=FALSE]
                node_names_top2000<-unique(c(as.character(df_matrix$from),as.character(df_matrix$to)))

                # sg <- subgraph(sg,v=unique(c(as.character(df_matrix$from),as.character(df_matrix$to))))
                sg<-graph.data.frame(d = df_matrix, directed = FALSE) #induced.subgraph(sg,which(V(sg)$name %in% c(node_names_top2000)))
                #sg<-subgraph.edges(sg,eids=factor(e))

            }

    }



    #graph.data.frame(d = df_matrix, directed = FALSE)

    node_vec<-unique(V(sg)$name)
    graphindnum<-seq(1,length(node_vec))
    node_vec<-cbind(node_vec,graphindnum)
    node_vec<-as.data.frame(node_vec)
    colnames(node_vec)=c("id","graphindnum")



    id_mapping_mat<-id_mapping_mat[which(id_mapping_mat$Node%in%node_vec[,1]),]

    node_vec<-merge(node_vec,id_mapping_mat,by.y="Node",by.x="id")
    node_vec<-node_vec[order(as.numeric(as.character(node_vec$graphindnum))),]

    node_vec<-node_vec[,-c(2)]

    node_type<-gsub(as.character(node_vec[,1]),pattern="[0-9]*",replacement="")

    if(identical(communitymembership, NA)==FALSE){
        node_vec<-cbind(node_vec,node_type)
        node_vec<-as.data.frame(node_vec)

        colnames(node_vec)<-c("id","Community", "Name","type")

    }else{
        node_vec<-cbind(node_vec,node_type)
        node_vec<-as.data.frame(node_vec)

        colnames(node_vec)<-c("id", "Name","type")

    }


    node_name<-as.character(node_vec$id)

    node_name[which(node_vec$type=="X")]<-Xname
    node_name[which(node_vec$type=="Y")]<-Yname

    if(is.na(Zname)==FALSE){

        node_name[which(node_vec$type=="Z")]<-Zname
    }
    if(is.na(Wname)==FALSE){
        node_name[which(node_vec$type=="W")]<-Wname
    }



    vis.nodes<-node_vec #[,c("id","type")]
    if(identical(communitymembership, NA)==FALSE){
        vis.nodes$title<-paste("Name: ",node_vec$Name, "<br> Community: ",node_vec$Community,sep="") #paste('<a target="_blank" href="" onclick="return false;" style="background-color:white">',paste("Name: ",node_vec$Name, "<br> Community: ",node_vec$Community,sep=""),'</a>',sep="")
    }else{
        vis.nodes$title<-paste("Name: ",node_vec$Name,sep="") #paste('<a target="_blank" href="" onclick="return false;" style="background-color:white">',paste("Name: ",node_vec$Name,sep=""),'</a>',sep="")
    }


    node_attr_matrix_sel<-merge(node_attr_matrix,node_vec,by.x="name",by.y="id")



    vis.nodes$shape<-V(sg)$shape



    vis.nodes$label<-V(sg)$name
    vis.nodes$label.cex<-V(sg)$label.cex
    vis.nodes$size=V(sg)$vertex.size
    vis.nodes$borderWidth <- 0.5
    vis.nodes$color.border <- "black"



    if(identical(communitymembership, NA)==TRUE){
        vis.nodes$color <- V(sg)$color

    }else{
        vis.nodes$color<-V(sg)$color #sg$palette[communitymembership]

    }


    vis.links<-df_matrix

    vis.links$width <- 1+E(sg)$weight/8 # line width

    vis.links$color <- E(sg)$color    # line color
    vis.links$arrows <- "NULL" # arrows: 'from', 'to', or 'middle'
    vis.links$smooth <- FALSE    # should the edges be curved?
    vis.links$shadow <- FALSE    # edge shadow

    vis.nodes$group <- node_vec$type

    net.width="100%"
    net.height="650px"
    # clips as a circle
    add_shape("triangle",plot=mytriangle) #, clip=shapes("circle")$clip,plot=mytriangle)
    # no clipping, edges will be below the vertices anyway
    add_shape("star", clip=shape_noclip,plot=mystar, parameters=list(vertex.norays=5))




    visnet<-visNetwork(nodes=vis.nodes,edges=vis.links,main=main_text,width=net.width, height=net.height)




    if(identical(communitymembership, NA)==FALSE){

        # passing custom nodes and/or edges

        lnodes <- data.frame(label =paste(unique(node_vec$type),unique(node_name),sep=""),
        shape = unique(vis.nodes$shape), color = rep("grey",length(unique(node_name))),size=rep(8,length(unique(vis.nodes$shape))),shape.size = rep(50, length(unique(node_name))),font.align="top",font.size=15)

        ledges1 <- data.frame(color = c("blue","red"),label = c("-ve correlation", "+ve correlation"), arrows =c("NULL", "NULL"),font.align = "top",size=10,font.size=15,border.bottom=30)

        ledges2 <- data.frame(color =unique(vis.nodes$color),label = paste("Community",seq(1,length(unique(vis.nodes$color))),sep=""), arrows =rep("NULL", length(unique(vis.nodes$color))),font.align = "top",size=10,font.size=15,border.bottom=30)

        ledges<-rbind(ledges1,ledges2)



    }else{


        # passing custom nodes and/or edges
        lnodes <- data.frame(label =paste(unique(node_vec$type),unique(node_name),sep=""),
        shape = unique(vis.nodes$shape), color = unique(vis.nodes$color),size=rep(8,length(unique(vis.nodes$shape))),shape.size = rep(50, length(unique(node_name))),font.align="top",font.size=15)


          ledges <- data.frame(color = c("blue","red"),label = c("-ve correlation", "+ve correlation"), arrows =c("NULL", "NULL"),font.align = "top",font.size=15,border.bottom=30)


    }

    visnet<-visLegend(visnet, position="left", ncol=1,addNodes = lnodes, addEdges = ledges, useGroups = FALSE,main = list(text = "Legend", style = "font-family:Helvetica;color:blue;font-size:14px;text-align:left;"),stepX=100,stepY=100,width=0.125,zoom=TRUE)


    visnet<-visOptions(visnet,selectedBy = list(variable=c("type"), multiple=T,style = 'width: 200px; height: 26px;
    background: #f8f8f8;
    color: darkblue'), manipulation = TRUE,highlightNearest =list(hover=TRUE,enabled = T, degree = 1),
    nodesIdSelection = list(enabled = TRUE, style = 'width: 200px; height: 26px;
    background: #f8f8f8;
    color: darkblue;
    '))


    #if(nrow_df_matrix<100){


    set.seed(seednum)
    l = layoutmatrix #layout.fruchterman.reingold(sg, weights = (1 -abs(E(sg)$weight)))

    visnet<-visIgraphLayout(visnet,layout = "layout.norm", layoutMatrix = cbind(l[,1],(-1)*l[,2]),randomSeed=seednum)

    #visnet<-visIgraphLayout(visnet,layout = "layout.norm", layoutMatrix = l,randomSeed=100)
    #}

    visnet<-visExport(visnet,type="jpeg",float = "middle", label = "Save network",background="white",style='color: darkred;font-size:14px;font-family:Helvetica;width: 200px; height: 20px')

    #visnet<-visInteraction(visnet,navigationButtons = TRUE)
    visnet<-visInteraction(visnet,zoomView = TRUE,keyboard = TRUE, navigationButtons = TRUE)

    visnet<-visPhysics(visnet,stabilization = FALSE)
    visnet<-visEdges(visnet,smooth = FALSE)

    if(identical(communitymembership, NA)==TRUE){
    visSave(visnet, file = "interactive.network.html",selfcontained = html.selfcontained)
    }else{
        visSave(visnet, file = "interactive.network.communities.html",selfcontained = html.selfcontained)

    }
  return(visnet)
}
