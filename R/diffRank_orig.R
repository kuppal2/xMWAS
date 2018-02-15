diffRank_orig <-
function(adjMtrxSample1,adjMtrxSample2,lambda,eps,DBC){
    
    #####Connectivity
    ##### Centrality
    
    #N is the number if genes in the sample.
    N=dim(adjMtrxSample1)[1];
    
    
    #Create and initialize Delta_C_i
    Delta_C_i=abs(adjMtrxSample1-adjMtrxSample2);
    for(u in 1:N)
    {
        # In the case where there is an element (GENE), is not connected to any other genes, then initialze it to 1/N.
        if(sum(Delta_C_i[u,])==0)
        {
            Delta_C_i[u,]=1/N
        }
        Delta_C_i[u,]=Delta_C_i[u,]/sum(Delta_C_i[u,])
    }
    error=100;
    count=1;
    
    # Create and initialize solution array to 1/N.
    # solution contains the ranks for all genes.
    solution=array(1/N,dim=c(N,1))
    solutionEachIteration=solution;
    
    # eps (EPSILON) is a value of the difference between 2 consecutive solutions to stop the iterations.
    # Do iterate while the difference between 2 consecutive solutions is not that much big (> eps).
    while(error > eps)
    {
        count=count+1;
        
        #Keep the previous solution
        formerSoulution=solution;
        
        #Find a new solution.
        for (v in 1:N)
        {
            s = sum(Delta_C_i[,v] * solution); # Solution, is the (Pi ) in this case
            solution[v]=(1-lambda) * DBC[v] + lambda * s;
        }
        solutionEachIteration=array(c(solutionEachIteration,solution),dim=c(N,count))
        
        #find the error to stop the iteration, the error in this case that there no significant difference between the old and the new solution
        error=sum((formerSoulution - solution)^2)
    }
    
    
    
    
    # Create a graph from the dataset; one graph for each condition.
    graph1=graph.adjacency(adjMtrxSample1, mode="undirected",weighted=TRUE)#, diag=FALSE )
    graph2=graph.adjacency(adjMtrxSample2, mode="undirected",weighted=TRUE)#, diag=FALSE )
    
    plot(graph1)
    plot(graph2)
    
    
    # Find the betweenness of each graph.
    betweennessGraph1=betweenness(graph1,directed = FALSE);
    betweennessGraph2=betweenness(graph2,directed = FALSE) ;
    
    #Find the degrees of each graph.
    graph1Degree=degree(graph1);
    graph2Degree=degree(graph2);
    
    # Find the degrees normalized to the maximum degree in the graph.
    graph1DegreeNormalized = graph1Degree / max(graph1Degree);
    graph2DegreeNormalized = graph2Degree / max(graph2Degree);
    
    diffK=rank(N + 1 - abs(graph1DegreeNormalized-graph2DegreeNormalized))
    
    rnk = rank(1-solution)
    
    pageRankGraph1 = page.rank ( graph1 )$vector
    pageRankGraph2 = page.rank ( graph2 )$vector
    
    pageRankGraph1Rnk=rank(1-pageRankGraph1)
    pageRankGraph2Rnk=rank(1-pageRankGraph2)
    
    dpr=rank(N + 1 - abs(pageRankGraph1Rnk-pageRankGraph2Rnk))
    
    
    clink=array(0,dim=c(N,1))
    
    for(i in 1:N)
    {
        c=0;
        for (j in 1:N)
        {
            if(adjMtrxSample1[i,j] > 0 & adjMtrxSample2[i,j] > 0)
            {
                c=c+1
            }
        }
        clink[i]=c;
    }
    
    Delta_C_i2 = array(0,dim=c(N,1))
    
    
    graph1Degree[graph1Degree==0] = 0.1;
    graph2Degree[graph2Degree==0] = 0.1;
    
    for(i in 1:N)
    {
        Delta_C_i2[i]=max(log10(graph1Degree[i]/graph2Degree[i]),log10(graph2Degree[i]/graph1Degree[i]))
    }
    
    Delta_C_i2=rank(N + 1 - Delta_C_i2)
    results=array(c(1:N ,graph1Degree,betweennessGraph1,pageRankGraph1Rnk,graph2Degree,betweennessGraph2,pageRankGraph2Rnk,dpr,diffK,Delta_C_i2,clink,DBC,solution,rnk),dim=c(N,14))
    results=data.frame(results);
    names(results)[1]="gene No."
    names(results)[2]="degree in NT 1"
    names(results)[3]="BC in NT 1"
    names(results)[4]="Page Rank value in Graph 1" #pageRankGraph1Rnk
    names(results)[5]="degree in N2"
    names(results)[6]="BC in NT 2"
    names(results)[7]="Page Rank value in Graph 2" #pageRankGraph2Rnk
    names(results)[8]="dPR"
    names(results)[9]="diffK"
    names(results)[10]="Delta_C_i"
    names(results)[11]="Common Links"
    names(results)[12]="DBC"
    names(results)[13]="solution"
    names(results)[14]="Rank"
    write.table(results,"results.csv",append = FALSE, quote = FALSE,sep=",",row.names=FALSE)
    #return(res)
}
