network_custom <-
function (mat, comp = NULL, blocks = c(1, 2), analysismode="regression")
{
    
    save(mat,file="mat.Rda")
    object.pls = c("pls", "spls", "mlspls", "mlpls","mixo_mlpls","mixo_pls","mixo_spls")
    object.rcc = c("rcc","mixOmics::rcc","mixo_rcc")
    object.blocks = c("sgcca", "rgcca","mixo_sgcca")
    class.object = class(mat)

   
    if (any(class.object == "DA"))
    mat$Y = mat$ind.mat
    
    if (any(class.object %in% object.pls)) {
    
		p = ncol(mat$X)
	    q = ncol(mat$Y)
	    n = nrow(mat$X)
	    ncomp = mat$ncomp
	    comp = 1:mat$ncomp
	    row.names = mat$names$colnames$X
	    col.names = mat$names$colnames$Y
        if (all(class(mat) %in% "pls")) {
            keep.X = rep(TRUE, p)
            keep.Y = rep(TRUE, q)
        }
        else {
            #sPLS: sparse criteria used for feature selection
            keep.X = apply(abs(mat$loadings$X[, comp, drop = FALSE]),1, sum) > 0
            keep.Y = apply(abs(mat$loadings$Y[, comp, drop = FALSE]),1, sum) > 0
            row.names = row.names[keep.X]
            col.names = col.names[keep.Y]
        }
        if (mat$mode == "canonical") {
            cord.X = cor(mat$X[, keep.X], mat$variates$X[,comp], use = "pairwise")
            cord.Y = cor(mat$Y[, keep.Y], mat$variates$Y[,comp], use = "pairwise")
        }
        else {
	   
	   #for regression: 
            cord.X = cor(mat$X[, keep.X], mat$variates$X[,comp], use = "pairwise") #calculate P loading matrix for X
            cord.Y = cor(mat$Y[, keep.Y], mat$variates$X[,comp], use = "pairwise") #calculate Q loading matrix for Y
        }
        cor_mat = cord.X %*% t(cord.Y) #dot product of P and Q loading matrices
        
        cor_mat[cor_mat>1]<-1
        cor_mat[cor_mat<(-1)]<-(-1)
        #cor_mat<-round(cor_mat,4)
        #M1<-M[which(abs(M)<cutoff)]<-0
        
        
        return(list(M=cor_mat))
    }else{
	   if (any(class.object %in% c("mvr"))) {
           
           
			
			   #for regression: 
               cord.X = cor(mat$model$X, mat$scores[,1:comp], use = "pairwise") #calculate P loading matrix for X
               cord.Y = cor(mat$model$Y, mat$scores[,1:comp], use = "pairwise") #calculate Q loading matrix for Y
			    
			    #cord.X = cor(X, mat$T[,comp], use = "pairwise") #calculate P loading matrix for X
			  #  cord.Y = cor(Y, mat$T[,comp], use = "pairwise") #calculate Q loading matrix for Y
			
			cor_mat = cord.X %*% t(cord.Y) #dot product of P and Q loading matrices
			
			cor_mat[cor_mat>1]<-1
			cor_mat[cor_mat<(-1)]<-(-1)
			#cor_mat<-round(cor_mat,4)
			#M1<-M[which(abs(M)<cutoff)]<-0
			#cor((X),(Y[,1]))

			
			return(list(M=cor_mat))
							
		
	}else{
	
		  if (any(class.object %in% c("mixOmics::rcc", "rcc"))) {
			
            bisect = mat$variates$X[, 1:comp] + mat$variates$Y[, 1:comp]
			cord.X = cor(mat$X, bisect, use = "pairwise")
			cord.Y = cor(mat$Y, bisect, use = "pairwise")
			cor_mat = cord.X %*% t(cord.Y)

			
			return(list(M=cor_mat))
		}
	}
    }
}
