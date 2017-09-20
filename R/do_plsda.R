do_plsda <-
function(X,Y,oscmode="pls",numcomp=3,keepX=15,keepY=15,sparseselect=FALSE,analysismode="regression",pairedanalysis=FALSE,optselect=FALSE,design=NA)
{
    repeatmeasures=pairedanalysis
    
    #X<-t(X)
    
    #Y<-as.numeric(Y)
    
    X<-as.data.frame(X)
    Y<-as.data.frame(Y)
    #print(Y)
    classlabels<-Y
    
    #opt_comp<-3
  
  if(is.na(keepX)==TRUE){
      
      keepX=dim(X)[2]
  }
  
  if(is.na(keepY)==TRUE){
      
      keepY=dim(Y)[2]
  }
  

  
  
    if(dim(X)[2]>1){
        if(optselect==TRUE){
           
                    set.seed(123)
                    opt_comp<-pls.regression.cv(Xtrain=X, Ytrain=Y,  ncomp=c(1:numcomp), nruncv=10, alpha=2/3)
                
                if(opt_comp<2){
                    opt_comp<-2
                }
                
                 keep_x_vec<-rep(keepX,opt_comp)
                 keep_y_vec<-rep(keepY,opt_comp)
            
        }else{
            opt_comp<-numcomp
            keep_x_vec<-rep(keepX,opt_comp)
            keep_y_vec<-rep(keepY,opt_comp)
        }
    }
    
    
   
    if(oscmode=="o1pls"){
        leukemia.pls <- plsr(Y ~ X, ncomp = opt_comp, validation = "LOO")
        ww <- leukemia.pls$loading.weights[,1]
        pp <- leukemia.pls$loadings[,1]
        w.ortho <- pp - crossprod(ww, pp)/crossprod(ww) * ww
        t.ortho <- X %*% w.ortho
        
        p.ortho <- crossprod(X, t.ortho) / c(crossprod(t.ortho))
        Xcorr <- X - tcrossprod(t.ortho, p.ortho)
        
        
        X<-Xcorr
    }
 
    bad_variables<-{}
    
    if(sparseselect==TRUE)
    {
            if(repeatmeasures==TRUE){
                
<<<<<<< HEAD
                #print("design")
                #print(design)
                
=======

>>>>>>> fb62679be2b513df3fd6da6bfbbbdca750be494c
                linn.pls <- try(mixOmics::multilevel(X=X, design=design,ncomp = opt_comp,
                keepX = keep_x_vec, Y=Y,keepY=keep_y_vec,method = 'spls',mode=analysismode),silent=TRUE)
                
                if(is(linn.pls,"try-error")){
                    
<<<<<<< HEAD
                    #save(Y,file="Y.Rda")
                    #save(design,file="design.Rda")
=======
                   
                   
>>>>>>> fb62679be2b513df3fd6da6bfbbbdca750be494c
                    linn.pls <- mixOmics::spls(X, Y,ncomp=opt_comp,keepX=keep_x_vec,keepY=keep_y_vec,mode=analysismode,multilevel=design)
                    
                }
                
               
<<<<<<< HEAD
               #save(linn.pls,file="linn_pls.Rda")
=======
               
>>>>>>> fb62679be2b513df3fd6da6bfbbbdca750be494c

            }else{
                linn.pls <- mixOmics::spls(X, Y,ncomp=opt_comp,keepX=keep_x_vec,keepY=keep_y_vec,mode=analysismode)
            }
     
        
    }else{
        
                linn.pls <- mixOmics::pls(X, Y,ncomp=opt_comp,mode=analysismode)
    }
    
    #save(linn.pls,file="pls_res.Rda")
    return(list("model"=linn.pls,"opt_comp"=opt_comp))
    
}
