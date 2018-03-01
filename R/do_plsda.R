do_plsda <-
function(X,Y,oscmode="pls",numcomp=3,keepX=15,keepY=15,sparseselect=FALSE,analysismode="regression",pairedanalysis=FALSE,optselect=FALSE,design=NA)
{
    repeatmeasures=pairedanalysis
    

    X<-as.data.frame(X)
    Y<-as.data.frame(Y)
 
    classlabels<-Y
    
   
  
  if(is.na(keepX)==TRUE){
      
      keepX=dim(X)[2]
  }
  
  if(is.na(keepY)==TRUE){
      
      keepY=dim(Y)[2]
  }
  
  keepX<-min(keepX,ncol(X))
  keepY<-min(keepY,ncol(Y))
  
  
    if(dim(X)[2]>1){
        if(optselect==TRUE){
           
           #find optimal number of components
                    set.seed(123)
                    opt_comp<-pls.regression.cv(Xtrain=X, Ytrain=Y,  ncomp=c(1:numcomp), nruncv=10, alpha=2/3)
                
                if(opt_comp<2){
                    
                   opt_comp<-2
                }
                
                 keep_x_vec<-rep(keepX,opt_comp)
                 keep_y_vec<-rep(keepY,opt_comp)
            
        }else{
            
            #use user-defined number of components
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
    
    if(opt_comp>ncol(Y) | opt_comp>ncol(X)){
        
        opt_comp<-min(opt_comp,ncol(X),ncol(Y))
        
    }
    if(sparseselect==TRUE)
    {
            if(repeatmeasures==TRUE){
                
                #perform multilevel spls analysis
                linn.pls <- try(mixOmics::multilevel(X=X, design=design,ncomp = opt_comp,
                keepX = keep_x_vec, Y=Y,keepY=keep_y_vec,method = 'spls',mode=analysismode),silent=TRUE)
                
                if(is(linn.pls,"try-error")){
                    
                    print("Design matrix")
                    print(design)
                    save(design,file="design.Rda")
                    
                 
                    linn.pls <- mixOmics::spls(X, Y,ncomp=opt_comp,keepX=keep_x_vec,keepY=keep_y_vec,mode=analysismode,multilevel=design)
                    
                }
                
               
              

            }else{
                linn.pls <- mixOmics::spls(X, Y,ncomp=opt_comp,keepX=keep_x_vec,keepY=keep_y_vec,mode=analysismode)
            }
     
        
    }else{
        
                linn.pls <- mixOmics::pls(X, Y,ncomp=opt_comp,mode=analysismode)
    }
    

    return(list("model"=linn.pls,"opt_comp"=opt_comp))
    
}
