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
  
<<<<<<< HEAD
  #keepX<-min(keepX,nrow(X))
  #keepY<-min(keepY,nrow(Y))
  
  opt_comp=1
  
   if(is.na(numcomp)==TRUE){
   
	numcomp=min(c(dim(X)[2],dim(Y)[2]))
   }
   
   if(analysismode=="canonical"){
       
       min_num_var=min(c(dim(X)[2],dim(Y)[2]))
       
       if(numcomp>min_num_var){
           numcomp=min_num_var
           
       }
       
   }
   
=======
  keepX<-min(keepX,ncol(X))
  keepY<-min(keepY,ncol(Y))
  
>>>>>>> 7d77755391041ddde01723e0f0155346233a0ed4
  
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
<<<<<<< HEAD
            
            if(is.na(numcomp)==TRUE){
                
                numcomp=nrow(X)-1 
                #numcomp=min(c(dim(X),dim(Y)))-1
                opt_comp=numcomp
                
                print(opt_comp)
            }else{
                opt_comp<-numcomp
            }
=======
            opt_comp<-numcomp
            
>>>>>>> 7d77755391041ddde01723e0f0155346233a0ed4
            keep_x_vec<-rep(keepX,opt_comp)
            keep_y_vec<-rep(keepY,opt_comp)
        }
    }
    
    
   
    if(oscmode=="o1pls"){
<<<<<<< HEAD
        
        X<-as.matrix(X)
        Y<-as.matrix(Y)
=======
>>>>>>> 7d77755391041ddde01723e0f0155346233a0ed4
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
    
<<<<<<< HEAD
    if(opt_comp>nrow(Y) | opt_comp>nrow(X)){
        
        #opt_comp<-min(opt_comp,nrow(X)-1,nrow(Y)-1)
        opt_comp<-min(opt_comp,nrow(X)-1)
=======
    if(opt_comp>ncol(Y) | opt_comp>ncol(X)){
        
        opt_comp<-min(opt_comp,ncol(X),ncol(Y))
        
>>>>>>> 7d77755391041ddde01723e0f0155346233a0ed4
    }
    if(sparseselect==TRUE)
    {
            if(repeatmeasures==TRUE){
                
<<<<<<< HEAD
                print("Design matrix")
                print(design)
                save(design,file="design.Rda")
                
                
=======
>>>>>>> 7d77755391041ddde01723e0f0155346233a0ed4
                #perform multilevel spls analysis
                linn.pls <- try(mixOmics::multilevel(X=X, design=design,ncomp = opt_comp,
                keepX = keep_x_vec, Y=Y,keepY=keep_y_vec,method = 'spls',mode=analysismode),silent=TRUE)
                
                if(is(linn.pls,"try-error")){
                    
<<<<<<< HEAD
                   
                
                if(analysismode=="classification"){
                    linn.pls <- mixOmics::splsda(X, Y,ncomp=opt_comp,keepX=keep_x_vec,mode="regression",multilevel=design)
                }else{
=======
                    print("Design matrix")
                    print(design)
                    save(design,file="design.Rda")
                    
                 
>>>>>>> 7d77755391041ddde01723e0f0155346233a0ed4
                    linn.pls <- mixOmics::spls(X, Y,ncomp=opt_comp,keepX=keep_x_vec,keepY=keep_y_vec,mode=analysismode,multilevel=design)
                    
                }
                
<<<<<<< HEAD
                }
                
=======
>>>>>>> 7d77755391041ddde01723e0f0155346233a0ed4
               
              

            }else{
<<<<<<< HEAD
                
                if(analysismode=="classification"){
                     linn.pls <- mixOmics::splsda(X, Y,ncomp=opt_comp,keepX=keep_x_vec,mode=analysismode)
                }else{
                    linn.pls <- mixOmics::spls(X, Y,ncomp=opt_comp,keepX=keep_x_vec,keepY=keep_y_vec,mode=analysismode)
                }
=======
                linn.pls <- mixOmics::spls(X, Y,ncomp=opt_comp,keepX=keep_x_vec,keepY=keep_y_vec,mode=analysismode)
>>>>>>> 7d77755391041ddde01723e0f0155346233a0ed4
            }
     
        
    }else{
        
<<<<<<< HEAD
                 if(analysismode=="classification"){
                     linn.pls <- mixOmics::plsda(X, Y,ncomp=opt_comp,mode=analysismode)
                 }else{
                        linn.pls <- mixOmics::pls(X, Y,ncomp=opt_comp,mode=analysismode)
                 }
  }
=======
                linn.pls <- mixOmics::pls(X, Y,ncomp=opt_comp,mode=analysismode)
    }
>>>>>>> 7d77755391041ddde01723e0f0155346233a0ed4
    

    return(list("model"=linn.pls,"opt_comp"=opt_comp))
    
}
