filter.by.missing.values <-
function(data_m,missing.val=0,all.missing.thresh=0.5){
    
    
    ##################################################################################
    metab_zeros={}
    data_clean<-{}
    clean_metabs<-{}
    
    if(is.na(all.missing.thresh)==FALSE)
    {
        
        total_sigs<-apply(data_m,1,function(x){
            
            #if missing.val=0 or a non NA value
            if(is.na(missing.val)==FALSE){return(length(which(x>missing.val)))
            }else{
                
                #if missing.val=NA
                return(length(which(is.na(x)==FALSE)))
            }})
        
        
        
        total_sig_thresh<-dim(data_m)[2]*all.missing.thresh
        
        total_good_metabs<-which(total_sigs>total_sig_thresh)
        
    }else{
        
        total_good_metabs<-seq(1,nrow(data_m))
    }
    
    #remove bad features based on all missing values criteria
    if(length(total_good_metabs)>0){
        data_m<-data_m[total_good_metabs,]
       
       
        print(paste("Dimension of data matrix after using overall ",100*all.missing.thresh, "% signal criteria for filtering:"),sep="")
        print(dim(data_m))
    }else{
        #stop(paste("None of the  have signal in ",all.missing.thresh*100, "% of samples",sep=""))
        data_m<-{}
    }
    return(data_m)
}
