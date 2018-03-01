getSumreplicateschild <-
function(curdata,alignment.tool,numreplicates,rep.max.missing.thresh,method="mean",missing.val)
{
    #curdata<-t(curdata)
    #write.table(curdata,file="test.txt",sep="\t",row.names=FALSE)
    numfeats=dim(curdata)[1]
    numsamp=dim(curdata)[2]
    # if(FALSE){
    resvec_1<-lapply(1:numfeats,function(r)
    {
        newrow={}
        finalmat={}
        #for(samp in seq(1,(numsamp),numreplicates))
        {
            # i=samp
            #j=i+numreplicates-1
            
            curdata_int=curdata[r,]
            
            #if(is.na(missing.val)==FALSE){
            #           check_zeros=which(curdata_int==missing.val)
            #           }else{
            check_zeros=which(is.na(curdata_int)==TRUE)
            #           	}
            na_thresh=round(rep.max.missing.thresh*numreplicates)
            
            
            if(length(check_zeros)>na_thresh)
            {
                meanval<-missing.val
            }
            else
            {
                #temporarily replace the missing intensities, set to 0 in apLCMS,
                #with mean intensity value of the corresponding replicates (with non-zero values)
                #curdata_int[check_zeros]=mean(t(curdata_int[-c(check_zeros)]))
                if(length(check_zeros)>0)
                {
                    if(method=="mean"){
                        meanval<-mean(t(curdata_int[-check_zeros]),na.rm=TRUE)
                    }else{
                        meanval<-median(t(curdata_int[-check_zeros]),na.rm=TRUE)
                    }
                }
                else
                {
                    if(method=="mean"){
                        meanval<-mean(t(curdata_int),na.rm=TRUE)
                    }else{
                        meanval<-median(t(curdata_int),na.rm=TRUE)
                    }
                }
                
            }
            newrow<-cbind(newrow,meanval)
        }
        
        
        finalmat<-rbind(finalmat, newrow)
        return(finalmat)
    })
    
    #colnames(final_set)<-colnames_data
    #rownames(final_set)=NULL
    return(resvec_1)
    
    
    
}
