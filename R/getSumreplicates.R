getSumreplicates <-
function(curdata,alignment.tool,numreplicates,numcluster,rep.max.missing.thresh,summary.method="mean",summary.na.replacement="zeros",missing.val=0)
{
    mean_replicate_difference<-{}
    sd_range_duplicate_pairs<-{}
		  #print(alignment.tool)
          if(alignment.tool=="apLCMS")
          {
              col_end=2
          }
          else
          {
              if(alignment.tool=="XCMS")
              {
                  col_end=2
              }
              else
              {
                  stop(paste("Invalid value for alignment.tool. Please use either \"apLCMS\" or \"XCMS\"", sep=""))
              }
          }
          
          curdata_mz_rt_info=curdata[,c(1:col_end)]
          curdata=curdata[,-c(1:col_end)]
          
          
          
          cl<-makeCluster(numcluster)
          numfeats=dim(curdata)[1]
          numsamp=dim(curdata)[2]
          
          clusterEvalQ(cl, "getSumreplicateschild")
          sub_samp_list<-list()
          
          sampcount=1
          for(samp in seq(1,(numsamp),numreplicates))
          {
              i=samp
              j=i+numreplicates-1
              if(dim(curdata[,c(i:j)])[1]>0){
                  sub_samp_list[[sampcount]]=curdata[,c(i:j)]
              }
              sampcount=sampcount+1
          }
          
          avg.res<-parSapply(cl,sub_samp_list,getSumreplicateschild,alignment.tool=alignment.tool,numreplicates=numreplicates,rep.max.missing.thresh=rep.max.missing.thresh,method=summary.method,missing.val=missing.val)
          #avg.res<-getAvgreplicateschild(sub_samp_list[[1]],alignment.tool,numreplicates)
          #print("done")
          
          
          stopCluster(cl)
          
          
          
          final_set<-as.data.frame(avg.res)
          colnames_data<-colnames(curdata)
          colnames_data<-colnames_data[seq(1,(numsamp),numreplicates)]
          colnames(final_set)<-colnames_data
          rownames(final_set)=NULL
          #final_set<-cbind(curdata_mz_rt_info,final_set)
          
          final_set<-apply(final_set,2,as.numeric)
          #	write.table(final_set,file="final_Set.txt",sep="\t",row.names=FALSE)
          
          if(summary.na.replacement=="zeros"){
              
              final_set<-replace(final_set,which(is.na(final_set)==TRUE),0)
          }else{
              if(summary.na.replacement=="halfsamplemin"){
                  
                  
                  final_set<-apply(final_set,2,function(x){naind<-which(is.na(x)==TRUE); if(length(naind)>0){x[naind]<-min(x,na.rm=TRUE)/2}; return(x)})
              }else{
                  
                  if(summary.na.replacement=="halfdatamin"){
                      
                      
                      min_val<-min(final_set,na.rm=TRUE)*0.5
                      final_set<-replace(final_set,which(is.na(final_set)==TRUE),min_val)
                      
                      #data_m<-apply(data_m,1,function(x){naind<-which(is.na(x)==TRUE); if(length(naind)>0){x[naind]<-min(x,na.rm=TRUE)/2}; return(x)})
                  }else{
                      if(summary.na.replacement=="halffeaturemin"){
                          
                          
                          final_set<-apply(final_set,1,function(x){naind<-which(is.na(x)==TRUE); if(length(naind)>0){x[naind]<-min(x,na.rm=TRUE)/2}; return(x)})
                          final_set<-t(final_set)
                      }
                  }
              }
              
              
          }
          
          return(final_set)
}
