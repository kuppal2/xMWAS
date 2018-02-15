<<<<<<< HEAD
data_preprocess <-
function(Xmat=NA,Ymat=NA,feature_table_file,parentoutput_dir,class_labels_file,num_replicates=3,feat.filt.thresh=NA,summarize.replicates=TRUE,summary.method="mean",
all.missing.thresh=0.5,group.missing.thresh=0.7,
log2transform=TRUE,medcenter=TRUE,znormtransform=FALSE,quantile_norm=TRUE,lowess_norm=FALSE,madscaling=FALSE,missing.val=0,samplermindex=NA, rep.max.missing.thresh=0.5,summary.na.replacement="zeros",featselmethod=NA){
    
    options(warn=-1)
    
    #read file; First row is column headers
    if(is.na(Xmat==TRUE)){
        data_matrix<-read.table(feature_table_file,sep="\t",header=TRUE)
    }else{
        data_matrix<-Xmat
        #rm(Xmat)
    }
    
    #print("signal filter threshold ")
    #print(group.missing.thresh)
    
    print("missing val is")
    print(missing.val)
    
    if(is.na(all.missing.thresh)==TRUE){
        
        all.missing.thresh=0
    }
    
    if(is.na(samplermindex)==FALSE){
        data_matrix<-data_matrix[,-c(samplermindex)]
    }
    
    #use only unique records
    data_matrix<-unique(data_matrix)
    
    if(is.na(missing.val)==FALSE){
        
        print("Replacing missing values with NAs.")
        data_matrix<-replace(as.matrix(data_matrix),which(data_matrix==missing.val),NA)
    }
    
    # print(data_matrix[1:10,1:5])
    
    #print("dim of original data matrix")
    #print(dim(data_matrix))
    data_matrix_orig<-data_matrix
    
    
    snames<-colnames(data_matrix)
    
    
    
    
    
    suppressWarnings(dir.create(parentoutput_dir,showWarnings=FALSE))
    parentoutput_dir<-paste(parentoutput_dir,"/Stage1/",sep="")
    
    suppressWarnings(dir.create(parentoutput_dir,showWarnings=FALSE))
    fheader="transformed_log2fc_threshold_"
    setwd(parentoutput_dir)
    
    data_m<-as.matrix(data_matrix[,-c(1:2)])
    
    if(is.na(Xmat)==FALSE){
        
        #   write.table(Xmat,file="organized_featuretableA.txt",sep="\t",row.names=TRUE)
        
        
    }
    
    if(is.na(Ymat)==FALSE){
        # write.table(Ymat,file="organized_classlabelsA.txt",sep="\t",row.names=FALSE)
        
    }
    
    #Step 2) Average replicates
    if(summarize.replicates==TRUE)
    {
        if(num_replicates>1)
        {
            
            data_m<-getSumreplicates(data_matrix,alignment.tool="apLCMS",numreplicates=num_replicates,numcluster=10,rep.max.missing.thresh=rep.max.missing.thresh,summary.method=summary.method,summary.na.replacement, missing.val=missing.val)
            
            
            data_m<-replace(data_m,which(is.na(data_m)==TRUE),missing.val)
            
            if(summary.method=="mean"){
                print("Replicate averaging done")
                filename<-paste("Rawdata_averaged.txt",sep="")
            }else{
                if(summary.method=="median"){
                    print("Replicate median summarization done")
                    filename<-paste("Rawdata_median_summarized.txt",sep="")
                }
                
            }
            
            data_m_prenorm<-cbind(data_matrix[,c(1:2)],data_m)
            
            write.table(data_m_prenorm, file=filename,sep="\t",row.names=FALSE)
            
            data_matrix<-cbind(data_matrix[,c(1:2)],data_m)
            #num_samps_group[[1]]=(1/num_replicates)*num_samps_group[[1]]
            #num_samps_group[[2]]=(1/num_replicates)*num_samps_group[[2]]
        }
    }
    
    
    data_matrix<-cbind(data_matrix[,c(1:2)],data_m)
    
    data_matrix_orig<-data_matrix
    data_subjects<-data_m
    
    ordered_labels={}
    
    num_samps_group<-new("list")
    
    if(is.na(class_labels_file)==FALSE)
    {
        
        print("Class labels file:")
        print(class_labels_file)
        
        data_matrix={}
        
        if(is.na(Ymat)==TRUE){
            classlabels<-read.table(class_labels_file,sep="\t",header=TRUE)
        }else{
            classlabels<-Ymat
        }
        
        #class_labels_sampnames<-classlabels[,1]
        #data_matrix_sampnames<-colnames(data_m)
        
        #classlabels<-classlabels[match(class_labels_sampnames,data_matrix_sampnames),]
        
        classlabels<-as.data.frame(classlabels)
        
        cnames1<-colnames(classlabels)
        cnames1[1]<-c("SampleID")
        cnames1[2]<-c("Class")
        
        colnames(classlabels)<-cnames1 #c("SampleID","Class")
        f1<-table(classlabels$SampleID)
        
        
        classlabels<-as.data.frame(classlabels)
        classlabels<-classlabels[seq(1,dim(classlabels)[1],num_replicates),]
        #print(classlabels)
        class_labels_levels<-levels(as.factor(classlabels[,2]))
        bad_rows<-which(class_labels_levels=="")
        if(length(bad_rows)>0){
            class_labels_levels<-class_labels_levels[-bad_rows]
        }
        
        for(c in 1:length(class_labels_levels))
        {
            if(c>1){
                data_matrix<-cbind(data_matrix,data_subjects[,which(classlabels[,2]==class_labels_levels[c])])
            }else{
                data_matrix<-data_subjects[,which(classlabels[,2]==class_labels_levels[c])]
            }
            classlabels_index<-which(classlabels[,2]==class_labels_levels[c])
            ordered_labels<-c(ordered_labels,as.character(classlabels[classlabels_index,2]))
            num_samps_group[[c]]<-length(classlabels_index)
            
        }
        
        #colnames(data_matrix)<-as.character(ordered_labels)
        data_matrix<-cbind(data_matrix_orig[,c(1:2)],data_matrix)
        data_m<-as.matrix(data_matrix[,-c(1:2)])
        
        
    }else
    {
        if(is.na(Ymat)==TRUE)
        {
            classlabels<-rep("A",dim(data_m)[2])
            ordered_labels<-classlabels
            num_samps_group[[1]]<-dim(data_m)[2]
            class_labels_levels<-c("A")
            data_m<-as.matrix(data_matrix[,-c(1:2)])
            
        }else{
            classlabels<-Ymat
            classlabels<-as.data.frame(classlabels)
            cnames1<-colnames(classlabels)
            cnames1[1]<-c("SampleID")
            cnames1[2]<-c("Class")
            
            colnames(classlabels)<-cnames1
            #colnames(classlabels)<-c("SampleID","Class")
            f1<-table(classlabels$SampleID)
            
            
            classlabels<-as.data.frame(classlabels)
            classlabels<-classlabels[seq(1,dim(classlabels)[1],num_replicates),]
            #print(classlabels)
            class_labels_levels<-levels(as.factor(classlabels[,2]))
            bad_rows<-which(class_labels_levels=="")
            if(length(bad_rows)>0){
                class_labels_levels<-class_labels_levels[-bad_rows]
            }
            
            for(c in 1:length(class_labels_levels))
            {
                #if(c>1){
                #data_matrix<-cbind(data_matrix,data_subjects[,which(classlabels[,2]==class_labels_levels[c])])
                #}else{
                #	data_matrix<-data_subjects[,which(classlabels[,2]==class_labels_levels[c])]
                #}
                classlabels_index<-which(classlabels[,2]==class_labels_levels[c])
                ordered_labels<-c(ordered_labels,as.character(classlabels[classlabels_index,2]))
                num_samps_group[[c]]<-length(classlabels_index)
                
            }
            
            #colnames(data_matrix)<-as.character(ordered_labels)
            #data_matrix<-cbind(data_matrix_orig[,c(1:2)],data_matrix)
            #data_m<-as.matrix(data_matrix[,-c(1:2)])
        }
        
        
        
    }
    
    
    rnames_xmat<-colnames(data_matrix[,-c(1:2)])
    rnames_ymat<-as.character(classlabels[,1])
    
    
    for(ind1 in 1:length(rnames_xmat)){
        check_ylabel<-regexpr(rnames_ymat[ind1],pattern="^[0-9]*",perl=TRUE)
        check_xlabel<-regexpr(rnames_xmat[ind1],pattern="^X[0-9]*",perl=TRUE)
        
        if(length(check_ylabel)>0 && length(check_xlabel)>0){
            if(attr(check_ylabel,"match.length")>0 && attr(check_xlabel,"match.length")>0){
                
                rnames_ymat<-paste("X",rnames_ymat,sep="") #gsub(rnames_ymat,pattern="\\.[0-9]*",replacement="")
                
                
            }
        }
        
    }
    
    match_names<-match(rnames_xmat,rnames_ymat)
    
    bad_colnames<-length(which(is.na(match_names)==TRUE))
    
    classlabels<-classlabels[match_names,]
    
    #Step 3a) Remove features if signal is not detected in at least x% of all samples
    ##################################################################################
    metab_zeros={}
    data_clean<-{}
    clean_metabs<-{}
    #num_samps_group[[3]]<-0
    
  
    
    if(is.na(all.missing.thresh)==FALSE)
    {
        
        total_sigs<-apply(data_m,1,function(x){
            if(is.na(missing.val)==FALSE){return(length(which(x>missing.val)))
            }else{
                return(length(which(is.na(x)==FALSE)))
            }})
        
        
        
        total_sig_thresh<-dim(data_m)[2]*all.missing.thresh
        
        total_good_metabs<-which(total_sigs>total_sig_thresh)
        
    }
    
    #remove bad features based on all missing values criteria
    if(length(total_good_metabs)>0){
        data_m<-data_m[total_good_metabs,]
        data_matrix<-data_matrix[total_good_metabs,]
        #print(paste("Dimension of data matrix after overall ",all.missing.thresh,"% signal threshold filtering",sep=""))
        print(paste("Dimension of data matrix after using overall ",100*all.missing.thresh, "% signal criteria for filtering:"),sep="")
        print(dim(data_matrix))
    }else{
        stop(paste("None of the metabolites have signal in ",all.missing.thresh*100, "% of samples",sep=""))
    }
    
    
    #Step 3b) Find features for which the signal is not detected in at least x% of samples in either of the groups
    
    
    data_m<-data_matrix[,-c(1:2)]
    
    if(is.na(group.missing.thresh)==FALSE)
    {
        
        if(length(class_labels_levels)==0)
        {
            
            sig_thresh_groupA<-group.missing.thresh*num_samps_group[[1]]
            sig_thresh_groupB<-group.missing.thresh*num_samps_group[[2]]
            
            for(metab_num in 1:dim(data_matrix)[1])
            {
                #print(missing.val)
                if(is.na(missing.val)==FALSE){
                    
                    num_sigsA<-length(which(data_m[metab_num,1:num_samps_group[[1]]]>missing.val))
                    
                    
                    num_sigsB<-length(which(data_m[metab_num,(num_samps_group[[1]]+1):(num_samps_group[[1]]+num_samps_group[[2]])]>missing.val))
                    
                }else{
                    
                    num_sigsA<-length(which(is.na(data_m[metab_num,1:num_samps_group[[1]]])==FALSE))
                    num_sigsB<-length(which(is.na(data_m[metab_num,(num_samps_group[[1]]+1):(num_samps_group[[1]]+num_samps_group[[2]])])==FALSE))
                    
                }
                
                if((num_sigsA>=sig_thresh_groupA) || (num_sigsB>=sig_thresh_groupB))
                {
                    clean_metabs<-c(clean_metabs,metab_num)
                }
                
            }
            
            #print(length(clean_metabs))
        }else{
            if(length(class_labels_levels)==0){
                
                
                sig_thresh_groupA<-group.missing.thresh*num_samps_group[[1]]
                sig_thresh_groupB<-group.missing.thresh*num_samps_group[[2]]
                sig_thresh_groupC<-group.missing.thresh*num_samps_group[[3]]
                
                for(metab_num in 1:dim(data_matrix)[1])
                {
                    if(is.na(missing.val)==FALSE){
                        num_sigsA<-length(which(data_m[metab_num,1:num_samps_group[[1]]]>missing.val))
                        num_sigsB<-length(which(data_m[metab_num,(num_samps_group[[1]]+1):(num_samps_group[[1]]+num_samps_group[[2]])]>missing.val))
                        num_sigsC<-length(which(data_m[metab_num,(num_samps_group[[1]]+num_samps_group[[2]]+1):(num_samps_group[[1]]+num_samps_group[[2]]+num_samps_group[[3]])]>missing.val))
                    }else{
                        
                        num_sigsA<-length(which(is.na(data_m[metab_num,1:num_samps_group[[1]]])==FALSE))
                        num_sigsB<-length(which(is.na(data_m[metab_num,(num_samps_group[[1]]+1):(num_samps_group[[1]]+num_samps_group[[2]])])==FALSE))
                        num_sigsC<-length(which(is.na(data_m[metab_num,(num_samps_group[[1]]+num_samps_group[[2]]+1):(num_samps_group[[1]]+num_samps_group[[2]]+num_samps_group[[3]])])==FALSE))
                        
                    }
                    
                    if((num_sigsA>=sig_thresh_groupA) || (num_sigsB>=sig_thresh_groupB) || (num_sigsC>=sig_thresh_groupC))
                    {
                        clean_metabs<-c(clean_metabs,metab_num)
                    }
                    
                }
            }else{
                if(length(class_labels_levels)>1){
                    
                    
                    
                    for(metab_num in 1:dim(data_m)[1])
                    {
                        for(c in 1:length(class_labels_levels)){
                            
                            classlabels_index<-which(classlabels[,2]==class_labels_levels[c])
                            templabels<-classlabels[,2]
                            if(is.na(missing.val)==FALSE){
                                
                                num_cursig<-length(which(data_m[metab_num,which(templabels==class_labels_levels[c])]>missing.val))
                                
                            }else{
                                
                                num_cursig<-length(which(is.na(data_m[metab_num,which(templabels==class_labels_levels[c])])==FALSE))
                                
                                
                            }
                            
                            sig_thresh_cur<-length(which(templabels==class_labels_levels[c]))*group.missing.thresh
                            if(num_cursig>=sig_thresh_cur)
                            {
                                clean_metabs<-c(clean_metabs,metab_num)
                                break   #for(i in 1:4){if(i==3){break}else{print(i)}}
                                
                            }
                            
                        }
                    }
                }
                else{
                    
                    
                    
                    if(length(class_labels_levels)==1){
                        num_samps_group[[1]]<-num_samps_group[[1]]
                        
                        
                        sig_thresh_groupA<-group.missing.thresh*num_samps_group[[1]]
                        
                        
                        for(metab_num in 1:dim(data_matrix)[1])
                        {
                            if(is.na(missing.val)==FALSE){
                                num_sigsA<-length(which(data_m[metab_num,1:num_samps_group[[1]]]>missing.val))
                                
                            }else{
                                
                                num_sigsA<-length(which(is.na(data_m[metab_num,1:num_samps_group[[1]]])==FALSE))
                            }
                            
                            if((num_sigsA>=sig_thresh_groupA) )
                            {
                                clean_metabs<-c(clean_metabs,metab_num)
                            }
                            
                        }
                    }
                }
            }
        }
        
        
        
    }
    ####################################################################################
    
    #Step 4) Replace missing values
    if(summarize.replicates==TRUE)
    {
        
        {
            
            if(is.na(missing.val)==FALSE){
                
                print("Replacing missing values with NAs.")
                data_m<-replace(as.matrix(data_m),which(data_m==missing.val),NA)
            }
            
            
            if(summary.na.replacement=="zeros"){
                data_m<-replace(data_m,which(is.na(data_m)==TRUE),0)
            }else{
                if(summary.na.replacement=="halfsamplemin"){
                    data_m<-apply(data_m,2,function(x){naind<-which(is.na(x)==TRUE); if(length(naind)>0){x[naind]<-min(x,na.rm=TRUE)/2}; return(x)})
                }else{
                    
                    if(summary.na.replacement=="halfdatamin"){
                        
                        
                        min_val<-min(data_m,na.rm=TRUE)*0.5
                        data_m<-replace(data_m,which(is.na(data_m)==TRUE),min_val)
                        
                        #data_m<-apply(data_m,1,function(x){naind<-which(is.na(x)==TRUE); if(length(naind)>0){x[naind]<-min(x,na.rm=TRUE)/2}; return(x)})
                    }else{
                        if(summary.na.replacement=="halffeaturemin"){
                            data_m<-apply(data_m,1,function(x){naind<-which(is.na(x)==TRUE); if(length(naind)>0){x[naind]<-min(x,na.rm=TRUE)/2}; return(x)})
                            data_m<-t(data_m)
                        }else{
                            
                            
                            if(summary.na.replacement=="bpca"){
                                library(pcaMethods)
                                pc1 <- pcaMethods::pca(t(data_m), method="bpca", nPcs=10)
                                
                                data_m<-pcaMethods::completeObs(pc1)
                                
                                try(detach("package:pcaMethods",unload=TRUE),silent=TRUE)
                                
                                data_m<-t(data_m)
                                
                            }
                            
                            
                            
                        }
                    }
                }
                
                
            }
        }
    }else
    {
        data_m<-data_matrix[,-c(1:2)]
        
        if(is.na(missing.val)==FALSE){
            
            print("Replacing missing values with NAs.")
            data_m<-replace(as.matrix(data_m),which(data_m==missing.val),NA)
        }
        
        if(summary.na.replacement=="zeros"){
            data_m<-replace(data_m,which(is.na(data_m)==TRUE),0)
        }else{
            if(summary.na.replacement=="halfsamplemin"){
                data_m<-apply(data_m,2,function(x){naind<-which(is.na(x)==TRUE); if(length(naind)>0){x[naind]<-min(x,na.rm=TRUE)/2}; return(x)})
            }else{
                
                if(summary.na.replacement=="halfdatamin"){
                    
                    
                    min_val<-min(data_m,na.rm=TRUE)*0.5
                    data_m<-replace(data_m,which(is.na(data_m)==TRUE),min_val)
                    
                    #data_m<-apply(data_m,1,function(x){naind<-which(is.na(x)==TRUE); if(length(naind)>0){x[naind]<-min(x,na.rm=TRUE)/2}; return(x)})
                }else{
                    if(summary.na.replacement=="halffeaturemin"){
                        data_m<-apply(data_m,1,function(x){naind<-which(is.na(x)==TRUE); if(length(naind)>0){x[naind]<-min(x,na.rm=TRUE)/2}; return(x)})
                        data_m<-t(data_m)
                    }else{
                        
                        if(summary.na.replacement=="bpca"){
                            
                            pc1 <- pca(t(data_m), method="bpca", nPcs=2)
                            
                            data_m<-completeObs(pc1)
                            data_m<-t(data_m)
                        }
                        
                    }
                }
            }
            
            
        }
        
    }
    
    
    
    #group-wise missing values
    if(length(clean_metabs)>0)
    {
        data_m<-data_m[clean_metabs,]
        data_matrix<-data_matrix[clean_metabs,]
        
        print(paste("Dimension of data matrix after using group-wise ",100*group.missing.thresh, "% signal criteria for filtering:"),sep="")
        print(dim(data_matrix))
        
    }
    
    
    
    #    data_matrix<-cbind(data_matrix[,c(1:2)],data_m)
    #write.table(data_matrix,file="pretransformation.txt",sep="\t",row.names=FALSE)
    ####################################################################
    #Step 4) Data transformation and normalization
    
    if(log2transform==TRUE)
    {
        data_m<-log2(data_m+1)
        
        # print("log scale")
        #print(head(data_m))
    }
    
    if(quantile_norm==TRUE)
    {
        data_m<-normalizeQuantiles(data_m)
        #print("quant norm")
        #print(head(data_m))
    }
    
    if(lowess_norm==TRUE)
    {
        data_m<-normalizeCyclicLoess(data_m)
        #print("lowess")
    }
    
    data_m_prescaling<-data_m
    
    if(medcenter==TRUE)
    {
        colmedians=apply(data_m,1,function(x){median(x,na.rm=TRUE)})
        data_m=sweep(data_m,1,colmedians)
        
        
    }
    if(znormtransform==TRUE)
    {
        data_m<-scale(t(data_m))
        data_m<-t(data_m)
    }
    
    
    if(madscaling==TRUE)
    {
        colmedians=apply(data_m,2,function(x){median(x,na.rm=TRUE)})
        
        Y=sweep(data_m,2,colmedians)
        mad<-apply(abs(Y),2,function(x){median(x,na.rm=TRUE)})
        const<-prod(mad)^(1/length(mad))
        scale.normalized<-sweep(data_m,2,const/mad,"*")
        data_m<-scale.normalized
    }
    
    
    #print("after")
    #print(data_m[1:10,1:10])
    #Use this if first column is gene/metabolite name
    #for apLCMS:
    #data_matrix_temp<-data_matrix[,c(1:2)]
    data_matrix<-cbind(data_matrix[,c(1:2)],data_m)
    
    #print(dim(data_matrix))
    #print(dim(data_m))
    
    data_m<-as.data.frame(data_m)
    
    num_rows<-dim(data_m)[1]
    num_columns<-dim(data_m)[2]
    
    #print("num rows is ")
    #print(num_rows)
    #for apLCMS:
    rnames<-paste("mzid_",seq(1,num_rows),sep="")
    rownames(data_m)=rnames
    
    mzid_mzrt<-data_matrix[,c(1:2)]
    colnames(mzid_mzrt)<-c("mz","time")
    rownames(mzid_mzrt)=rnames
    write.table(mzid_mzrt, file="mzid_mzrt.txt",sep="\t",row.names=FALSE)
    
    
    
    filename<-paste("ordered_classlabels_file.txt",sep="")
    write.table(classlabels, file=filename,sep="\t",row.names=FALSE)
    
    filename<-paste("Normalized_sigthreshfilt_averaged_data.txt",sep="")
    data_matrix<-cbind(data_matrix[,c(1:2)],data_m)
    write.table(data_matrix, file=filename,sep="\t",row.names=FALSE)
    data_matrix_prescaling<-cbind(data_matrix[,c(1:2)],data_m_prescaling)
    return(list(data_matrix_afternorm_scaling=data_matrix,data_matrix_prescaling=data_matrix_prescaling,classlabels=classlabels))
    #return(data_matrix)
}
=======
data_preprocess <-
function(Xmat=NA,Ymat=NA,feature_table_file,parentoutput_dir,class_labels_file,num_replicates=3,feat.filt.thresh=NA,summarize.replicates=TRUE,summary.method="mean",
all.missing.thresh=0.5,group.missing.thresh=0.7,
log2transform=TRUE,medcenter=TRUE,znormtransform=FALSE,quantile_norm=TRUE,lowess_norm=FALSE,madscaling=FALSE,missing.val=0,samplermindex=NA, rep.max.missing.thresh=0.5,summary.na.replacement="zeros",featselmethod=NA){
    
    options(warn=-1)
    
    #read file; First row is column headers
    if(is.na(Xmat==TRUE)){
        data_matrix<-read.table(feature_table_file,sep="\t",header=TRUE)
    }else{
        data_matrix<-Xmat
        #rm(Xmat)
    }
    
    #print("signal filter threshold ")
    #print(group.missing.thresh)
    
    print("missing val is")
    print(missing.val)
    
    if(is.na(all.missing.thresh)==TRUE){
        
        all.missing.thresh=0
    }
    
    if(is.na(samplermindex)==FALSE){
        data_matrix<-data_matrix[,-c(samplermindex)]
    }
    
    #use only unique records
    data_matrix<-unique(data_matrix)
    
    if(is.na(missing.val)==FALSE){
        
        print("Replacing missing values with NAs.")
        data_matrix<-replace(as.matrix(data_matrix),which(data_matrix==missing.val),NA)
    }
    
    # print(data_matrix[1:10,1:5])
    
    #print("dim of original data matrix")
    #print(dim(data_matrix))
    data_matrix_orig<-data_matrix
    
    
    snames<-colnames(data_matrix)
    
    
    
    
    
    suppressWarnings(dir.create(parentoutput_dir,showWarnings=FALSE))
    parentoutput_dir<-paste(parentoutput_dir,"/Stage1/",sep="")
    
    suppressWarnings(dir.create(parentoutput_dir,showWarnings=FALSE))
    fheader="transformed_log2fc_threshold_"
    setwd(parentoutput_dir)
    
    data_m<-as.matrix(data_matrix[,-c(1:2)])
    
    if(is.na(Xmat)==FALSE){
        
        #   write.table(Xmat,file="organized_featuretableA.txt",sep="\t",row.names=TRUE)
        
        
    }
    
    if(is.na(Ymat)==FALSE){
        # write.table(Ymat,file="organized_classlabelsA.txt",sep="\t",row.names=FALSE)
        
    }
    
    #Step 2) Average replicates
    if(summarize.replicates==TRUE)
    {
        if(num_replicates>1)
        {
            
            data_m<-getSumreplicates(data_matrix,alignment.tool="apLCMS",numreplicates=num_replicates,numcluster=10,rep.max.missing.thresh=rep.max.missing.thresh,summary.method=summary.method,summary.na.replacement, missing.val=missing.val)
            
            
            data_m<-replace(data_m,which(is.na(data_m)==TRUE),missing.val)
            
            if(summary.method=="mean"){
                print("Replicate averaging done")
                filename<-paste("Rawdata_averaged.txt",sep="")
            }else{
                if(summary.method=="median"){
                    print("Replicate median summarization done")
                    filename<-paste("Rawdata_median_summarized.txt",sep="")
                }
                
            }
            
            data_m_prenorm<-cbind(data_matrix[,c(1:2)],data_m)
            
            write.table(data_m_prenorm, file=filename,sep="\t",row.names=FALSE)
            
            data_matrix<-cbind(data_matrix[,c(1:2)],data_m)
            #num_samps_group[[1]]=(1/num_replicates)*num_samps_group[[1]]
            #num_samps_group[[2]]=(1/num_replicates)*num_samps_group[[2]]
        }
    }
    
    
    data_matrix<-cbind(data_matrix[,c(1:2)],data_m)
    
    data_matrix_orig<-data_matrix
    data_subjects<-data_m
    
    ordered_labels={}
    
    num_samps_group<-new("list")
    
    if(is.na(class_labels_file)==FALSE)
    {
        
        print("Class labels file:")
        print(class_labels_file)
        
        data_matrix={}
        
        if(is.na(Ymat)==TRUE){
            classlabels<-read.table(class_labels_file,sep="\t",header=TRUE)
        }else{
            classlabels<-Ymat
        }
        
        #class_labels_sampnames<-classlabels[,1]
        #data_matrix_sampnames<-colnames(data_m)
        
        #classlabels<-classlabels[match(class_labels_sampnames,data_matrix_sampnames),]
        
        classlabels<-as.data.frame(classlabels)
        
        cnames1<-colnames(classlabels)
        cnames1[1]<-c("SampleID")
        cnames1[2]<-c("Class")
        
        colnames(classlabels)<-cnames1 #c("SampleID","Class")
        f1<-table(classlabels$SampleID)
        
        
        classlabels<-as.data.frame(classlabels)
        classlabels<-classlabels[seq(1,dim(classlabels)[1],num_replicates),]
        #print(classlabels)
        class_labels_levels<-levels(as.factor(classlabels[,2]))
        bad_rows<-which(class_labels_levels=="")
        if(length(bad_rows)>0){
            class_labels_levels<-class_labels_levels[-bad_rows]
        }
        
        for(c in 1:length(class_labels_levels))
        {
            if(c>1){
                data_matrix<-cbind(data_matrix,data_subjects[,which(classlabels[,2]==class_labels_levels[c])])
            }else{
                data_matrix<-data_subjects[,which(classlabels[,2]==class_labels_levels[c])]
            }
            classlabels_index<-which(classlabels[,2]==class_labels_levels[c])
            ordered_labels<-c(ordered_labels,as.character(classlabels[classlabels_index,2]))
            num_samps_group[[c]]<-length(classlabels_index)
            
        }
        
        #colnames(data_matrix)<-as.character(ordered_labels)
        data_matrix<-cbind(data_matrix_orig[,c(1:2)],data_matrix)
        data_m<-as.matrix(data_matrix[,-c(1:2)])
        
        
    }else
    {
        if(is.na(Ymat)==TRUE)
        {
            classlabels<-rep("A",dim(data_m)[2])
            ordered_labels<-classlabels
            num_samps_group[[1]]<-dim(data_m)[2]
            class_labels_levels<-c("A")
            data_m<-as.matrix(data_matrix[,-c(1:2)])
            
        }else{
            classlabels<-Ymat
            classlabels<-as.data.frame(classlabels)
            cnames1<-colnames(classlabels)
            cnames1[1]<-c("SampleID")
            cnames1[2]<-c("Class")
            
            colnames(classlabels)<-cnames1
            #colnames(classlabels)<-c("SampleID","Class")
            f1<-table(classlabels$SampleID)
            
            
            classlabels<-as.data.frame(classlabels)
            classlabels<-classlabels[seq(1,dim(classlabels)[1],num_replicates),]
            #print(classlabels)
            class_labels_levels<-levels(as.factor(classlabels[,2]))
            bad_rows<-which(class_labels_levels=="")
            if(length(bad_rows)>0){
                class_labels_levels<-class_labels_levels[-bad_rows]
            }
            
            for(c in 1:length(class_labels_levels))
            {
                #if(c>1){
                #data_matrix<-cbind(data_matrix,data_subjects[,which(classlabels[,2]==class_labels_levels[c])])
                #}else{
                #	data_matrix<-data_subjects[,which(classlabels[,2]==class_labels_levels[c])]
                #}
                classlabels_index<-which(classlabels[,2]==class_labels_levels[c])
                ordered_labels<-c(ordered_labels,as.character(classlabels[classlabels_index,2]))
                num_samps_group[[c]]<-length(classlabels_index)
                
            }
            
            #colnames(data_matrix)<-as.character(ordered_labels)
            #data_matrix<-cbind(data_matrix_orig[,c(1:2)],data_matrix)
            #data_m<-as.matrix(data_matrix[,-c(1:2)])
        }
        
        
        
    }
    
    
    rnames_xmat<-colnames(data_matrix[,-c(1:2)])
    rnames_ymat<-as.character(classlabels[,1])
    
    
    for(ind1 in 1:length(rnames_xmat)){
        check_ylabel<-regexpr(rnames_ymat[ind1],pattern="^[0-9]*",perl=TRUE)
        check_xlabel<-regexpr(rnames_xmat[ind1],pattern="^X[0-9]*",perl=TRUE)
        
        if(length(check_ylabel)>0 && length(check_xlabel)>0){
            if(attr(check_ylabel,"match.length")>0 && attr(check_xlabel,"match.length")>0){
                
                rnames_ymat<-paste("X",rnames_ymat,sep="") #gsub(rnames_ymat,pattern="\\.[0-9]*",replacement="")
                
                
            }
        }
        
    }
    
    match_names<-match(rnames_xmat,rnames_ymat)
    
    bad_colnames<-length(which(is.na(match_names)==TRUE))
    
    classlabels<-classlabels[match_names,]
    
    #Step 3a) Remove features if signal is not detected in at least x% of all samples
    ##################################################################################
    metab_zeros={}
    data_clean<-{}
    clean_metabs<-{}
    #num_samps_group[[3]]<-0
    
  
    
    if(is.na(all.missing.thresh)==FALSE)
    {
        
        total_sigs<-apply(data_m,1,function(x){
            if(is.na(missing.val)==FALSE){return(length(which(x>missing.val)))
            }else{
                return(length(which(is.na(x)==FALSE)))
            }})
        
        
        
        total_sig_thresh<-dim(data_m)[2]*all.missing.thresh
        
        total_good_metabs<-which(total_sigs>total_sig_thresh)
        
    }
    
    #remove bad features based on all missing values criteria
    if(length(total_good_metabs)>0){
        data_m<-data_m[total_good_metabs,]
        data_matrix<-data_matrix[total_good_metabs,]
        #print(paste("Dimension of data matrix after overall ",all.missing.thresh,"% signal threshold filtering",sep=""))
        print(paste("Dimension of data matrix after using overall ",100*all.missing.thresh, "% signal criteria for filtering:"),sep="")
        print(dim(data_matrix))
    }else{
        stop(paste("None of the metabolites have signal in ",all.missing.thresh*100, "% of samples",sep=""))
    }
    
    
    #Step 3b) Find features for which the signal is not detected in at least x% of samples in either of the groups
    
    
    data_m<-data_matrix[,-c(1:2)]
    
    if(is.na(group.missing.thresh)==FALSE)
    {
        
        if(length(class_labels_levels)==0)
        {
            
            sig_thresh_groupA<-group.missing.thresh*num_samps_group[[1]]
            sig_thresh_groupB<-group.missing.thresh*num_samps_group[[2]]
            
            for(metab_num in 1:dim(data_matrix)[1])
            {
                #print(missing.val)
                if(is.na(missing.val)==FALSE){
                    
                    num_sigsA<-length(which(data_m[metab_num,1:num_samps_group[[1]]]>missing.val))
                    
                    
                    num_sigsB<-length(which(data_m[metab_num,(num_samps_group[[1]]+1):(num_samps_group[[1]]+num_samps_group[[2]])]>missing.val))
                    
                }else{
                    
                    num_sigsA<-length(which(is.na(data_m[metab_num,1:num_samps_group[[1]]])==FALSE))
                    num_sigsB<-length(which(is.na(data_m[metab_num,(num_samps_group[[1]]+1):(num_samps_group[[1]]+num_samps_group[[2]])])==FALSE))
                    
                }
                
                if((num_sigsA>=sig_thresh_groupA) || (num_sigsB>=sig_thresh_groupB))
                {
                    clean_metabs<-c(clean_metabs,metab_num)
                }
                
            }
            
            #print(length(clean_metabs))
        }else{
            if(length(class_labels_levels)==0){
                
                
                sig_thresh_groupA<-group.missing.thresh*num_samps_group[[1]]
                sig_thresh_groupB<-group.missing.thresh*num_samps_group[[2]]
                sig_thresh_groupC<-group.missing.thresh*num_samps_group[[3]]
                
                for(metab_num in 1:dim(data_matrix)[1])
                {
                    if(is.na(missing.val)==FALSE){
                        num_sigsA<-length(which(data_m[metab_num,1:num_samps_group[[1]]]>missing.val))
                        num_sigsB<-length(which(data_m[metab_num,(num_samps_group[[1]]+1):(num_samps_group[[1]]+num_samps_group[[2]])]>missing.val))
                        num_sigsC<-length(which(data_m[metab_num,(num_samps_group[[1]]+num_samps_group[[2]]+1):(num_samps_group[[1]]+num_samps_group[[2]]+num_samps_group[[3]])]>missing.val))
                    }else{
                        
                        num_sigsA<-length(which(is.na(data_m[metab_num,1:num_samps_group[[1]]])==FALSE))
                        num_sigsB<-length(which(is.na(data_m[metab_num,(num_samps_group[[1]]+1):(num_samps_group[[1]]+num_samps_group[[2]])])==FALSE))
                        num_sigsC<-length(which(is.na(data_m[metab_num,(num_samps_group[[1]]+num_samps_group[[2]]+1):(num_samps_group[[1]]+num_samps_group[[2]]+num_samps_group[[3]])])==FALSE))
                        
                    }
                    
                    if((num_sigsA>=sig_thresh_groupA) || (num_sigsB>=sig_thresh_groupB) || (num_sigsC>=sig_thresh_groupC))
                    {
                        clean_metabs<-c(clean_metabs,metab_num)
                    }
                    
                }
            }else{
                if(length(class_labels_levels)>1){
                    
                    
                    
                    for(metab_num in 1:dim(data_m)[1])
                    {
                        for(c in 1:length(class_labels_levels)){
                            
                            classlabels_index<-which(classlabels[,2]==class_labels_levels[c])
                            templabels<-classlabels[,2]
                            if(is.na(missing.val)==FALSE){
                                
                                num_cursig<-length(which(data_m[metab_num,which(templabels==class_labels_levels[c])]>missing.val))
                                
                            }else{
                                
                                num_cursig<-length(which(is.na(data_m[metab_num,which(templabels==class_labels_levels[c])])==FALSE))
                                
                                
                            }
                            
                            sig_thresh_cur<-length(which(templabels==class_labels_levels[c]))*group.missing.thresh
                            if(num_cursig>=sig_thresh_cur)
                            {
                                clean_metabs<-c(clean_metabs,metab_num)
                                break   #for(i in 1:4){if(i==3){break}else{print(i)}}
                                
                            }
                            
                        }
                    }
                }
                else{
                    
                    
                    
                    if(length(class_labels_levels)==1){
                        num_samps_group[[1]]<-num_samps_group[[1]]
                        
                        
                        sig_thresh_groupA<-group.missing.thresh*num_samps_group[[1]]
                        
                        
                        for(metab_num in 1:dim(data_matrix)[1])
                        {
                            if(is.na(missing.val)==FALSE){
                                num_sigsA<-length(which(data_m[metab_num,1:num_samps_group[[1]]]>missing.val))
                                
                            }else{
                                
                                num_sigsA<-length(which(is.na(data_m[metab_num,1:num_samps_group[[1]]])==FALSE))
                            }
                            
                            if((num_sigsA>=sig_thresh_groupA) )
                            {
                                clean_metabs<-c(clean_metabs,metab_num)
                            }
                            
                        }
                    }
                }
            }
        }
        
        
        
    }
    ####################################################################################
    
    #Step 4) Replace missing values
    if(summarize.replicates==TRUE)
    {
        
        {
            
            if(is.na(missing.val)==FALSE){
                
                print("Replacing missing values with NAs.")
                data_m<-replace(as.matrix(data_m),which(data_m==missing.val),NA)
            }
            
            
            if(summary.na.replacement=="zeros"){
                data_m<-replace(data_m,which(is.na(data_m)==TRUE),0)
            }else{
                if(summary.na.replacement=="halfsamplemin"){
                    data_m<-apply(data_m,2,function(x){naind<-which(is.na(x)==TRUE); if(length(naind)>0){x[naind]<-min(x,na.rm=TRUE)/2}; return(x)})
                }else{
                    
                    if(summary.na.replacement=="halfdatamin"){
                        
                        
                        min_val<-min(data_m,na.rm=TRUE)*0.5
                        data_m<-replace(data_m,which(is.na(data_m)==TRUE),min_val)
                        
                        #data_m<-apply(data_m,1,function(x){naind<-which(is.na(x)==TRUE); if(length(naind)>0){x[naind]<-min(x,na.rm=TRUE)/2}; return(x)})
                    }else{
                        if(summary.na.replacement=="halffeaturemin"){
                            data_m<-apply(data_m,1,function(x){naind<-which(is.na(x)==TRUE); if(length(naind)>0){x[naind]<-min(x,na.rm=TRUE)/2}; return(x)})
                            data_m<-t(data_m)
                        }else{
                            
                            
                            if(summary.na.replacement=="bpca"){
                                library(pcaMethods)
                                pc1 <- pcaMethods::pca(t(data_m), method="bpca", nPcs=10)
                                
                                data_m<-pcaMethods::completeObs(pc1)
                                
                                try(detach("package:pcaMethods",unload=TRUE),silent=TRUE)
                                
                                data_m<-t(data_m)
                                
                            }
                            
                            
                            
                        }
                    }
                }
                
                
            }
        }
    }else
    {
        data_m<-data_matrix[,-c(1:2)]
        
        if(is.na(missing.val)==FALSE){
            
            print("Replacing missing values with NAs.")
            data_m<-replace(as.matrix(data_m),which(data_m==missing.val),NA)
        }
        
        if(summary.na.replacement=="zeros"){
            data_m<-replace(data_m,which(is.na(data_m)==TRUE),0)
        }else{
            if(summary.na.replacement=="halfsamplemin"){
                data_m<-apply(data_m,2,function(x){naind<-which(is.na(x)==TRUE); if(length(naind)>0){x[naind]<-min(x,na.rm=TRUE)/2}; return(x)})
            }else{
                
                if(summary.na.replacement=="halfdatamin"){
                    
                    
                    min_val<-min(data_m,na.rm=TRUE)*0.5
                    data_m<-replace(data_m,which(is.na(data_m)==TRUE),min_val)
                    
                    #data_m<-apply(data_m,1,function(x){naind<-which(is.na(x)==TRUE); if(length(naind)>0){x[naind]<-min(x,na.rm=TRUE)/2}; return(x)})
                }else{
                    if(summary.na.replacement=="halffeaturemin"){
                        data_m<-apply(data_m,1,function(x){naind<-which(is.na(x)==TRUE); if(length(naind)>0){x[naind]<-min(x,na.rm=TRUE)/2}; return(x)})
                        data_m<-t(data_m)
                    }else{
                        
                        if(summary.na.replacement=="bpca"){
                            
                            pc1 <- pca(t(data_m), method="bpca", nPcs=2)
                            
                            data_m<-completeObs(pc1)
                            data_m<-t(data_m)
                        }
                        
                    }
                }
            }
            
            
        }
        
    }
    
    
    
    #group-wise missing values
    if(length(clean_metabs)>0)
    {
        data_m<-data_m[clean_metabs,]
        data_matrix<-data_matrix[clean_metabs,]
        
        print(paste("Dimension of data matrix after using group-wise ",100*group.missing.thresh, "% signal criteria for filtering:"),sep="")
        print(dim(data_matrix))
        
    }
    
    
    
    #    data_matrix<-cbind(data_matrix[,c(1:2)],data_m)
    #write.table(data_matrix,file="pretransformation.txt",sep="\t",row.names=FALSE)
    ####################################################################
    #Step 4) Data transformation and normalization
    
    if(log2transform==TRUE)
    {
        data_m<-log2(data_m+1)
        
        # print("log scale")
        #print(head(data_m))
    }
    
    if(quantile_norm==TRUE)
    {
        data_m<-normalizeQuantiles(data_m)
        #print("quant norm")
        #print(head(data_m))
    }
    
    if(lowess_norm==TRUE)
    {
        data_m<-normalizeCyclicLoess(data_m)
        #print("lowess")
    }
    
    data_m_prescaling<-data_m
    
    if(medcenter==TRUE)
    {
        colmedians=apply(data_m,1,function(x){median(x,na.rm=TRUE)})
        data_m=sweep(data_m,1,colmedians)
        
        
    }
    if(znormtransform==TRUE)
    {
        data_m<-scale(t(data_m))
        data_m<-t(data_m)
    }
    
    
    if(madscaling==TRUE)
    {
        colmedians=apply(data_m,2,function(x){median(x,na.rm=TRUE)})
        
        Y=sweep(data_m,2,colmedians)
        mad<-apply(abs(Y),2,function(x){median(x,na.rm=TRUE)})
        const<-prod(mad)^(1/length(mad))
        scale.normalized<-sweep(data_m,2,const/mad,"*")
        data_m<-scale.normalized
    }
    
    
    #print("after")
    #print(data_m[1:10,1:10])
    #Use this if first column is gene/metabolite name
    #for apLCMS:
    #data_matrix_temp<-data_matrix[,c(1:2)]
    data_matrix<-cbind(data_matrix[,c(1:2)],data_m)
    
    #print(dim(data_matrix))
    #print(dim(data_m))
    
    data_m<-as.data.frame(data_m)
    
    num_rows<-dim(data_m)[1]
    num_columns<-dim(data_m)[2]
    
    #print("num rows is ")
    #print(num_rows)
    #for apLCMS:
    rnames<-paste("mzid_",seq(1,num_rows),sep="")
    rownames(data_m)=rnames
    
    mzid_mzrt<-data_matrix[,c(1:2)]
    colnames(mzid_mzrt)<-c("mz","time")
    rownames(mzid_mzrt)=rnames
    write.table(mzid_mzrt, file="mzid_mzrt.txt",sep="\t",row.names=FALSE)
    
    
    
    filename<-paste("ordered_classlabels_file.txt",sep="")
    write.table(classlabels, file=filename,sep="\t",row.names=FALSE)
    
    filename<-paste("Normalized_sigthreshfilt_averaged_data.txt",sep="")
    data_matrix<-cbind(data_matrix[,c(1:2)],data_m)
    write.table(data_matrix, file=filename,sep="\t",row.names=FALSE)
    data_matrix_prescaling<-cbind(data_matrix[,c(1:2)],data_m_prescaling)
    return(list(data_matrix_afternorm_scaling=data_matrix,data_matrix_prescaling=data_matrix_prescaling,classlabels=classlabels))
    #return(data_matrix)
}
>>>>>>> d85ff5fd429b8ce2c4d44411a09b32765ce92b65
