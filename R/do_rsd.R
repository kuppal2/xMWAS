<<<<<<< HEAD
do_rsd <-
function(x){
	
	sd_val<-sd(x,na.rm=TRUE)
	mean_val<-mean(x,na.rm=TRUE)
	cv_val<-100*(sd_val/(mean_val))
	return(cv_val)
}
=======
do_rsd <-
function(x){
	
	sd_val<-sd(x,na.rm=TRUE)
	mean_val<-mean(x,na.rm=TRUE)
	cv_val<-100*(sd_val/(mean_val))
	return(cv_val)
}
>>>>>>> d85ff5fd429b8ce2c4d44411a09b32765ce92b65
