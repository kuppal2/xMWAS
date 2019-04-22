do_rsd <-
function(x){
	
	sd_val<-sd(x,na.rm=TRUE)
	mean_val<-mean(x,na.rm=TRUE)
	cv_val<-100*(sd_val/(mean_val))
	return(cv_val)
}
