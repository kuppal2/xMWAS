Run <-
function(file1,file2,lambda,eps,DBCfile){
    
    adjMtrxSample1 <-read.table(file1,header=FALSE, sep=",");
    adjMtrxSample2 <-read.table(file2,header=FALSE, sep=",");
    
    DBC <-read.table(DBCfile,header=FALSE, sep=",");
    DBC = t(DBC);
    diffRank_orig(adjMtrxSample1, adjMtrxSample2, lambda, eps, DBC);
    
}
