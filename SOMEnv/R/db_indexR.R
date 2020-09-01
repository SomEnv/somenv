#' Evaluate Davis-Bouldin index
#'
#' The function has been coded in R code starting from db_index.m
#' script present in somtoolbox for Matlab by Vesanto 
#'
#' @param codebook SOM codebook
#' @param k_best Vector with cluster number assignment for each sample
#' @param c_best Matrix with cluster centroids
#' @author S. Licen
#' @return The mean DB-index for the clustering

db_indexR<-function(codebook,k_best,c_best){
D<-as.matrix(codebook)
l<-nrow(codebook)
dim<-ncol(codebook)
cl<-k_best
u<-sort(unique(as.vector(cl)),decreasing=F)
count<-length(u)

C<-c_best

#Action
#Dispersion of each cluster

S<-rep(NA,count)
for(i in c(1:count)) {
indx<-which(cl==u[i]) # points in the cluster
lin<-length(indx)
if (lin>0) {S[i]<-mean(
((rowSums((D[indx,]-t(replicate(lin,C[i,])))^2))^0.5)^2)^(1/2)
} else {S[i]<-NA}
}

# Distance matrix
M<-som_mdistR(C)

# Davies-Bouldin index

R<-matrix(rep(NA,count^2),count,count)
r<-rep(NA,count)
suppressWarnings(
for (i in c(1:count)) {
     for (j in c((i+1):count)) { 
if((i+1)<=count) {R[i,j]<-((S[i]+S[j])/M[i,j])} else {}}
     r[i]<-max(R[i,],na.rm=T)}# r max DB-index per ogni cluster
)#END suppressWarnings
t<-mean(r*as.numeric(is.finite(r)),na.rm=T)  

return(t)
}









