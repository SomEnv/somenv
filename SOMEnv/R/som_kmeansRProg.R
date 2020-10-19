#' K-means algorithm applied for a specific number of clusters
#'
#' The training is run for a custom number of epochs for k number of clusters
#'
#' The function has been coded in R code starting from som_kmeans.m
#' script present in somtoolbox for Matlab by Vesanto and adapted to show a progress bar when working embedded in the shiny app.
#'
#' @param codebook SOM codebook
#' @param k Number of clusters
#' @param epochs Number of training epochs
#' @param seed Number for set.seed function
#' @author Sabina Licen, Pierluigi Barbieri
#' @return This function returns a list containing the cluster number assignment for each sample,
#' the cluster centroids, the total quantization error, and the random seed number used
#' @seealso set.seed
#' @references {J. Vesanto, J. Himberg, E. Alhoniemi, J. Parhankagas, SOM Toolbox for Matlab
#' 5, Report A57, 2000, Available at: www.cis.hut.fi/projects/somtoolbox/package/papers/techrep.pdf
#' }

som_kmeansRProg<-function(codebook,k,epochs,seed=NULL) {

Data<-as.matrix(codebook)
munits<-nrow(Data)
dim<-ncol(Data)

m1<-c(1:munits)

if (is.null(seed)) {
Set<-sum(c(as.numeric(format(Sys.time(), "%Y")),as.numeric(format(Sys.time(), "%m")),as.numeric(format(Sys.time(), "%d")),
as.numeric(format(Sys.time(), "%H")),as.numeric(format(Sys.time(), "%M")),as.numeric(format(Sys.time(), "%OS1"))),na.rm=T)
} else {Set<-seed}

set.seed(Set)
temp<-sample(m1)

centroids<-Data[temp[1:k],]
clusters<-rep(0,munits)

#---- batch mode:

iter<-0
old_clusters<-rep(0,k)

    withProgress(message = paste0('Training cluster n. = ',k), value = 0, {
      # Number of times we'll go through the loop

while (iter<epochs) {

val<-t(replicate(k,colSums(t(Data^2)))+ t(replicate(munits,colSums(t(centroids^2))))-2*(Data%*%t(centroids)))
dummy<-apply(val,2,min) # valori minimi
clusters<-apply(val,2,which.min) # posizione dei valori minimi

for (i in c(1:k)) {
f<-which(clusters==i)
s<-length(f)
if (s>0) { if (s>1) {centroids[i,]<-(apply(Data[f,],2,sum)/s)} else {centroids[i,]<-Data[f,]/s} } 
}

if (iter>0) {
if (sum(old_clusters==clusters)==0) {
print(paste("Convergence in ",iter," iterations",sep=""));
break
}
}

        # Increment the progress bar, and update the detail text.
        incProgress(1/epochs, detail = paste("Epoch:", iter))

old_clusters<-clusters
iter<-iter+1


}#end while
    })

val<-t(replicate(k,colSums(t(Data^2)))+ t(replicate(munits,colSums(t(centroids^2))))-2*(Data%*%t(centroids)))
dummy<-apply(val,2,min) # valori minimi
clusters<-apply(val,2,which.min) # posizione dei valori minimi

err<-0

for (i in c(1:k)) {
f<-which(clusters==i)
s<-length(f)
if (s>0) { err<-err + sum(rowSums((Data[f,]-t(replicate(s,centroids[i,])))^2)) }
}

result<-list("centroids" = centroids, "clusters" = clusters, "err" = err, "seed" = Set)

return(result)

}#END function



