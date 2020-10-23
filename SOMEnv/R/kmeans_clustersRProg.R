#' K-means algorithm applied for different values of clusters
#'
#' The som_kmeansR function with 100 epochs training is run for a custom number of times for each k value of clusters
#' and the best of these is selected based on sum of squared errors (err).
#' The Davies-Bouldin index is calculated for each k-clustering.
#' The function has been coded in R code starting from kmeans_clusters.m
#' script present in somtoolbox for Matlab by Vesanto and adapted to show a progress bar when working embedded in the shiny app.
#'
#' @param codebook SOM codebook
#' @param k Maximum number of clusters (the function will be run from 2 to k clusters)
#' @param times Number of times the som_kmeansR function is iterated
#' @param seed Number for set.seed function
#' @author Sabina Licen, Pierluigi Barbieri
#' @return This function returns a list containing the cluster number assignment for each sample,
#' the cluster centroids, the total quantization error, the DB-index for each number of clusters, and the random seed number used
#' @importFrom shiny withProgress incProgress
#' @seealso som_mdistR, som_kmeansRProg, db_indexR
#' @references {J. Vesanto, J. Himberg, E. Alhoniemi, J. Parhankagas, SOM Toolbox for Matlab
#' 5, Report A57, 2000, Available at: www.cis.hut.fi/projects/somtoolbox/package/papers/techrep.pdf
#' }
#' @export


kmeans_clustersRProg<-function(codebook,k=5,times=5,seed=NULL) {

if (is.null(seed)) {times<-times
} else {times<-1}

codebook<-as.matrix(codebook)
n_max<-k
c_max<-times
Machprest<-.Machine
CENTROIDS<-vector(mode = "list", length = n_max)
CLUSNum<-rep(0,nrow(codebook))
ERR<-NULL
IND<-NULL
SEED<-NULL

    withProgress(message = paste0('Total training times = ',times), value = 0, {
      # Number of times we'll go through the loop

for (j in c(2:n_max)) {
                    best<-Machprest$double.xmax
                       for (i in c(1:c_max)) {KM<-som_kmeansRProg(codebook,j,100,seed=seed);
                                if (KM$err<best) {k_best<-KM$clusters; c_best<-KM$centroids; best<-KM$err; Seed<-KM$seed}
        incProgress(1/times, detail = paste("Time:", i))
                       }

CENTROIDS[[j]]<-c_best
CLUSNum<-cbind(CLUSNum,k_best)
ERR<-c(ERR,best)
ind<-db_indexR(codebook,k_best,c_best)
IND<-c(IND,ind)
SEED<-c(SEED,Seed)

}
    })
result<-list("centroids" = CENTROIDS, "clusNum" = CLUSNum, "err" = ERR, "ind"=IND, "seed"=SEED)
return(result)

}# END function



