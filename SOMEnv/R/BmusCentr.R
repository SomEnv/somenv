#' BMUs of the cluster centroids
#'
#' The function finds the Best Matching Units of the cluster centroids
#'
#' @param centroids Centroids array (output of kmeans_clustersR function)
#' @param som_model An object of class kohonen
#' @param k Number of clusters
#' @export
#' @author S. Licen
#' @return An array containing the BMUs for each centroid


BmusCentr<-function (centroids,som_model,k) {
                                    A<-vector(mode = "list", length = k)
                                    for (i in c(2:k)) {
                                    Centr<-map(som_model, as.matrix(centroids[[i]]))
                                    A[[i]]<-Centr$unit.classif}
                                    return(A)
                                    }




