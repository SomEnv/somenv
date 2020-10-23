#' BMUs of the cluster centroids
#'
#' The function finds the Best Matching Units of the cluster centroids
#'
#' @param centroids Centroids array (output of kmeans_clustersR function)
#' @param som_model An object of class kohonen
#' @param k Number of clusters
#'
#' @author Sabina Licen
#' @return An array containing the BMU for each centroid
#' @importFrom kohonen map
#' @references {Licen, S., Cozzutto, S., Barbieri, P. (2020) Aerosol Air Qual. Res., 20 (4), pp. 800-809. DOI: 10.4209/aaqr.2019.08.0414
#' }
#' @export

BmusCentr<-function (centroids,som_model,k) {
                                    A<-vector(mode = "list", length = k)
                                    for (i in c(2:k)) {
                                    Centr<-map(som_model, as.matrix(centroids[[i]]))
                                    A[[i]]<-Centr$unit.classif}
                                    return(A)
                                    }




