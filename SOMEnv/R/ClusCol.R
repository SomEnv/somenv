#' Custom color sequence for clusters
#'
#' Generate the sequence of colors to plot the SOM map according to clusters
#'
#' @param Centroids Centroids matrix
#' @param Cluster Vector containing cluster number assignment for prototypes
#' @param colSeq Color sequence for the clusters
#' @author Sabina Licen
#' @return A vector of colors with length equal to Cluster
#' @importFrom grDevices rainbow
#' @references {Licen, S., Cozzutto, S., Barbieri, P. (2020) Aerosol Air Qual. Res., 20 (4), pp. 800-809. DOI: 10.4209/aaqr.2019.08.0414
#' }

ClusCol<-function(Centroids,Cluster, colSeq=rainbow(nrow(data.frame(Centroids))))
{ nClus<-nrow(data.frame(Centroids));
Vector<-unlist(Cluster, recursive = TRUE, use.names = FALSE);
      ab<-as.factor(as.numeric(Vector));
      ac<-factor(ab,levels=levels(ab),colSeq);
      return(as.character(ac))
}
