#' Custom color sequence for clusters
#'
#' Generate the sequence of colors to plot the SOM map according clusters 
#'
#' @param Centroids Centroids matrix
#' @param Cluster Vector containing cluster number assignment for prototypes
#' @param colSeq Color sequence for the clusters 
#' @author S. Licen
#' @return A vector of colors with length equal to Cluster 


ClusCol<-function(Centroids,Cluster, colSeq=rainbow(nrow(data.frame(Centroids)))) 
{ nClus<-nrow(data.frame(Centroids));
Vector<-unlist(Cluster, recursive = TRUE, use.names = FALSE);
      ab<-as.factor(as.numeric(Vector));
      ac<-factor(ab,levels=levels(ab),colSeq);
      return(as.character(ac))
}