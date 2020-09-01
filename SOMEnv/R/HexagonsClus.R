#' SOM map with clusters
#'
#' Generate the sequence of colors to plot the SOM map according clusters 
#'
#' @param Centroids Centroids matrix
#' @param Cluster Vector containing cluster number assignment for prototypes
#' @param BCentr Best Matching Unit of the cluster centroids
#' @param Coord Prototype coordinates for plotting the map
#' @param Row Number of SOM map rows
#' @param Col Number of SOM map columns
#' @param colSeq Color sequence for the clusters 
#' @author S. Licen
#' @return A SOM map with clusters 

HexagonsClus<-function(Centroids,Cluster,BCentr,Coord,Row,Col,colSeq=rainbow(nrow(Centroids))) 
{  Vector<-Cluster;
   Colors<-ClusCol(Centroids,Vector,colSeq=colSeq);
   Hexagons(Coord,Row, Col,col = NA, border = NA) 
   for (i in c(1:nrow(Coord))) {Hexa(Coord$X[i],Coord$Y[i],col=Colors[i],border="gray")}
   text(Coord[BCentr,"X"],Coord[BCentr,"Y"],c(1:nrow(Centroids)),cex=1,font=2,col="black")
}

