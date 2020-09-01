#' Realtive quantization error distribution on the SOM map
#'
#' Plot a SOM map with realtive quantization error plotted as grayscale according to quartiles
#'
#' The function evaluate the relative quantization error for each prototype
#' dividing the sum of quantization errors for experimental samples represented by the single
#' prototype by the number of hits of the same prototype,
#' then plots a SOM map with with filled hexagons according to the realtive quantization error
#'
#' @param bmus Vector with Best Matching Unit for each experimental sample
#' @param qerrs Vector with quantization error for each experimental sample
#' @param Coord Prototype coordinates for plotting the map
#' @param Row Number of SOM map rows
#' @param Col Number of SOM map columns
#' @author S. Licen
#' @return Plot a SOM map with filled hexagons according to the realtive quantization error   


HexaQerrs<-function(bmus,qerrs,Coord,Row,Col,col="black") 
{ ORIGINALS<-data.frame(Bmus=bmus,Qerrs=qerrs)
  QSUM<-NULL
  for(i in c(1:nrow(Coord))) 
  {s<-sum(ORIGINALS[which(ORIGINALS$Bmus==i),"Qerrs"])/nrow(ORIGINALS[which(ORIGINALS$Bmus==i),]);
  QSUM<-c(QSUM,s)}
  Unitcell<-1
   Hits<-unlist(QSUM, recursive = TRUE, use.names = FALSE);
   HitsNorm<-round(Hits/max(Hits,na.rm=T)*100,digit=0);
   Hexagons(Coord,Row,Col,col = NA, border = "gray");
   for (i in c(1:nrow(Coord))) {Hexa(Coord$X[i],Coord$Y[i],unitcell=Unitcell*HitsNorm[i]/100,col=col,border=NA)}
#  HexaHits(QSUM,Coord,Row,Col,col="black") 
}


