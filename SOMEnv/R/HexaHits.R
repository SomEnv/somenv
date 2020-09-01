#' Hits distribution on the SOM map
#'
#' Plot a SOM map with filled hexagons according to the number of hits
#'
#' @param hits Vector with number of hits for each prototype 
#' @param Coord Prototype coordinates for plotting the map
#' @param Row Number of SOM map rows
#' @param Col Number of SOM map columns
#' @param col color filling of the hexagons
#' @author S. Licen
#' @return Plot a SOM map with filled hexagons according to the number of hits   


####################################################################
# FUNZIONE per disegnare grafico con esagoni grandi in scala secondo num. di Hits:

HexaHits<-function(hits,Coord,Row,Col,col="black") 
{  Unitcell<-1
   Hits<-unlist(hits, recursive = TRUE, use.names = FALSE);
   HitsNorm<-round(Hits/max(Hits,na.rm=T)*100,digit=0);
   Hexagons(Coord,Row,Col,col = NA, border = "gray");
   for (i in c(1:nrow(Coord))) {Hexa(Coord$X[i],Coord$Y[i],unitcell=Unitcell*HitsNorm[i]/100,col=col,border=NA)}
}


