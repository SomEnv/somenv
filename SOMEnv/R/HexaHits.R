#' Hits distribution on the SOM map
#'
#' Plot a SOM map with filled hexagons according to the number of hits
#'
#' @param hits Vector with number of hits for each prototype
#' @param Coord Prototype coordinates for plotting the map
#' @param Row Number of SOM map rows
#' @param Col Number of SOM map columns
#' @param color color filling of the hexagons
#' @author Sabina Licen
#' @return Plot a SOM map with filled hexagons according to the number of hits
#' @references {Licen, S., Cozzutto, S., Barbieri, P. (2020) Aerosol Air Qual. Res., 20 (4), pp. 800-809. DOI: 10.4209/aaqr.2019.08.0414
#' }

HexaHits<-function(hits,Coord,Row,Col,color="black")
{  Unitcell<-1
   Hits<-unlist(hits, recursive = TRUE, use.names = FALSE);
   HitsNorm<-round(Hits/max(Hits,na.rm=T)*100,digits=0);
   Hexagons(Coord,Row,Col,color = NA, border = "gray");
   for (i in c(1:nrow(Coord))) {Hexa(Coord$X[i],Coord$Y[i],unitcell=Unitcell*HitsNorm[i]/100,color=color,border=NA)}
}


