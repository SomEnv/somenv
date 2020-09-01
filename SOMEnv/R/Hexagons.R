#' Function to draw an hexagonal SOM map
#'
#' Draw an hexagon around a point of x and y coordinates
#'
#' @param x X-coordinates of the hexagon centers
#' @param y Y-coordinates of the hexagon centers
#' @param Row Number of SOM map rows
#' @param Col Number of SOM map columns
#' @param color Filling color of the hexagons (default NA)
#' @param border Border color of the hexagons (default "gray")
#' @param unitcell The distance side to side between two parallel sides of the hexagon (default 1)
#' @author S. Licen
#' @return A hexagonal SOM map
#' @export
#' @examples
#' Coord<-CodeCoord(10,5)
#' Hexagons(Coord$XC,Coord$YC,10,5)

Hexagons<-function (Coords,Row,Col,color = NA, border = "gray",unitcell=1) 
{  Apo<-unitcell/2;R<-Apo/(cos(pi/6));
   par(mar=c(1,1,1,2),pty="m",xpd=TRUE,family="serif");
   plot(c(1-2*Apo,Col+Apo),c(1-R,1+(Row-1)*(R*1.5)+R),
   type="n",xlab="",ylab="",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n");
   for (i in c(1:nrow(Coords))) {Hexa(Coords$X[i],Coords$Y[i],col=color,border=border)}
}



