#' Hits distribution on the SOM map
#'
#' Plot a SOM map with hits plotted as grayscale according to quartiles
#'
#' The function plots a SOM map with hits represented as grayscale according to quartiles,
#' from white (lower outliers) followed by grayscale (quartiles) and black (upper outiliers).
#' The prototype with the maximum number of hits is represented by a red hexagon.
#' The outilers and quartiles are evaluated by boxplot function applying default parameters.
#'
#' @param hits Vector with number of hits for each prototype
#' @param Coord Prototype coordinates for plotting the map
#' @param Row Number of SOM map rows
#' @param Col Number of SOM map columns
#' @author Sabina Licen
#' @return Plot a SOM map with hits represented as grayscale according to quartiles
#' @seealso boxplot
#' @references {Licen, S., Cozzutto, S., Barbieri, P. (2020) Aerosol Air Qual. Res., 20 (4), pp. 800-809. DOI: 10.4209/aaqr.2019.08.0414
#' }
#' @importFrom graphics boxplot

HexaHitsQuant<-function(hits,Coord,Row,Col)
{ Hexagons(Coord,Row,Col,col = NA, border = NA);
  colSeq <- c("white","gray85","gray75","gray60","gray50","black");
  Hits<-unlist(hits, recursive = TRUE, use.names = FALSE);
  BOX<-boxplot(Hits,plot=FALSE);
  MIN<-(-1);
  MAX<-max(Hits,na.rm=TRUE);
  MINCol<-colSeq[1];
  INTRACol<-colSeq[2:5];
  MAXCol<-colSeq[6];
     if (max(Hits,na.rm=TRUE)==BOX$stats[5]){MAX<-NULL; MAXCol<-NULL} else {};
  Breaks<-c(MIN,as.numeric(BOX$stats),MAX);
  Labels<-c(MINCol,INTRACol,MAXCol);
DIV<-.bincode(Hits, breaks=Breaks,include.lowest = FALSE, right = TRUE);
      ab<-as.factor(as.numeric(DIV));
     FACT<-factor(ab,levels=levels(ab),Labels[as.numeric(levels(ab))]);
     FACT<-as.character(FACT)
for (i in c(1:nrow(Coord))) {Hexa(Coord$X[i],Coord$Y[i],col=as.character(FACT[i]),border="gray85")};
}

