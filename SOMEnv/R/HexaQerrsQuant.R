#' Realtive quantization error distribution on the SOM map
#'
#' Plot a SOM map with realtive quantization error plotted as grayscale according to quartiles
#'
#' The function evaluate the relative quantization error for each prototype
#' dividing the sum of quantization errors for experimental sample represented by the single
#' prototype by the number of hits of the same prototype,
#' then plots a SOM map with the realtive quantization error represented as grayscale according to quartiles,
#' from white (lower outliers) followed by grayscale (quartiles) and black (upper outiliers).
#' The outilers and quartiles are evaluated by boxplot function applying default parameters.
#'
#' @param bmus Vector with Best Matching Unit for each experimental sample
#' @param qerrs Vector with quantization error for each experimental sample
#' @param Coord Prototype coordinates for plotting the map
#' @param Row Number of SOM map rows
#' @param Col Number of SOM map columns
#' @author S. Licen
#' @return Plot a SOM map with realtive quantization error represented as grayscale according to quartiles
#' @seealso boxplot
#' @references {Licen, S., Cozzutto, S., Barbieri, P. (2020) Aerosol Air Qual. Res., 20 (4), pp. 800-809. DOI: 10.4209/aaqr.2019.08.0414
#' }
#' @importFrom graphics boxplot
#' @export


HexaQerrsQuant<-function(bmus,qerrs,Coord,Row,Col)
{ ORIGINALS<-data.frame(Bmus=bmus,Qerrs=qerrs)
  colSeq <- c("gray95","gray85","gray75","gray60","gray50","black");
  QSUM<-NULL
  for(i in c(1:nrow(Coord)))
  {s<-sum(ORIGINALS[which(ORIGINALS$Bmus==i),"Qerrs"])/nrow(ORIGINALS[which(ORIGINALS$Bmus==i),]);
  QSUM<-c(QSUM,s)}
  Hits<-unlist(QSUM, recursive = TRUE, use.names = FALSE);
  BOX<-boxplot(Hits,plot=FALSE);
  MIN<-0
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
Hexagons(Coord,Row,Col,color = NA, border = NA);
for (i in c(1:nrow(Coord))) {Hexa(Coord$X[i],Coord$Y[i],color=as.character(FACT[i]),border="gray85")};
}

