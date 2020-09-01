#' Heatmaps
#'
#' Multiple plots that show the distribution of the modeled variables on the map
#'
#' The function plots a SOM map for the values of each modeled variable using a grayscale according to quartiles,
#' from white (lower outliers), followed by grayscale (quartiles) and black (upper outiliers).
#' The outilers and quartiles are evaluated by boxplot function applying default parameters.
#'
#' @param Dms A vector of length 2, where the first argument specifies the number of rows and the second the number of columns of plots (see mfrow in par)
#' @param codebook SOM codebook
#' @param Coord Prototype coordinates for plotting the map
#' @param Row Number of SOM map rows
#' @param Col Number of SOM map columns
#' @author S. Licen
#' @return Plot a SOM map for the values of each modeled variable using a grayscale according to quartiles   


HexagonsVar<-function (Dms,codebook,Coords,Row,Col) 
{ ColIndex<-c(1:ncol(codebook))
  if ((Dms[1]*Dms[2])>=length(ColIndex)) {ColIndex<-ColIndex
  } else {ColIndex<-c(1:(Dms[1]*Dms[2]))}
  colSeq <- c("white","gray85","gray75","gray60","gray50","black");
  par(mfrow=Dms,oma=c(0,0.2,0.5,0.2),xpd=FALSE,pty="m",family="serif")
for (j in ColIndex) {
  Var<-j
  BOX<-boxplot(codebook[,Var],plot=FALSE);
  MIN<-min(codebook[,Var]);
  MAX<-max(codebook[,Var]);
  MINCol<-colSeq[1];
  INTRACol<-colSeq[2:5];
  MAXCol<-colSeq[6];
     if (min(codebook[,Var])==BOX$stats[1]){MIN<-NULL; MINCol<-NULL} else {};
     if (max(codebook[,Var])==BOX$stats[5]){MAX<-NULL; MAXCol<-NULL} else {};
  Breaks<-c(MIN,as.numeric(BOX$stats),MAX);
  Labels<-c(MINCol,INTRACol,MAXCol);
  FACT<-cut(codebook[,Var], breaks=Breaks,labels = Labels,include.lowest = TRUE, right = FALSE);
Hexagons(Coords,Row,Col,col = NA, border = NA);
   Colors<-as.character(FACT);
   for (i in c(1:nrow(Coords))) {Hexa(Coords$X[i],Coords$Y[i],col=Colors[i],border=NA)}
   mtext(colnames(codebook)[Var],line = 0,side=3,cex=0.75,family="serif",font=2)
               }
}