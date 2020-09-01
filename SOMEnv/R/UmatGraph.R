#' U-matrix plot
#'
#' Plot of Unified Distance Matrix using a grayscale according to quartiles
#'
#' The function plots a SOM map for the values of each modeled variable using a grayscale according to quartiles,
#' from dark gray (lower distances) to light gray (higher distances).
#' The quartiles are evaluated by boxplot function applying default parameters.
#'
#' @param umat Unified Distance Matrix
#' @param Row Number of SOM map rows
#' @param Col Number of SOM map columns
#' @author S. Licen
#' @return Plot of Unified Distance Matrix using a grayscale according to quartiles   

UmatGraph<-function(umat,Row,Col,colorscale=c("bw","gs")) 
{   RowU<-Row*2-1;
    ColU<-Col*2-1;
    CodeUmat<-CodeCoord(RowU, ColU);
    DISTANCE<-NULL
    for (i in c(1:ncol(umat))) {column<-umat[,i];DISTANCE<-c(DISTANCE,column)}
    UMAT<-data.frame(X=CodeUmat$X,Y=CodeUmat$Y,Value=DISTANCE)
if(colorscale=="bw") {colSeq <- c("black","gray50","gray60","gray75","gray85","white");
} else {colSeq <- c("palegreen4","darkolivegreen3","darkolivegreen2","darkolivegreen1","lemonchiffon1","snow")}
  Var<-unlist(UMAT$Value, recursive = TRUE, use.names = FALSE);
  BOX<-boxplot(Var,plot=FALSE);
  MIN<-min(Var);
  MAX<-max(Var);
  MINCol<-colSeq[1];
  INTRACol<-colSeq[2:5];
  MAXCol<-colSeq[6];
     if (min(Var)==BOX$stats[1]){MIN<-NULL; MINCol<-NULL} else {};
     if (max(Var)==BOX$stats[5]){MAX<-NULL; MAXCol<-NULL} else {};
  Breaks<-c(MIN,as.numeric(BOX$stats),MAX);
  Labels<-c(MINCol,INTRACol,MAXCol);
  FACT<-cut(Var, breaks=Breaks,labels = Labels,include.lowest = TRUE, right = FALSE);
unitcell<-1 
Apo<-unitcell/2;R<-Apo/(cos(pi/6));  
par(mar=c(1,1,1,2),pty="m",xpd=TRUE,family="serif");
plot(c(1-2*Apo,(Col*2-1)+Apo),c(1-R,1+((Row*2-1)-1)*(R*1.5)+R),
type="n",xlab="",ylab="",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n");
for (i in c(1:nrow(UMAT))) {Hexa(UMAT$X[i],UMAT$Y[i],col=as.character(FACT[i]),border=NA)};
}
