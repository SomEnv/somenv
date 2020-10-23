#' Boxplot of prototype variables split by cluster and variable
#'
#' Boxplot function is used, box whiskers are omitted
#'
#' @param Dms A vector of length 2, where the first argument specifies the number of rows and the second the number of columns of plots (see mfrow in par)
#' @param codebook De-normalized prototype codebook
#' @param Cluster Vector containing cluster number assignment for prototypes
#' @author Sabina Licen
#' @return Boxplot of prototype variables split by cluster
#' @seealso boxplot, par
#' @importFrom graphics axis boxplot mtext par
#' @references {Licen, S., Cozzutto, S., Barbieri, P. (2020) Aerosol Air Qual. Res., 20 (4), pp. 800-809. DOI: 10.4209/aaqr.2019.08.0414
#' }
#' @export



BoxClus<-function (Dms,codebook,Cluster)
{ ColIndex<-c(1:ncol(codebook))
  if ((Dms[1]*Dms[2])>=length(ColIndex)) {ColIndex<-ColIndex
  } else {ColIndex<-c(1:(Dms[1]*Dms[2]))}
  par(mfrow=Dms,oma=c(0.5,0.5,0.5,0.5),mar=c(1.5,1.5,2,1),xpd=FALSE,pty="m",family="serif")
for (j in ColIndex) {
  Var<-j
  BOX<-boxplot(codebook[,Var]~Cluster,xaxt="n",yaxt="n",boxwex=0.8,range=0)
  axis(1,at=seq(1,length(levels(as.factor(Cluster))),1),labels=NA,tcl=-0.3,cex.axis=0.7);
  axis(1,at=seq(1,length(levels(as.factor(Cluster))),1),seq(1,length(levels(as.factor(Cluster))),1),lwd=0,line=-0.5,cex.axis=0.9)
  axis(2,labels=NA,tcl=-0.3,cex.axis=0.7);
  axis(2,lwd=0,line=-0.7,cex.axis=0.65,las=2)
  mtext(colnames(codebook)[Var],line = 0.2,side=3,cex=0.65,family="serif")
               }
}
