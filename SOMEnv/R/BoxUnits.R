#' Boxplot of prototype variables split by cluster
#'
#' Boxplot function is used, box whiskers are omitted
#'
#' @param codebook Prototype codebook normalized by variable
#' @param Cluster Vector containing cluster number assignment for prototypes
#' @param Ylim Vector of length 2 for y-axis limits
#' @param xdim x axes label dimensions
#' @param pitch Vector containing the position of horizontal grid lines
#' @author Sabina Licen
#' @return Boxplot of prototype variables split by cluster
#' @importFrom graphics abline axis boxplot mtext par
#' @seealso boxplot
#' @references {Licen, S., Cozzutto, S., Barbieri, P. (2020) Aerosol Air Qual. Res., 20 (4), pp. 800-809. DOI: 10.4209/aaqr.2019.08.0414
#' }



BoxUnits<-function(codebook,Cluster,Ylim=NA,pitch=NA,xdim=0.75)
{nClus<-length(levels(as.factor(Cluster)));
 data<-data.frame(codebook,Cluster=Cluster)
 Names<-colnames(codebook)
    if (is.na(sum(Ylim))) {f<-data.frame(codebook); f<-unlist(f, recursive = TRUE, use.names = FALSE);
                      f<-data.frame(PM=f,Cluster=rep(Cluster,ncol(codebook)));
                      BOX<-boxplot(f$PM~f$Cluster,plot=F);
                      M<-max(BOX$stats);
                      m<-min(BOX$stats);
                      Ylim<-c(m-(M-m)/10,M+(M-m)/5)
    } else {m<-Ylim[1];M<-Ylim[2]}
  par(mfrow=c(nClus,1),oma=c(0.5,0.5,0,0.5),xpd=FALSE,pty="m",family="serif")
  for (j in c(1:nClus)) {par(mar=c(1.4,1,1.7,0.5));
  boxplot(data[which(data$Cluster==j),1],range=0,xaxt="n",yaxt="n",xlim=c(1,ncol(codebook)),ylim=Ylim,
  boxwex=0.9,at=1,whisklty = 0, staplelty = 0);
    if (is.na(sum(pitch))) {abline(h=c(0,round(M,digit=0)),col="gray")
    } else {abline(h=pitch,col="gray")}
  for (i in c(2:ncol(codebook)))
  {boxplot(data[which(data$Cluster==j),i],range=0,xaxt="n",yaxt="n",boxwex=0.9,at=i,add=TRUE,whisklty = 0, staplelty = 0)};
  axis(1,at=seq(1,ncol(codebook),1),labels=NA,tcl=-0.3,cex.axis=0.7);
  axis(1,at=seq(1,ncol(codebook),1),labels=Names,lwd=0,line=-0.5,cex.axis=xdim);
    if (is.na(sum(pitch))) {axis(2,at=c(0,round(M,digit=0)),labels=NA,tcl=-0.3,cex.axis=0.7);
    axis(2,at=c(0,round(M,digit=0)),labels=c(0,round(M,digit=0)),lwd=0,line=-0.7,cex.axis=0.75,las=2);
    } else {axis(2,at=pitch,labels=NA,tcl=-0.3,cex.axis=0.7);axis(2,at=pitch,labels=pitch,lwd=0,line=-0.7,cex.axis=0.75,las=2)}
  mtext(paste("Cluster ",j,sep=""),line = 0.2,side=3,cex=0.75,family="serif")}
}
