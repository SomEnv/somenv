#' Plot of daily percentages for each cluster
#'
#' The function tries to guess the correct total number of records per day (e.g. 1440 for minutely data)
#'
#' @param experimental Experimental data (must contain variable "date")
#' @param TrainClus Vector containing cluster number assignment for experimental data
#' @param colSeq Color sequence for the clusters
#' @param Total Number of observations per day
#' @param xdim x axes label dimensions
#' @param ydim y axes label dimensions
#' @author S. Licen
#' @return Plot of daily percentages for each cluster 

# Require openair!!!! Ultima riga sono le osservazione giornaliere not determined!!!

DailyBar<-function(experimental,TrainClus,colSeq=rainbow(length(levels(as.factor(TrainClus)))),Total=1440,xdim=0.7,ydim=0.8)
{colSeq<-c(colSeq,"black");
 nClus<-length(levels(as.factor(TrainClus)))
 g<-as.POSIXct(substr(experimental$date, 1, 10),format = "%Y-%m-%d",tz="GMT")#NEW!!!
# g<-ordered(g, levels = unique(g))
 g2<-levels(as.factor(g))
 interval<-seq(as.POSIXct(g2[1],format = "%Y-%m-%d",tz="GMT"), as.POSIXct(g2[length(g2)],format = "%Y-%m-%d",tz="GMT"), by = "1 day") #NEW!!!
# Count<-count(g)
# Total<-round(median(Count$freq,na.rm=T),digit=0)
 mydata<-data.frame(experimental$date,day=g,Cluster=TrainClus)
    FRE<-data.frame(Canc=rep(0,nClus+1));
    for  (j in interval) {Select<-mydata[which(mydata$day==j),];
    fre<-NULL;
    for  (i in c(1:nClus)) {n<-nrow(Select[which(Select$Cluster==i),])/Total*100;fre<-c(fre,round(n,digit=1))}
    s<-(100-sum(fre,na.rm=T));fre<-c(fre,s);FRE<-cbind(FRE,fre)}
    FRE<-data.frame(FRE)
    FRE<-FRE[,-1]
    colnames(FRE)<-paste0(substr(interval, 9, 10),"/",substr(interval, 6, 7))
    FRE2<-as.matrix(FRE)
LAB<-colnames(FRE2)
par(oma=c(0,0,0,0),xpd=T)
BAR<-barplot(FRE2,col=colSeq,ylab="",xlab="",xaxt="n",yaxt="n",ylim=c(0,100),density=c(rep(NA,nClus),10),angle=c(rep(NA,nClus),45))
    axis(1,at=BAR,labels=NA,tcl=-0.3,cex.axis=xdim)
    axis(1,at=BAR,labels=LAB,lwd=0,line=-0.5,cex.axis=xdim,las=2)
    axis(2,at=seq(0,100,25),labels=NA,tcl=-0.3,cex.axis=ydim)
    axis(2,at=seq(0,100,25),labels=seq(0,100,25),lwd=0,line=-0.5,cex.axis=ydim,las=1)
    I<-BAR[length(BAR)-1]
    F<-BAR[length(BAR)]
    legend(quantile(BAR,prob=0.1),110,ncol=nClus+1,legend=c(paste("Cl",seq(1,nClus,1),sep=""),"ND"),fill=colSeq,bty="n",cex=0.85,
    density=c(rep(NA,nClus),10),angle=c(rep(NA,nClus),45))
}
