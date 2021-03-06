#' Daily percentage frequency for each cluster
#'
#'
#' @param Date Vector containing date/time variable for experimental data
#' @param Cluster Vector containing cluster number assignment for experimental data
#' @param Total Number of observations per day
#' @author Sabina Licen
#' @return A data frame containing the daily percentage frequency of each cluster
#' @import openair dplyr
#' @references {Licen, S., Cozzutto, S., Barbieri, P. (2020) Aerosol Air Qual. Res., 20 (4), pp. 800-809. DOI: 10.4209/aaqr.2019.08.0414
#' }
#' @export

FreqD<-function(Date,Cluster,Total=1440)
{   nClus<-length(levels(as.factor(Cluster)))
 g<-as.POSIXct(substr(Date, 1, 10),format = "%Y-%m-%d",tz="GMT")#NEW!!!
 g2<-levels(as.factor(g))
 interval<-seq(as.POSIXct(g2[1],format = "%Y-%m-%d",tz="GMT"), as.POSIXct(g2[length(g2)],format = "%Y-%m-%d",tz="GMT"), by = "1 day")
 mydata<-data.frame(date=Date,day=g,Cluster=Cluster)
    FRE<-data.frame(Canc=rep(0,nClus+1));
    for  (j in interval) {Select<-mydata[which(mydata$day==j),];
    fre<-NULL;
    for  (i in c(1:nClus)) {n<-nrow(Select[which(Select$Cluster==i),])/Total*100;fre<-c(fre,round(n,digits=1))}
    s<-(100-sum(fre,na.rm=T));fre<-c(fre,round(s,digits=1));FRE<-cbind(FRE,fre)}
    FRE<-data.frame(FRE)
    FRE<-FRE[,-1]
    colnames(FRE)<-paste0(substr(interval, 9, 10),"/",substr(interval, 6, 7))
    FRE2<-t(FRE)
colnames(FRE2)<-c(paste0("Cl",c(1:nClus)),"ND")
FRE2<-data.frame(FRE2)
FRE2[which(FRE2$ND<0),"ND"]<-0
return(FRE2)
}




