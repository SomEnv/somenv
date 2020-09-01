#' Monthly percentage frequency for each cluster
#'
#'
#' @param Date Vector containing date/time variable for experimental data
#' @param Cluster Vector containing cluster number assignment for experimental data
#' @author S. Licen
#' @return A data frame containing the monthly percentage frequency of each cluster 

# Percentage distribution of the experimental data divided by cluster and month
# Periods with no data recorded are not considered

# Require openair!!!!

FreqM<-function(Date,Cluster)
{   nClus<-length(levels(as.factor(Cluster)))
    ORIGINALS<-data.frame(date=Date,Cluster=Cluster)
    mydata <- cutData(ORIGINALS, type = "monthyear")
    Months<-levels(mydata$monthyear)
    MonthNames<-NULL
    for (i in c(1:length(Months))) {n<-paste(substr(Months[i],1,3),"-",substr(Months[i],
    nchar(Months[i])-1,nchar(Months[i])),sep="");
    MonthNames<-c(MonthNames,n)};
Ntot<-NULL;
for (j in Months) {N<-NULL;
Select<-mydata[which(mydata$monthyear==j),];
for (i in c(1:nClus)) {m<-nrow(Select[which(Select$Cluster==i),]);n<-m/nrow(ORIGINALS)*100;N<-c(N,round(n,digit=2))};
Ntot<-cbind(Ntot,N)}
Ntot<-data.frame(Cluster=c(1:nClus),Ntot)
dimnames(Ntot)[[2]]<-c("Cluster",MonthNames)
return(Ntot)
}