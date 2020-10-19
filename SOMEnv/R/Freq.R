#' Percentage frequency for each cluster
#'
#'
#' @param Cluster Vector containing cluster number assignment for experimental data
#' @author Sabina Licen
#' @return A data frame containing the percentage frequency of each cluster 
#' @references {Licen, S., Cozzutto, S., Barbieri, P. (2020) Aerosol Air Qual. Res., 20 (4), pp. 800-809. DOI: 10.4209/aaqr.2019.08.0414
#' }

Freq<-function(Cluster)
{FRE<-NULL
nClus<-length(levels(as.factor(Cluster)))
for  (i in c(1:nClus)) {n<-length(Cluster[which(Cluster==i)])/length(Cluster)*100;FRE<-c(FRE,round(n,digit=1))}
NEW<-data.frame(Cluster=paste("Cl",seq(1,nClus,1),sep=""),Freq=FRE)
return(NEW)
}