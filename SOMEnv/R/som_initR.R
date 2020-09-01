#' Calculate initialization matrix for SOM training
#'
#' Generate SOM map dimensions according to Vesanto heuristic rules related to the first two
#' eigenvalues of the experimental data and their related eigenvectors
#'
#' @param dataset Experimental data
#' @param Row Number of SOM map rows
#' @param Col Number of SOM map columns
#' @param munits Number of SOM map units (Row*Col)
#' @author S. Licen
#' @return This function returns an initialization matrix for SOM training


som_initR<-function(dataset,Row,Col,munits) {
dataset<-as.matrix(dataset)
CorDat<-cor(dataset)
EigenDat<-eigen(CorDat)
EigenVec<-data.frame(EigenDat$vector)
EigenVal<-EigenDat$values
Vecs<-EigenVec[,1:2]
Vals<-EigenVal[1:2]
#----
M<-NULL
for (i in c(1:ncol(dataset))) {p<-mean(dataset[,i]);M<-c(M,p)}
Vecs2<-NULL
for (i in c(1:2)){b<-(Vecs[,i]/sum(Vecs[,i]^2)^0.5)*(Vals[i])^0.5;Vecs2<-cbind(Vecs2,b)}
#----
k<-c(1, Row)
inds<-c(0:(munits-1))

Coords<-NULL
for(i in c(2,1)) {q<-floor(inds/k[i]);inds<-(inds%%k[i]);Coords<-cbind(q,Coords)}
Coords2<-cbind(Coords[,2],Coords[,1])

indsrow<-(c(1:Col)-1)*Row
for(i in seq(2,Row,2)) {Coords2[i+indsrow,1]<-Coords2[i+indsrow,1]+0.5}

Coords2[,2]<-Coords2[,2]*(0.75)^0.5
#----
CodB01<-NULL
for (i in c(1:munits)) {CodB01<-rbind(CodB01,M)}

Coords2<-cbind(Coords2[,2],Coords2[,1])

for(i in c(1,2)) {ma<-max(Coords2[,i]);mi<-min(Coords2[,i]);
if(ma>mi) {Coords2[,i]<-((Coords2[,i]-mi)/(ma-mi))} else {Coords2[,i]<-0.5}
}

Coords2<-(Coords2-0.5)*2

for (n in c (1:munits)) {for (d in c(1,2)){CodB01[n,]<-CodB01[n,]+Coords2[n,d]*Vecs2[,d]}}

rownames(CodB01)<-c(1:munits)
colnames(CodB01)<-colnames(dataset)

return(CodB01)

}


