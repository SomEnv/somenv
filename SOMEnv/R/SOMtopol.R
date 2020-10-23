#' Topographical error for the SOM map
#'
#' Calculate topographical error for the SOM map
#'
#' @param dataset Experimental data used for training the map
#' @param codebook SOM codebook
#' @param grid SOM grid expressed as a matrix of x and y coordinates of the map units
#' @author Sabina Licen
#' @return This function returns the topographical error
#' @references {Clark, S., Sisson, S.A., Sharma, A. (2020) Adv. Water Resour. 143, art. no. 103676 DOI: 10.1016/j.advwatres.2020.103676}
#' @export
#' @importFrom stats dist

SOMtopol<-function(dataset,codebook,grid) {
              GridDist<-round(as.matrix(dist(grid)),digits=3)
              D<-dataset
              M<-codebook
              dlen<-nrow(D)
              munits<-nrow(M)
              dim<-ncol(D)
              mask<-matrix(1,dim,1)
              blen<-min(munits,dlen)
              BMU <- rep(0,dlen)
              BMU2<- BMU
              W1<-(mask%*%matrix(1,1,dlen))
              WD<-2*diag(dim)%*%t(D)
                   i<-0
                   while (i+1<=dlen) {
                                      inds<-c((i+1):min(dlen,i+blen)); i<-i+blen;
                                      Dist<-(M^2)%*%W1[,inds]-M%*%WD[,inds]
                                      bmu<-apply(Dist,2,function(x) which(x==min(x)))
                                      bmu2<-apply(Dist,2,function(x)which(x == sort(x,partial=2)[2]))
                                      BMU[inds] <- bmu
                                      BMU2[inds] <- bmu2
                   }#end while

              Merge<-cbind(BMU,BMU2)
              Count<-apply(Merge,1,function(x)(GridDist[x[1],x[2]]))
              CountFinal<-Count
              CountFinal[which(CountFinal<=1)]<-0
              CountFinal[which(CountFinal>1)]<-1
              TE<-round(mean(CountFinal),digits=6)
              return(TE)
          }
