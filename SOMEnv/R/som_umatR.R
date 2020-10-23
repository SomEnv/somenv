#' Unified distance matrix for the SOM map
#'
#' The function has been coded in R code starting from som_umat.m
#' script present in somtoolbox for Matlab by Vesanto and adapted for the use in the shiny app
#'
#' @param codebook SOM codebook
#' @param Row Number of SOM map rows
#' @param Col Number of SOM map columns
#' @author Sabina Licen, Pierluigi Barbieri
#' @return The unified distance matrix for the SOM map
#' @importFrom stats median
#' @references {J. Vesanto, J. Himberg, E. Alhoniemi, J. Parhankagas, SOM Toolbox for Matlab
#' 5, Report A57, 2000, Available at: www.cis.hut.fi/projects/somtoolbox/package/papers/techrep.pdf;
#' A. Ultsch, H.P. Siemon, Proceedings of International Neural Network Conference
#' (INNC?90), Kluwer academic Publishers, Dordrecht, 1990, pp. 305?308.
#' }
#' @export

som_umatR<-function(codebook,Row,Col) {

M<-as.matrix(codebook)
munits<-Row*Col
dim<-ncol(M)

y<-Row
x<-Col

M2<-array(M,dim = c(y,x,dim))

ux<-(2*x-1)
uy<-(2*y-1)

U<-matrix(0,uy,ux)

mask<-rep(1,dim)

#--- distances between map units

for (j in c(1:y)) {
    for (i in c(1:x)) {
          if (i < x) { # horizontal
                      dx<-(M2[j,i,]-M2[j,i+1,])^2;
                      U[2*j-1,2*i]<-(mask%*%dx)^0.5;
                     }
          if (j < y) { # diagonals
                      dy<-(M2[j,i,]-M2[j+1,i,])^2;
                      U[2*j,2*i-1]<-(mask%*%dy)^0.5;

             if (j%%2==0 & i<x) {
                         dz<-(M2[j,i,]-M2[j+1,i+1,])^2;
                         U[2*j,2*i]<-(mask%*%dz)^0.5;
     } else {if (j%%2==1 & i>1) {
                         dz<-(M2[j,i,]-M2[j+1,i-1,])^2;
                         U[2*j,2*i-2]<-(mask%*%dz)^0.5;
                         }
            }

                    }
}
}

#--- values on the units

ma<-max(c(ux,uy))

for (i in seq(1,ma,2)) {
                  if (i>1 & i<ma) {A<-c(U[i-1],U[i+1]);
                                   U[i]<-median(A)
                  } else {if (i==1) {U[i]<-U[i+1]
                          } else {if (i==ma) {U[i]<-U[i-1]}}
                                  }
                        }

#---

for(j in seq(1,uy,2)) {
   for(i in seq(1,ux,2)) {
       if(i>1 & j>1 & i<ux & j<uy) {A<-c(U[j,i-1],U[j,i+1]);
                    if((j-1)%%4==0) {A<-c(A,U[j-1,i-1],U[j-1,i],U[j+1,i-1],U[j+1,i]);
                         } else {A<-c(A,U[j-1,i],U[j-1,i+1],U[j+1,i],U[j+1,i+1])}
       } else if(j==1 & i>1 & i<ux) {A<-c(U[j,i-1],U[j,i+1],U[j+1,i-1],U[j+1,i])
       } else if(j==uy & i>1 & i<ux) {A<-c(U[j,i-1],U[j,i+1])
                     if((j-1)%%4==0) {A<-c(A,U[j-1,i-1],U[j-1,i]);
                         } else {A<-c(A,U[j-1,i],U[j-1,i+1])}
       } else if(i==1 & j>1 & j<uy) {A<-c(U[j,i+1])
                     if((j-1)%%4==0) {A<-c(A,U[j-1,i],U[j+1,i]);
                         } else {A<-c(A,U[j-1,i],U[j-1,i+1],U[j+1,i],U[j+1,i+1])}
       } else if(i==ux & j>1 & j<uy) {A<-c(U[j,i-1])
                     if((j-1)%%4==0) {A<-c(A,U[j-1,i],U[j-1,i-1],U[j+1,i],U[j+1,i-1]);
                         } else {A<-c(A,U[j-1,i],U[j+1,i])}
       } else if(i==1 & j==1) {A<-c(U[j,i+1],U[j+1,i])
       } else if(i==ux & j==1) {A<-c(U[j,i-1],U[j+1,i-1],U[j+1,i])
       } else if(i==1 & j==uy) {
                     if((j-1)%%4==0) {A<-c(U[j,i+1],U[j-1,i]);
                         } else {A<-c(U[j,i+1],U[j-1,i],U[j-1,i+1])}
       } else if(i==ux & j==uy) {
                     if((j-1)%%4==0) {A<-c(U[j,i-1],U[j-1,i],U[j-1,i-1]);
                         } else {A<-c(U[j,i-1],U[j-1,i])}
       } else {A<-0}
       U[j,i]<-median(A)
   }
}

return(U)

}# END function



