#' Evaluate pairwise distance matrix for the given codebook
#'
#' The function has been coded in R code starting from som_mdist.m
#' script present in somtoolbox for Matlab by Vesanto and adapted for the use in the shiny app 
#'
#' @param codebook SOM codebook
#' @author Sabina Licen, Pierluigi Barbieri
#' @return Distance matrix
#' @seealso db_indexR
#' @references {J. Vesanto, J. Himberg, E. Alhoniemi, J. Parhankagas, SOM Toolbox for Matlab
#' 5, Report A57, 2000, Available at: www.cis.hut.fi/projects/somtoolbox/package/papers/techrep.pdf
#' }

som_mdistR<-function(codebook) {

D<-as.matrix(codebook)
dlen<-nrow(codebook)
dim<-ncol(codebook)

mask<-matrix(rep(1,dim),dim,1)
o<-rep(1,dlen)

Md<-matrix(rep(0,dlen^2),dlen,dlen)

for (i in c(1:(dlen-1))) {j<-c((i+1):dlen);
          Cd<-(D[j,]-D[(i*o[1:length(j)]),])
          Md[j,i]<-((Cd^2)%*%mask)^0.5
  Md[i,j]<-t(Md[j,i])} 

return(Md)
}