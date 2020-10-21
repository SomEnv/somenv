#' Calculate map dimensions
#'
#' Generate SOM map dimensions according to Vesanto heuristic rules based on the first two
#' eigenvalues of the experimental data and their related eigenvectors
#' The function has been coded in R code starting from som_dim.m
#' script present in somtoolbox for Matlab by Vesanto and adapted for the use in the shiny app
#'
#' @param dataset Experimental data
#' @param type Either "regular", "small" or "big" map (default ="regular")
#' @author Sabina Licen, Pierluigi Barbieri
#' @return This function returns a list containing the number of rows, columns and overall map units
#' @importFrom stats cor
#' @references {J. Vesanto, J. Himberg, E. Alhoniemi, J. Parhankagas, SOM Toolbox for Matlab
#' 5, Report A57, 2000, Available at: www.cis.hut.fi/projects/somtoolbox/package/papers/techrep.pdf
#' }
#' @seealso eigen, cor
#' @export
#' @examples
#' library(datasets)
#' som_dimR(iris[,1:4], type="small")

som_dimR<-function(dataset,type="regular") {
CorDat<-cor(dataset)
EigenDat<-eigen(CorDat)
Ratio<-(EigenDat$values[1]/EigenDat$values[2])^0.5
munits<-ceiling(5*nrow(dataset)^0.5)
  switch(type,
         regular = {munits<-munits},
         small = {munits<-max(9,ceiling(munits/4),na.rm=T)},
         big = {munits<-4*munits})
Col<-max(2,(min(munits,round((munits/Ratio*(0.75^0.5))^0.5,digits=0)))) # Col must be more than 1
Row<-max(2,round(munits/Col,digits=0)) # Row must be more than 1
munits<-Row*Col
return(list(Row=Row,Col=Col,munits=munits))
}
