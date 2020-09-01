#' Calculate map dimensions
#'
#' Generate SOM map dimensions according to Vesanto heuristic rules
#'
#' @param dataset Experimental data
#' @param type Either "regular", "small" or "big" map (default ="regular")
#' @author S. Licen
#' @return This function returns a list containing the number of rows, columns and overall map units

som_dimR<-function(dataset,type="regular") {
CorDat<-cor(dataset)
EigenDat<-eigen(CorDat)
Ratio<-(EigenDat$values[1]/EigenDat$values[2])^0.5
munits<-ceiling(5*nrow(dataset)^0.5)
  switch(type,
         regular = {munits<-munits},
         small = {munits<-max(9,ceiling(munits/4),na.rm=T)},
         big = {munits<-4*munits})
Col<-max(2,(min(munits,round((munits/Ratio*(0.75^0.5))^0.5,digit=0)))) # Col must be more than 1
Row<-max(2,round(munits/Col,digit=0)) # Row must be more than 1
munits<-Row*Col
return(list(Row=Row,Col=Col,munits=munits))
}