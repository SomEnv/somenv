#' Basic statistics of values present in the input vector
#'
#' Generate basic statistics for the input vector
#'
#' The outilers and quartiles are evaluated by boxplot function applying default parameters.
#'
#' @param param Numeric vector
#' @author Sabina Licen
#' @return A table which contains basic statistics for the input vector
#' @importFrom graphics boxplot
#' @seealso boxplot
#' @export
#' @examples
#' library(datasets)
#' paramQuant(iris[,1])

paramQuant<-function(param)
{ Param<-unlist(param, recursive = TRUE, use.names = FALSE);
  BOX<-boxplot(Param,plot=FALSE);
  MIN<-min(Param,na.rm=TRUE);
  MAX<-max(Param,na.rm=TRUE);
  Quart<-data.frame(Value=BOX$stats[,1])
  Tab<-data.frame(Statistic=c("Min","LowerOut","1stQ","Median","3rdQ","UpperOut","Max"),Value=rbind(MIN,Quart,MAX))
  return(Tab);
}

