#' Basic statistics of values present in the input vector
#'
#' Generate basic statistics for the input vector
#'
#' The outilers and quartiles are evaluated by boxplot function applying default parameters.
#'
#' @param Numeric vector 
#' @author S. Licen
#' @return A table which contains basic statistics for the input vector   


#################################################################### A POSTO
# FUNZIONE per disegnare grafico con esagoni colorati a quantili secondo num. di Hits:
paramQuant<-function(param) 
{ Param<-unlist(param, recursive = TRUE, use.names = FALSE);
  BOX<-boxplot(Param,plot=FALSE);
  MIN<-min(Param,na.rm=TRUE);
  MAX<-max(Param,na.rm=TRUE);
  Quart<-data.frame(Value=BOX$stats[,1])
  Tab<-data.frame(Statistic=c("Min","LowerOut","1stQ","Median","3rdQ","UpperOut","Max"),Value=rbind(MIN,Quart,MAX))
  return(Tab);
}

