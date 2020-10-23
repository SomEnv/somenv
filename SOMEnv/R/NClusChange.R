#' Custom number sequence for clusters
#'
#' Changes the input vector according the custom number sequence for clusters
#'
#' @param Vector Vector containing cluster number assignment for prototypes or experimental data
#' @param NCh Vector with custom sequence of numbers for clusters
#' @author Sabina Licen
#' @return A vector of same length as input vector with cluuster numbers changed according to custom input
#' @export


NClusChange<-function(Vector, NCh)
{ if (length(NCh)!=length(levels(as.factor(Vector)))) {
print("Cluster number and custom number vector length differ") } else {
ClusDefinitivo<-unlist(Vector, recursive = TRUE, use.names = FALSE);
      ab<-as.factor(as.numeric(ClusDefinitivo));
      ac<-factor(ab,levels=levels(ab),NCh);
ClusDefinitivo<-as.numeric(as.character(ac));
return(ClusDefinitivo)}
}
