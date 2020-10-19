#' Prototype coordinates for graph
#'
#' Generate X and Y coordinates for plotting a SOM map shaped according to Vesanto visualization fashion
#'
#' @param Row Number of SOM map rows
#' @param Col Number of SOM map columns
#' @author Sabina Licen, Pierluigi Barbieri
#' @return This function returns a \code{data.frame} including columns:
#' \itemize{
#'  \item X
#'  \item Y
#' }
#'
#' @references {J. Vesanto, J. Himberg, E. Alhoniemi, J. Parhankagas, SOM Toolbox for Matlab
#' 5, Report A57, 2000, Available at: www.cis.hut.fi/projects/somtoolbox/package/papers/techrep.pdf;
#' Licen, S., Cozzutto, S., Barbieri, P. (2020) Aerosol Air Qual. Res., 20 (4), pp. 800-809. DOI: 10.4209/aaqr.2019.08.0414
#' }
#' @export
#' @examples
#' Coord<-CodeCoord(10,5)

CodeCoord<-function (Row, Col)
{ unitcell<-1;
Apo<-unitcell/2;R<-Apo/(cos(pi/6));
if (Row %% 2 == 0) {
   Yd<-seq(1,Row,2);
   Yp<-seq(2,Row,2);
   Xc<-seq(1-Apo,(Col+Apo)*2*Apo-Apo,Apo);
     YHD<-NULL;
     for (i in c(0:(length(Yd)-1))) {k<-1+i*R*3;YHD<-c(YHD,k)};
     YHP<-NULL;
     for (i in c(0:(length(Yp)-1))) {k<-1+R*1.5+i*R*3;YHP<-c(YHP,k)};
     YHD<-(-(sort(-YHD)));
     YHP<-(-(sort(-YHP)));
   Coord<-NULL;
   COO<-NULL;
     for (i in seq(1,length(Xc),2)) {XHD<-rep(Xc[i+1],length(YHD));XHP<-rep(Xc[i],length(YHP));XC<-c(XHD,XHP);YC<-c(YHD,YHP);
     COO<-data.frame(XC=XC,YC=YC);COO<-COO[order(-COO$YC),];dimnames(COO)[[1]]<-seq(1,nrow(COO),1);
     Coord<-rbind(Coord,COO)};
} else {
   Yd<-seq(1,Row,2);
   Yp<-seq(2,Row,2);
   Xc<-seq(1-Apo,(Col+Apo)*2*Apo-Apo,Apo);
     YHD<-NULL;
     for (i in c(0:(length(Yd)-1))) {k<-1+i*R*3;YHD<-c(YHD,k)};
     YHP<-NULL;
     for (i in c(0:(length(Yp)-1))) {k<-1+R*1.5+i*R*3;YHP<-c(YHP,k)};
     YHD<-(-(sort(-YHD)));
     YHP<-(-(sort(-YHP)));
   Coord<-NULL;
   COO<-NULL;
     for (i in seq(1,length(Xc),2)) {XHD<-rep(Xc[i],length(YHD));XHP<-rep(Xc[i+1],length(YHP));XC<-c(XHD,XHP);YC<-c(YHD,YHP);
     COO<-data.frame(XC=XC,YC=YC);COO<-COO[order(-COO$YC),];dimnames(COO)[[1]]<-seq(1,nrow(COO),1);
     Coord<-rbind(Coord,COO)};
}
 Coord<-data.frame(Coord);
 colnames(Coord)<-c("X","Y");
 return(Coord)
    }


