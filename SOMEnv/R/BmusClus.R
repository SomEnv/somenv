#' Cluster assignment for the experimental data
#'
#' Generate a vector containing the cluster assignment to experimental data
#'
#' @param Bmus Best Matching Unit assignment to the experimental data
#' @param Cluster Vector containing cluster number assignment for prototypes
#' @author Sabina Licen
#' @return A vector containing the cluster assignment to experimental data
#' @importFrom plyr mapvalues
#' @references {Licen, S., Cozzutto, S., Barbieri, P. (2020) Aerosol Air Qual. Res., 20 (4), pp. 800-809. DOI: 10.4209/aaqr.2019.08.0414
#' }
#' @export

BmusClus<-function(Bmus,Cluster)
{   TotBmus<-data.frame(Bmus=Bmus);
          dimnames(TotBmus)[[2]]<-c("Bmus");
          mn<-as.factor(TotBmus$Bmus);
        CL<-NULL;
        for(i in c(as.numeric(levels(mn)))){c<-Cluster[i];CL<-c(CL,c)};
        mp<-mapvalues(mn, from = levels(mn), to = CL);
    Output<-as.numeric(as.character(mp));
    return(Output)
}
