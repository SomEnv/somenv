#' Cluster assignment for the experimental data
#'
#' Generate a vector containing the cluster assignment to experimental data 
#'
#' @param Bmus Best Matching Unit assignment to the experimental data
#' @param Cluster Vector containing cluster number assignment for prototypes
#' @author S. Licen
#' @return A vector containing the cluster assignment to experimental data 

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