% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BoxClus.R
\name{BoxClus}
\alias{BoxClus}
\title{Boxplot of prototype variables split by cluster and variable}
\usage{
BoxClus(Dms, codebook, Cluster)
}
\arguments{
\item{Dms}{A vector of length 2, where the first argument specifies the number of rows and the second the number of columns of plots (see mfrow in par)}

\item{codebook}{De-normalized prototype codebook}

\item{Cluster}{Vector containing cluster number assignment for prototypes}
}
\value{
Boxplot of prototype variables split by cluster
}
\description{
Boxplot function is used, box whiskers are omitted
}
\references{
{Licen, S., Cozzutto, S., Barbieri, P. (2020) Aerosol Air Qual. Res., 20 (4), pp. 800-809. DOI: 10.4209/aaqr.2019.08.0414
}
}
\seealso{
boxplot, par
}
\author{
Sabina Licen
}
