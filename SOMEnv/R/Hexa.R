#' Function to draw an hexagon around a point
#'
#' Draw an hexagon around a point of x and y coordinates
#'
#' @param x X-coordinate of the hexagon center
#' @param y Y-coordinate of the hexagon center
#' @param col Filling color of the hexagon (default NA)
#' @param border Border color of the hexagon (default "gray")
#' @param unitcell The distance side to side between two parallel sides of the hexagon (default 1)
#' @author S. Licen
#' @return This function draws an hexagon on a plot 

Hexa<-function (x, y,  col = NA, border = "gray",unitcell=1) 
{Apo<-unitcell/2; R<-Apo/(cos(pi/6)); Inc<-Apo/(tan(pi/3));
    polygon(c(x, x+Apo, x+Apo, x, x-Apo, x-Apo), 
            c(y-R,y-R+Inc,y+R-Inc,y+R,y+R-Inc,y-R+Inc), col = col, border = border)}