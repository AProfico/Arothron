#' grid_pov
#'
#' This function creates a grid for an interactive way to define the set of the points of view 
#' @param mesh object of class mesh3d 
#' @param expand numeric: scale factor for the grid for the interactive 3D plot
#' @return matrice matrix: matrix with the x,y,z coordinates of the points of view
#' @author Antonio Profico, Alessio Veneziano, Marina Melchionna, Pasquale Raia
#' @export
grid_pov<-function (mesh, expand=1){
  bounding <- as.matrix(meshcube(scalemesh(mesh,size =expand,center = "mean" )))
  matrice <- bounding
  for (z in 1:3) {
    matrice <- matrice
    dimension <- nrow(matrice)
    for (i in 1:dimension) {
      A <- matrice[i, ]
      for (j in 1:dimension) {
        B <- matrice[j, ]
        C <- (A + B)/2
        matrice <- rbind(matrice, C)
      }
    }
    matrice <- unique(matrice)
  }
  return(matrice)
}
