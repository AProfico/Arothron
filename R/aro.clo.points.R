#' aro.clo.points
#'
#' Find the closest matches between a reference (2D or 3D matrix) and a target matrix (2D/3D) or mesh returning row indices and distances
#' @param target kxm matrix or object of class "mesh3d"
#' @param reference numeric: a kxm matrix (coordinates)
#' @return position numeric: a vector of the row indices 
#' @return distances numeric: a vector of the coordinates distances 
#' @author Antonio Profico, Alessio Veneziano, Marina Melchionna, Pasquale Raia
#' @examples
#' #load an example: mesh, and L set
#' data(yoda_sur)
#' data(yoda_set)
#' sur<-yoda_sur
#' set<-yoda_set
#' ver_pos<-aro.clo.points(target=sur,reference=set) 
#' @export

aro.clo.points=function (target, reference) 
{
  
  if(class(target)=="mesh3d"){
    target=t(target$vb)[,-4]  
  }
  
  if(class(reference)=="mesh3d"){
    reference=t(reference$vb)[,-4]  
  }
  
  if(dim(target)[2]!=dim(reference)[2]){
  stop("the number of columns (d) must be the same")  
  }
  
    position <- NULL
    distances <- NULL
    for (i in 1:dim(reference)[1]) {
        distance <- sqrt((target[, 1] - reference[i, 1])^2 + 
            (target[, 2] - reference[i, 2])^2 + (target[, 
            3] - reference[i, 3])^2)
        mindist_selector <- which(distance == min(distance))
        position <- c(position, mindist_selector)
        distances <- c(distances, min(distance))
    }
    out=list("position"=position,"distances"=distances)
    return(out)
}



