#' array2list
#'
#' converts an array in a list storing each element of the third dimension of the array (specimen) as element of the list 
#' @param array a kx3xn array with landmark coordinates
#' @return a list containing the landmark configurations stored as separated elements 
#' @author Antonio Profico, Alessio Veneziano, Marina Melchionna, Paolo Piras, Pasquale Raia
#' @export

array2list<-function (array) 
{
  thelist <- NULL
  for (i in 1:dim(array)[3]) {
    eli <- array[, , i]
    thelist <- c(thelist, list(eli))
  }
  if (is.null(dimnames(array)[[3]]) == F) {
    names(thelist) <- dimnames(array)[[3]]
  }
  return(thelist)
}

