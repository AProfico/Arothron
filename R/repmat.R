#' repmat
#'
#' This function repeats copies of a matrix
#' @param X numeric: a matrix 
#' @param m numeric: number of times to repeat the X matrix in row and column dimension
#' @param n numeric: repetition factor for each dimesion
#' @return matrice: repeated matrix
#' @author Antonio Profico, Alessio Veneziano, Marina Melchionna, Pasquale Raia
#' @export
repmat <- function(X,m,n){
if(is.vector(X)==TRUE){
mx <- 1
nx <- 1  
} else{
mx <- dim(X)[1]
nx <- dim(X)[2]}
matrice<-matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)
return(matrice)
}