#' Euclidean of two numbers.
#' 
#' @param digit1 A number.
#' @param digit2 A number.
#' @description Implementation ofEuclidian algorithm to find the greatest common divisor of two numbers.
#' @return The euclidean of \code{digit1} and \code{digit2}.
#' @references https://en.wikipedia.org/wiki/Euclidean_algorithm
#' @export

euclidean <-
function(digit1,digit2){
  stopifnot(is.numeric(digit1),is.numeric(digit2),length(digit1)==1,length(digit2)==1)
  n1=abs(digit1)
  n2=abs(digit2)
  r=n1%%n2
  if (r==0){
    return(n2)
  } else {
    return(euclidean(n2,r))
  }
}
