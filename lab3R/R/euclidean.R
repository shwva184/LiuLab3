#' Euclidean of two numbers.
#' 
#' @param digit1 A number.
#' @param digit2 A number.
#' @description Implementation ofEuclidian algorithm to find the greatest common divisor of two numbers.
#' @return The euclidean of \code{digit1} and \code{digit2}.
#' @references https://en.wikipedia.org/wiki/Euclidean_algorithm
#' @examples
#' euclidean(1, 1)
#' euclidean(10, 1)
#' @export

euclidean <-
function(digit1,digit2){
  stopifnot(is.numeric(digit1),is.numeric(digit2),length(digit1)==1,length(digit2)==1)
  if (digit1==0){
    return(digit2)
  } else {
    return(euclidean(digit2%%digit1,digit1))
  }
}
