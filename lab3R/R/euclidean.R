#' Euclidean of two numbers.
#' 
#' @param digit1 A number.
#' @param digit2 A number.
#' @return The euclidean of \code{digit1} and \code{digit2}.
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
