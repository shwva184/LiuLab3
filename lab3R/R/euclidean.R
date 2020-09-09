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
