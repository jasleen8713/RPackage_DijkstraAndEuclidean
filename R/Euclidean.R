#'Implements Euclidean algorithm to find GCD in the fastest possible way
#' Authors@R: Aashana, Jasleen
#' @param x as numeric
#' @param y as numeric
#' @return numeric
#' @export
#' @example euclidean(x=100, y=1000)
#' @references  <https://en.wikipedia.org/wiki/Euclidean_algorithm>

euclidean <- function(x,y){
stopifnot(is.numeric(x) && is.numeric(y) && length(x)==1 && length(y) ==1)
  
  if(x<0){
    x <- -x
  }
  if(y<0){
    y <- -y
  }
  while (x != y){
  if (x > y)
    {x <- x - y}
  else
    {y <- y - x}
  }
  return (x)

}
euclidean(-100, 1000)