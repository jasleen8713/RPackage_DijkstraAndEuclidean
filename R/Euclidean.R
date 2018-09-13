#'Implements Euclidean algorithm to find GCD in the fastest possible way
#' Authors@R: Aashana, Jasleen
#' @param x numeric
#' @param y numeric
#' 
#' @return GCD for x and y
#' @export
#' @example euclidean(100, 1000)

euclidean <- function(x,y){
stopifnot(is.numeric(x) && is.numeric(y) && length(x)==1 && length(y) ==1)
  # small<-min(x,y)
  # i<-1
  # k<- vector()
  # for(i in 1:small)
  #   if(x%%i==0 && y%%i==0 ){
  #     k <- i
  #   }
  # return(k)
  
  while (x != y){
  if (x > y)
    {x <- x - y}
  else
    {y <- y - x}
  }
  return (x)
  
}
euclidean(13892347912, 123612)
euclidean(100, 1000)
