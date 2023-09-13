#' Find the greatest common divisor of two numbers
#' 
#' @description
#' This is an efficient function for computing the greatest common divisor (GCD) of two integers (numbers) a and b.
#' The arguments a and b can not be 0 because 0 has no divisor.
#' 
#' @param a,b numeric scalars or integers.
#'
#' @returns A number that is the greatest common divisor of a and b.
#' 
#' @references [link url:](https://en.wikipedia.org/wiki/Euclidean_algorithm)
#' 
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#' @export
euclidean <-
function(a, b){
    if (!(is.numeric(a)&is.numeric(b))){
        stop('the arguments should be numeric scalars or integers')
    }
    if (a == 0 || b == 0){
        stop('0 has no divisor')
    }
    r <- a%%b
    return(ifelse(r, euclidean(b, r), b))
}
