euclidean <-
function(a, b){
    if (!(is.numeric(a)&is.numeric(b))){
        stop('Args should be numeric scalars')
    }
    if (a == 0 || b == 0){
        stop('0 has no divisor')
    }
    r <- a%%b
    return(ifelse(r, euclidean(b, r), b))
}
