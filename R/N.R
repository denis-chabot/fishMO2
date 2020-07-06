##' Calculates the number of non-missing values
##' 
##' Calculates the number of non-missing values in the elements of vector
##' \verb{x}.
##' 
##' Used by \verb{calcSMR}.
##' 
##' @param x A vector of numeric or character values that may include NAs.
##' @return An integer representing the number of non-missing values in
##' \verb{x}.
##' @examples
##' 
##' a = c(1:5, NA, 7:10)
##' N(a)
##' 
##' @export N
N = function(x){
    sum(!is.na(x))
}
