##' Pressure of dissolved oxygen in air or water
##' 
##' Calculate oxygen partial pressure as a function of temperature and
##' salinity. Oxygen is a constant fraction (o2frac) of total gas pressure
##' after removing water vapor pressure.
##' 
##' See Warnings.
##' 
##' @param t A vector of temperature values in degrees Celsius
##' @param s A vector of salinity values (parts per thousand)
##' @param p Total gas pressure (in kPa), typically atmospheric pressure above
##' the body of water
##' @return Partial pressure of oxygen in kPa
##' @note %% ~~further notes~~
##' @section Warnings: It is assumed that vectors \code{t} and \code{s} have
##' the same length, or that one of the two is a constant value. This is not
##' checked in the present version, the user must make sure it is the case.
##' @author Denis Chabot
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords ~kwd1 ~kwd2
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' 
##' po2(t=10, s=28, p=101.325)
##' 
##' 
##' @export po2
po2 <-
function(t, s=0, p=101.325) # in kPa
   {
	o2frac <- 0.20946
  	(p - wvp(t, s)) * o2frac
   }

