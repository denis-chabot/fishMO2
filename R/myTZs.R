##' List of time zones.
##' 
##' List of time zones at sites where respirometry data were available to test
##' this package.
##' 
##' Can be used to specify time zone in \code{drawNights}. The choice of using
##' daylight saving time or not must match how the computer was setup when
##' respirometry data were obtained.
##' 
##' @return A list with 8 elements:
##' 
##' \item{IMLcivil}{IML (Institut Maurice-Lamontagne, Maurice Lamontagne
##' Institute), uses daylight saving time in Summer.} \item{IMLst}{IML
##' (Institut Maurice-Lamontagne, Maurice Lamontagne Institute), uses standard
##' time year long.} \item{FRcivil}{France, civil time, i.e. uses daylight
##' saving time in Summer.} \item{FRst}{France, standard time, i.e. does not
##' use daylight saving time in Summer.} \item{DKcivil}{Danemark, civil time,
##' i.e. uses daylight saving time in Summer.} \item{DKst}{Danemark, standard
##' time, i.e. does not use daylight saving time in Summer.}
##' \item{BCcivil}{British Columbia, civil time, i.e. uses daylight saving time
##' in Summer.} \item{BCst}{British Columbia, standard time, i.e. does not use
##' daylight saving time in Summer.}
##' @examples
##' 
##' myTZs$IMLst
##' 
##' @export myTZs
myTZs = list(
    IMLcivil = "Canada/Eastern", 
    IMLst = "Etc/GMT+5", 
    FRcivil = "Europe/Paris", 
    FRst = "Etc/GMT-1",
    DKcivil = "Europe/Copenhagen",
    DKst = "Etc/GMT-1",
    BCcivil = "Canada/Pacific",
    BCst = "ETC/GMT+8"
)
