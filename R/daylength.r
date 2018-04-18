#' Calculates day length (in hours) and the solar elevation
#' 
#' This routine uses Forsythe et al. 1995.
#'
#' @param doy a vector with doy values 1 - 365(6)
#' @param latitude a given latitude
#' @return nested list with daylength (daylength) and
#' solar elevation (solar_elev) elements
#' @keywords solar, ephemerids
#' @export
#' @examples
#' 
#' \donttest{
#' # calcualte the hours of sunlight and solar elevation on day of year 1
#' # and latitude 51
#' ephem <- daylength(1, 51)
#' print(ephem)
#' }

daylength = function(doy, latitude) {
  
  # convert to numeric to be sure
  latitude = as.numeric(latitude)
  
  # define constant
  p = 0
  
  # degrees to radial conversion
  conv = pi / 180
  
  # Forsythe et al. 1995 eq. 1
  Omega = 0.2163108 + 2 * atan(0.9671396 * tan(0.00860 * (doy - 186)))
  
  # eq. 2
  Phi = asin(0.39795 * cos(Omega))
  
  # eq. 3 / returns daylength D
  DL = 24 - 24 / pi * acos((sin(p * conv) + sin(latitude * conv) *
                              sin(Phi)) / (cos(latitude * conv) * cos(Phi)))
  
  # convert declination to solar elevation (above ecliptica) or
  # 90 - zenith angle
  solar_elev = 90 - acos(sin(latitude * conv) * sin(Phi) + cos(latitude * conv) * cos(Phi)) * 180 / pi
  
  for (i in 1:length(DL)) {
    l <- DL[i - 1] > 20
    if (length(l) == 0) {
      l <- FALSE
    }
    l[is.na(l)] <- FALSE
    if (l  & is.na(DL[i])) {
      DL[i] <- 24
    }
    if (is.na(DL[i])) {
      DL[i - 1] <- 0
    }
  }
  return(list("daylength" = DL,
              "solar_elev" = solar_elev))
}