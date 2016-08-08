source("CONSTANTS.R")
source("radiation_functions.R")

# s.boltzmann.law gives the power per m^2 emitted by the sun.
# This is multiplied with the surface area of the sun, and
# then multiplied with sphere-surface of the su-earth distance,
# giving the power per m^2 reaching the earth:
# [W / m^2]
I.solar <- s.boltzmann.law(sun.surface.temp, 1.) * (4 * pi * sun.radius^2) / (4 * pi * sun.earth.dist^2)

eps.atm        <- 1.
# how much visible radiation is reflected back by the earth surface
albedo.ground  <- 0.33
# how much visible radiation is reflected back by the dust layer
albedo.dust    <- 0.33
# how much of the not reflected visible light is absorbed by the dust
opacity.dust   <- 1.

greenhouse.layers <- function(N=2) {
  # N: how many layers should be calculated
  # initialize the array of Temperature of each layer with an
  # unrealistic value.
  T <- array(-999.9, N)

  # read in the surface albedo from the command line
  options(warn=-1)
  dummy <- as.numeric(readline("Enter the albedo of the earth surface (0-1, default 0.33): "))
  options(warn=0)
  if (is.finite(dummy)) {
    if (dummy >= 0 && dummy <= 1) {
      albedo.ground <- dummy
    } else {
      warning(paste("Invalid range \"", dummy, "\" must be between 0 and 1.\nUse default of 0.33 for now.", sep=""))
    }
  }
  # upper most layer equals the skin or bare ground temperature, since
  # we assume steady state.
  T[1] <- ((1. - albedo.ground) * I.solar / (4. * eps.atm * sigma))^0.25

  # return, if only one layer requested
  if (N==1)
    return(T)

  # The lower layers (N) are calculated from the above one (N-1), since
  # ouput and input must be equal, according to:
  #
  #    I.up.(N-1) + I.down.(N-1) = I.up.N, with I.up.(N-1) = I.down.(N-1)
  # => 2*epsilon*sigma*T.(N-1)^4 = epsilon*sigma*T.N^4
  # => 2*T.(N-1)^4 = T.N^4
  # => T.N = root 4 (2) * T.(N-1)
  for (i in seq(from=2, to=N)) {
    T[i] = 2^0.25 * T[i-1]
  }
  #plot(c(1:N)~T, type="b")
  # return the temperature array
  return(T)
}

dusty.layers <- function() {
  # only one dust layer (N=2)
  N <- 2
  # initialize the array of Temperature of each layer with an
  # unrealistic value.
  T <- array(-999.9,N)

  # read the surface albedo, dust albedo and dust opacity as user input from the command line
  options(warn=-1)
  dummy <- as.numeric(readline("Enter the albedo of the earth surface (0-1, default 0.33): "))
  options(warn=0)
  if (is.finite(dummy)) {
    if (dummy >= 0 && dummy <= 1) {
      albedo.ground <- dummy
    } else {
      warning(paste("Invalid range \"", dummy, "\" must be between 0 and 1.\nUse default of 0.33 for now.", sep=""))
    }
  }

  options(warn=-1)
  dummy <- as.numeric(readline("Enter the albedo of the dust layer (0-1, default 0.33): "))
  options(warn=0)
  if (is.finite(dummy)) {
    if (dummy >= 0 && dummy <= 1) {
      albedo.dust <- dummy
    }
  }
  
  options(warn=-1)
  dummy <- as.numeric(readline("Enter the opacity of the dust layer,\nmust not be larger than 1 - albedo[dust] (0-1, default 1 - albedo[dust]): "))
  options(warn=0)
  if (is.finite(dummy)) {
    if (dummy >= 0 && dummy <= 1) {
      opacity.dust <- dummy
    } else {
      warning(paste("Invalid range \"", dummy, "\" must be between 0 and 1.\nUse default of 0.33 for now.", sep=""))
    }
  }
  
  if (opacity.dust + albedo.dust > 1) {
      warning("Aldebo and opacity of dust is larger than 1.\nOpacity rescaled to 1 - albedo[dust]")
      opacity.dust <- 1 - albedo.dust
  }
  
  # radiation out at the upper most layer equals
  # the radiation absorbed by the dust
  # + radiation absorbed by the ground
  # with the albedo and opacity of the dust layer,
  # and the albedo of the ground
  # for simplicity, we assume the fraction of visible light reflected
  # by the earth surface just passes the dust layer. 
  albedo.total <- (1. - albedo.dust) * opacity.dust + (1. - albedo.ground) * (1. - opacity.dust)

  T[1] <- (albedo.total * I.solar / (4. * eps.atm * sigma))^0.25
  I.dust <- s.boltzmann.law(T[1], 1.)
  T[2] <- ((1. - opacity.dust) * (1. - albedo.ground) * I.solar / (4. * eps.atm * sigma) +
           (1. - albedo.ground) * I.dust / (4. * eps.atm * sigma) )^0.25
  
  return(T)
}

#greenhouse.layers(3)
#dusty.layers()
