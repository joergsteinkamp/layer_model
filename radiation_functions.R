planck.law <- function(T, lambda) {
# emissive power [W/m^2/m]
  2 * pi * c^2 * h * lambda^-5 / (exp((c * h) / (k * lambda * T))-1)
# lambda_max = 2.897*10^6/T
# see Seinfeld and Pandis pp. 100
}

lambda.max <- function(T) {
  # Wien's law
  2.89776e-3 / T
}

s.boltzmann.law <- function(T, epsilon){
  epsilon * sigma * T^4
}
