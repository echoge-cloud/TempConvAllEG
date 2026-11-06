#' Fahrenheit conversion
#'
#' Convert degrees Fahrenheit temperatures to degrees Celsius
#' @param F_temp The temperature in degrees Fahrenheit
#' @return The temperature in degrees Celsius
#' @examples
#' temp1 <- F_to_C(50)
#' temp2 <- F_to_C(c(50, 63, 23))
#' @export
F_to_C <- function(F_temp){
  C_temp <- (F_temp - 32) * 5/9
  return(C_temp)
}

#' Fahrenheit to Kelvin conversion
#'
#' Convert degrees Fahrenheit temperatures to Kelvin
#' @param F_temp The temperature in degrees Fahrenheit
#' @return The temperature in Kelvin
#' @examples
#' temp1 <- F_to_K(18)
#' temp2 <- F_to_K(c(39, 50, 46))
#' @export
F_to_K <- function(F_temp){
  K_temp <- (F_temp - 32) * 5/9 + 273
  return(K_temp)
}

#' Celsius to Fahrenheit conversion
#'
#' Convert degrees Celsius temperatures to degrees Fahrenheit
#' @param C_temp The temperature in degrees Celsius
#' @return The temperature in degrees Fahrenheit
#' @examples
#' temp1 <- C_to_F(10)
#' temp2 <- C_to_F(c(0, 20, 100))
#' @export
C_to_F <- function(C_temp){
  F_temp <- (C_temp * 9/5) + 32
  return(F_temp)
}

#' Celsius to Kelvin conversion
#'
#' Convert degrees Celsius temperatures to Kelvin
#' @param C_temp The temperature in degrees Celsius
#' @return The temperature in Kelvin
#' @examples
#' temp1 <- C_to_K(10)
#' temp2 <- C_to_K(c(-40, 0, 100))
#' @export
C_to_K <- function(C_temp){
  K_temp <- C_temp + 273
  return(K_temp)
}

#' Kelvin to Fahrenheit conversion
#'
#' Convert temperatures in Kelvin to degrees Fahrenheit
#' @param K_temp The temperature in Kelvin
#' @return The temperature in degrees Fahrenheit
#' @examples
#' temp1 <- K_to_F(273)
#' temp2 <- K_to_F(c(233, 273, 373))
#' @export
K_to_F <- function(K_temp){
  F_temp <- (K_temp - 273) * 9/5 + 32
  return(F_temp)
}

#' Kelvin to Celsius conversion
#'
#' Convert temperatures in Kelvin to degrees Celsius
#' @param K_temp The temperature in Kelvin
#' @return The temperature in degrees Celsius
#' @examples
#' temp1 <- K_to_C(273)
#' temp2 <- K_to_C(c(233, 273, 373))
#' @export
K_to_C <- function(K_temp){
  C_temp <- K_temp - 273
  return(C_temp)
}