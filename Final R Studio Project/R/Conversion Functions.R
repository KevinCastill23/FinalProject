#' Convert distance between units
#'
#' @param value Numeric value to convert.
#' @param from_unit Character, the unit to convert from.
#' @param to_unit Character, the unit to convert to.
#'
#' @return The converted value.
#' @examples
#' convert_distance(1, "km", "mi")
#' convert_distance(1000, "m", "ft")
convert_distance <- function(value, from_unit, to_unit) {
  units <- c("km", "m", "mi", "ft")
  if (!(from_unit %in% units) || !(to_unit %in% units)) {
    stop("Invalid unit provided.")
  }

  # Convert input to meters
  value_in_meters <- switch(from_unit,
                            km = value * 1000,
                            m = value,
                            mi = value * 1609.34,
                            ft = value * 0.3048)

  # Convert meters to desired output unit
  converted_value <- switch(to_unit,
                            km = value_in_meters / 1000,
                            m = value_in_meters,
                            mi = value_in_meters / 1609.34,
                            ft = value_in_meters / 0.3048)

  return(converted_value)
}

#' Convert weight between units
#'
#' @param value Numeric value to convert.
#' @param from_unit Character, the unit to convert from.
#' @param to_unit Character, the unit to convert to.
#'
#' @return The converted value.
#' @examples
#' convert_weight(1, "kg", "lb")
#' convert_weight(100, "g", "oz")
convert_weight <- function(value, from_unit, to_unit) {
  units <- c("kg", "g", "lb", "oz")
  if (!(from_unit %in% units) || !(to_unit %in% units)) {
    stop("Invalid unit provided.")
  }

  # Convert input to grams
  value_in_grams <- switch(from_unit,
                           kg = value * 1000,
                           g = value,
                           lb = value * 453.592,
                           oz = value * 28.3495)

  # Convert grams to desired output unit
  converted_value <- switch(to_unit,
                            kg = value_in_grams / 1000,
                            g = value_in_grams,
                            lb = value_in_grams / 453.592,
                            oz = value_in_grams / 28.3495)

  return(converted_value)
}

#' Convert temperature between units
#'
#' @param value Numeric value to convert.
#' @param from_unit Character, the unit to convert from.
#' @param to_unit Character, the unit to convert to.
#'
#' @return The converted value.
#' @examples
#' convert_temperature(0, "C", "F")
#' convert_temperature(32, "F", "K")
convert_temperature <- function(value, from_unit, to_unit) {
  units <- c("C", "F", "K")
  if (!(from_unit %in% units) || !(to_unit %in% units)) {
    stop("Invalid unit provided.")
  }

  # Convert input to Kelvin
  value_in_kelvin <- switch(from_unit,
                            C = value + 273.15,
                            F = (value + 459.67) * (5/9),
                            K = value)

  # Convert Kelvin to desired output unit
  converted_value <- switch(to_unit,
                            C = value_in_kelvin - 273.15,
                            F = value_in_kelvin * (9/5) - 459.67,
                            K = value_in_kelvin)

  return(converted_value)
}
