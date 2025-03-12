library(tidyverse)

##poisson function

pois_probabilities <- function(x, lambda, type = "<=") {
  prob <- case_when(
    type == "="  ~ dpois(x, lambda),  # P(X = x)
    type == "!=" ~ 1 - dpois(x, lambda),  # P(X ≠ x)
    type == "<"  ~ ppois(x - 1, lambda),  # P(X < x)
    type == "<=" ~ ppois(x, lambda),  # P(X ≤ x)
    type == ">"  ~ ppois(x, lambda, lower.tail = FALSE),  # P(X > x)
    type == ">=" ~ ppois(x - 1, lambda, lower.tail = FALSE)  # P(X ≥ x)
  )
  return(tibble(Type = type, Probability = prob))
}
pois_probabilities(3, lambda = 5, type = ">=")
##beta function

beta_probabilities <- function(x, alpha, beta, type = "<=") {
    prob <- case_when(
      type == "="  ~ 0,  #always = 0 for continuous distribution
      type == "!=" ~ 1, #always = 1 for continuous distribution
      type == "<"  ~ pbeta(x, alpha, beta),  # P(X < x)
      type == "<=" ~ pbeta(x, alpha, beta),  # P(X ≤ x) (same for continuous)
      type == ">"  ~ pbeta(x, alpha, beta, lower.tail = FALSE),  # P(X > x)
      type == ">=" ~ pbeta(x, alpha, beta, lower.tail = FALSE)  # P(X ≥ x)
    )
  
  return(tibble(Type = type, Probability = prob))
}

beta_probabilities(0.3, alpha = 2, beta = 5)
