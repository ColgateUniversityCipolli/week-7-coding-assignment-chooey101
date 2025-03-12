poisson_probabilities <- function(x, lambda) {
  prob_x <- dpois(x, lambda)
  prob_not_x <- 1 - prob_x
  prob_less_than_x <- ppois(x - 1, lambda)
  prob_less_than_equal_x <- ppois(x, lambda)
  prob_greater_than_x <- ppois(x, lambda, lower.tail = FALSE) #lower tail is true for less than and false for greater than
  prob_greater_than_equal_x <- ppois(x - 1, lambda, lower.tail = FALSE)
  
  return(data.frame(
    Condition = c("P(X = x)", "P(X ≠ x)", "P(X < x)", "P(X ≤ x)", "P(X > x)", "P(X ≥ x)"),
    Probability = c(prob_x, prob_not_x, prob_less_than_x, prob_less_than_equal_x, prob_greater_than_x, prob_greater_than_equal_x)
  ))
}