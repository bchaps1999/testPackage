#' Estimate Beta Coefficients for Linear Regression
#'
#' This function estimates the beta coefficients in a linear regression model
#' y = X * beta + epsilon using ordinary least squares (OLS).
#'
#' @name estimate_beta
#' @param y A numeric vector of response values
#' @param X A numeric matrix of predictor variables
#'
#' @return A numeric vector of estimated beta coefficients
#'
#' @examples
#' X <- matrix(c(1, 1, 1, 2, 3, 4), nrow = 3)
#' y <- c(2, 3, 5)
#' estimate_beta(y, X)
#'
#' @export
estimate_beta <- function(y, X) {
  # Input validation
  if (!is.numeric(y) || !is.vector(y)) {
    stop("y must be a numeric vector")
  }
  
  if (!is.numeric(X) || !is.matrix(X)) {
    stop("X must be a numeric matrix")
  }
  
  if (length(y) != nrow(X)) {
    stop("Number of observations in y must equal the number of rows in X")
  }
  
  # Calculate beta_hat using the OLS formula: beta_hat = (X'X)^(-1)X'y
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  
  return(beta_hat)
}
