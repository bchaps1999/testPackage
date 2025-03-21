% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_beta.R
\name{estimate_beta}
\alias{estimate_beta}
\title{Estimate Beta Coefficients for Linear Regression}
\usage{
estimate_beta(y, X)
}
\arguments{
\item{y}{A numeric vector of response values}

\item{X}{A numeric matrix of predictor variables}
}
\value{
A numeric vector of estimated beta coefficients
}
\description{
This function estimates the beta coefficients in a linear regression model
y = X * beta + epsilon using ordinary least squares (OLS).
}
\examples{
X <- matrix(c(1, 1, 1, 2, 3, 4), nrow = 3)
y <- c(2, 3, 5)
estimate_beta(y, X)

}
