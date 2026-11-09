f <- function(x) {
  x^2
}
x <- 2
h <- 0.1
backward <- (f(x-h)- f(x)) / h

backward