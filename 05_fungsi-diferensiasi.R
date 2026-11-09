forward_diference <- function (f, x, h){
  (f(x+h)-f(x)) / h
}

f <- function(x) {
  x^2
}
forward_diference(f, 2, 0.1)

backward_diference <- function (f, x, h){
  (f(x)-f(x - h)) / h
}

f <- function(x) {
  x^2
}
backward_diference(f, 2, 0.1)


central_diference <- function (f, x, h){
  (f(x + h)-f(x - h)) / (2*h)
}

f <- function(x) {
  x^2
}
central_diference(f, 2, 0.1)