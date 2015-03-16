nnls_test <- function(){
  A = c(1,2,3,4)
  rbind(A, c(3,4,5,6), c(5,6,7,8))
  b = c(-10, 13, 82)
  nnls(A, b)
}