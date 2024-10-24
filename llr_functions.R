llr <- function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}

compute_f_hat <- function(z, x, y, omega) {
  Wz = make_weight_matrix(z, x, omega)
  X = make_predictor_matrix(x)
  f_hat = c(1, z) %*% solve(t(X) %*% Wz %*% X) %*% t(X) %*% Wz %*% y
  return(f_hat)
}

make_weight_matrix <- function(z, x, omega){
  W = function(r){
    ifelse(abs(r) < 1, (1 - abs(r)^3)^3, 0)
  }
  distances = abs(x - z) / omega
  weights = W(distances)
  Wz = diag(weights)
  return(Wz)
}

make_predictor_matrix <- function(x){
  n = length(x)
  X = cbind(rep(1, n), x)
  return(X)
}