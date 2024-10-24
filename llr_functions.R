llr <- function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}

compute_f_hat <- function(z, x, y, omega) {
  weights = make_weight_matrix(z, x, omega)
  X = make_predictor_matrix(x)
  f_hat = c(1, z) %*% 
    solve(t(sweep(X, 1, weights, FUN = "*")) %*% sweep(X, 1, weights, FUN = "*")) %*% 
    t(sweep(X, 1, weights, FUN = "*")) %*% (weights * y)
  return(f_hat)
}

make_weight_matrix <- function(z, x, omega){
  W = function(r){
    ifelse(abs(r) < 1, (1 - abs(r)^3)^3, 0)
  }
  distances = abs(x - z) / omega
  weights = W(distances)
  Wz = diag(weights)
  return(weights)
}

make_predictor_matrix <- function(x){
  n = length(x)
  X = cbind(rep(1, n), x)
  return(X)
}
