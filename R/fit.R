fit.ols <- function(X, y) {
  # For using the efficient matrix algebra in the SparseM package we 
  # need X to be in matrix.csr form
  X.csc <- new("matrix.csc", 
               ra = X@x,
               ja = X@i + 1L,
               ia = X@p + 1L,
               dimension = X@Dim)
  X.csr <- as.matrix.csr(X.csc)
  ols.fit <- slm.fit(x = X.csr, y = y, tmpmax = 100 * nrow(X.csr))
  return(ols.fit)
}

# Weighted least squares regression from sparse design matrix in Matrix form (X), 
# response vector (y), vector of weights (w)
# https://stackoverflow.com/questions/17375056/r-sparse-matrix-conversion
fit.wols <- function(X, y, w) {
  X.csc <- new("matrix.csc", ra = X@x,
               ja = X@i + 1L,
               ia = X@p + 1L,
               dimension = X@Dim)
  X.csr <- as.matrix.csr(X.csc)
  wols.fit <- slm.wfit(x = X.csr, y = y, weights = w,tmpmax = 100 * nrow(X))
  return(wols.fit)
}

fit.lasso <- function(X, y, w, lambda = NULL){
  # Code to run LASSO using cross-validation to choose lambda
  lasso.cv <- cv.glmnet(X, y,
                        weights = w,
                        alpha = 1,
                        standardize = FALSE,
                        lambda = lambda)
  return(lasso.cv)
}

fit.ridge <- function(X, y, w, lambda = NULL){
  # Fit ridge regression using cross-validation to choose lambda
  ridge.cv <- cv.glmnet(X, y,
                        weights = w,
                        alpha = 0,
                        standardize = FALSE,
                        lambda = lambda)
  return(ridge.cv)
}

fit.pois <- function(X, y, w, lambdas = NULL) {
  pois <- cv.glmnet(X, y,
                    offset = w,
                    family = "poisson",
                    lambda = lambdas,
                    maxit = 200000)
  return(pois)
}

fit.ols.svd <- function(X, y) {
  duv <- svd(t(X) %*% X)
  return(duv$v %*% diag(1 / duv$d) %*% t(duv$u) %*% t(X) %*% y)
}

fit.wols.svd <- function(X, y, w) {
  duv <- svd(t(X) %*% (w * X))
  return(duv$v %*% diag(1 / duv$d) %*% t(duv$u) %*% t(X) %*% (w * y))
}

fit.ridge.svd <- function(X, y, lambdas) {
  best_lambda <- NULL
  betas <- NULL
  best_mse <- NULL
  
  for (lambda in lambdas) {
    duv <- svd(t(X) %*% X + lambda * diag(ncol(X)))
    beta <- duv$v %*% diag(1 / duv$d) %*% t(duv$u) %*% t(X) %*% y
    
    mse <- mean((y - X %*% beta)^2)
    
    if (is.null(best_mse) || mse < best_mse) {
      best_lambda <- lambda
      betas <- beta
      best_mse <- mse
    }
  }
  
  return(list(beta = betas, lambda = best_lambda, mse = best_mse))
}

# Linear model to predict the ridge coefficients using box score statistics
pseudo_bayes_model <- function(x, y, pure_boxscore = TRUE) {
  if (pure_boxscore == TRUE) {
    model <- lm(y ~ goals + assists + takeaways + giveaways, data = x)
  } else {
    model <- lm(y ~ total_xg + playmaking_score + takeaways + giveaways, data = x)
  }
  
  predicted_values <- predict(model, newdata = x)
  
  return(list(model = model, predicted_values = predicted_values))
}