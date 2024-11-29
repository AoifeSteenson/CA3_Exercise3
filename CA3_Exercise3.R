#1
df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))


#2

nll_lm <- function(par, data) {
  
  
  y <- data$y
  X <- as.matrix(data[, par + 1])
  X <- cbind(1, X)
  
  
  beta <- solve(crossprod(X), crossprod(X,y)) 
  
  y_hat <- X %*% beta
  
  residuals <- y - y_hat
  
  theta <- log(prod(dnorm(residuals, mean = 0, sd = sqrt(var(y)))))
  
  return(list(residuals = as.vector(residuals), intercept = beta[1], slopes = beta[-1], theta = theta, sd = sqrt(var(y))))
}


par <- c(1, 2, 3)
nll_lm(par, df)

#3

nll_lm <- function(par, data, ...) {
  
  
  y <- data$y
  X <- as.matrix(data[, -1])
  X <- cbind(1, X)
  
  
  beta <- as.matrix(par)
  
  y_hat <- X %*% beta
  
  residuals <- y - y_hat
  
  theta <- -log(prod(dnorm(residuals, mean = 0, sd = sqrt(var(y)))))
  
  return(theta)
}

par <- c(37.1055, -0.0009370091, -0.0311565508, -3.8008905826)


optim(par, nll_lm, data = df, hessian = TRUE, lower = -Inf, upper = Inf, method = "BFGS")


#5

optim(par, nll_lm, data = df, hessian = TRUE, lower = -Inf, upper = Inf, method = "BFGS")


y <- df$y
X <- as.matrix(df[, -1])
X <- cbind(1, X)


beta <- solve(crossprod(X), crossprod(X,y)) 

beta


#6

results <- optim(par, nll_lm, data = df, hessian = TRUE, lower = -Inf, upper = Inf, method = "BFGS")
par <- results$par


nll_lm <- function(par, data, ...) {
  
  
  y <- data$y
  X <- as.matrix(data[, -1])
  X <- cbind(1, X)
  
  
  beta <- as.matrix(par)
  
  y_hat <- X %*% beta
  
  residuals <- y - y_hat
  
  theta <- -log(prod(dnorm(residuals, mean = 0, sd = sqrt(var(y)))))
  
  return(sqrt(var(y)))
}

nll_lm(par, df)


y <- df$y
X <- as.matrix(df[, -1])
X <- cbind(1, X)


beta <- solve(crossprod(X), crossprod(X,y)) 

y_hat <- X %*% beta

residuals <- y - y_hat

sqrt(var(residuals))


#8 

results <- optim(c(0,0,0,0), nll_lm, data = df, hessian = TRUE, lower = -Inf, upper = Inf, method = "BFGS")

nll_lm <- function(par, data, ...) {
  
  
  y <- data$y
  X <- as.matrix(data[, -1])
  X <- cbind(1, X)
  
  
  beta <- as.matrix(par)
  
  y_hat <- X %*% beta
  
  residuals <- y - y_hat
  
  theta <- -log(prod(dnorm(residuals, mean = 0, sd = sqrt(var(y)))))
  
  return(list(residuals = as.vector(residuals), intercept = beta[1], slopes = beta[-1], theta = theta, sd = sqrt(var(y))))
}


residauls <- nll_lm(results$par, df)$residuals

sqrt(var(residuals))/sqrt(length(df$y))results <- optim(c(0,0,0,0), nll_lm, data = df, hessian = TRUE, lower = -Inf, upper = Inf, method = "BFGS")

nll_lm <- function(par, data, ...) {
  
  
  y <- data$y
  X <- as.matrix(data[, -1])
  X <- cbind(1, X)
  
  
  beta <- as.matrix(par)
  
  y_hat <- X %*% beta
  
  residuals <- y - y_hat
  
  theta <- -log(prod(dnorm(residuals, mean = 0, sd = sqrt(var(y)))))
  
  return(list(residuals = as.vector(residuals), intercept = beta[1], slopes = beta[-1], theta = theta, sd = sqrt(var(y))))
}


residauls <- nll_lm(results$par, df)$residuals

sqrt(var(residuals))/sqrt(length(df$y))