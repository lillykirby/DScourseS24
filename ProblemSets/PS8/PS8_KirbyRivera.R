rm(list = ls())

#Set up workspace:
#install.packages("nloptr")
library(nloptr)
library(modelsummary)
library(xtable)

#4. Set up matrix with column of 1's:
set.seed(100)
X <- matrix(data = rnorm(100000 * 10), nrow = 100000, ncol = 10)
X[, 1] <- 1

#Error term:
eps <- rnorm(100000, mean=0, sd=0.5)

#Beta vector:
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
true_beta <- beta

mod4 <- data.frame(true_beta)
print(xtable(mod4, caption = "True Parameters", digits = 5), caption.placement = "top")

#Generate Y:
Y <- X %*% beta + eps

#5. Compute beta hat OLS:
beta_hat_ols <- solve(crossprod(X)) %*% crossprod(X,Y)

mod5 <- data.frame(beta_hat_ols)
print(xtable(mod5, caption = "Matrix Algebra", digits = 5), caption.placement = "top")

#6. Gradient descent:
#Set up step size and iterations:
alpha <- 0.0000003
maxiter <- 500000

#Objective function:
objfun <- function(beta, y, X) {
  return(sum((Y - X %*% beta)^2))
}

#Gradient:
gradient <- function(beta, y, X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}

#Initial values:
beta <- runif(dim(X)[2])

#Beta vector for all steps:
beta.All <- matrix("numeric", length(beta), maxiter)

#Gradient descent to find minimum:
iter  <- 1
beta0 <- 0*beta
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0, Y, X)
  beta.All[,iter] <- beta
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}

print(iter)
print(paste("The minimum of f(beta, Y, X) is ", beta, sep = ""))
gd_beta <- beta

mod6 <- data.frame(gd_beta)
print(xtable(mod6, caption = "Gradient Descent", digits = 5), caption.placement = "top")

#7. L-BFGS algorithm:
#Initial values:
beta0 <- runif(dim(X)[2])

#Algorithm parameters:
options <- list("algorithm" = "NLOPT_LD_LBFGS", "xtol_rel"=1.0e-6, "maxeval" = 1e3)

#Results:
result <- nloptr( x0=beta0, eval_f=objfun, eval_grad_f=gradient, opts=options, y=Y, X=X)
print(result)
l_bfgs_ols <- result$solution

mod7a <- data.frame(l_bfgs_ols)
print(xtable(mod7a, caption = "L-BFGS OLS", digits = 5), caption.placement = "top")

#Nelder-Mead algorithm:
options <- list("algorithm" = "NLOPT_LN_NELDERMEAD", "xtol_rel"=1.0e-6, "maxeval" = 1e3)
result <- nloptr( x0=beta0, eval_f=objfun, eval_grad_f=gradient, opts=options, y=Y, X=X)
print(result)
nm_ols <- result$solution

mod7b <- data.frame(nm_ols)
print(xtable(mod7b, caption = "Nelder-Mead OLS", digits = 5), caption.placement = "top")

#8. Beta hat MLE:
#Objective function:
objfun  <- function(theta, y, X) {
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((Y-X%*%beta)/sig)^2) ) 
  return (loglike)
}

#Gradient:
gradient <- function(theta, y, X) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta[1:(length(theta)-1)]
  sig <- theta[length(theta)]
  grad[1:(length(theta)-1)] <- -t(X) %*% (Y - X%*%beta)/(sig^2)
  grad[length(theta)] <- dim(X)[1]/sig - crossprod(Y - X%*%beta)/(sig^3)
  return ( grad )
}

#Initial values:
theta0 <- runif(dim(X)[2]+1)

#Algorithm parameters:
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e4)

#Optimize:
result <- nloptr( x0=theta0, eval_f=objfun, eval_grad_f=gradient, opts=options, y=Y, X=X)
print(result)
betahat  <- result$solution[1:(length(result$solution)-1)]
sigmahat <- result$solution[length(result$solution)]

mod8 <- data.frame(betahat)
print(xtable(mod8, caption = "L-BFGS MLE", digits = 5), caption.placement = "top")

#9. Beta hat OLS using lm():
lm <- lm(Y ~ X - 1)
modelsummary(lm)

output <- modelsummary(lm, stars = TRUE, output = "mod.tex")
print(output)
