# Exercise 3

#1.


df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))
df


nll_lm <- function(par, data) {

  n <- nrow(data)
  sigma2 <- par[length(par)]^2
  beta <- par[-length(par)]

  X <- cbind(1, as.matrix(data[, -1]))
  y <- data$y

  epsilon <- y - X %*% beta

  log_likelihood <- sum(dnorm(epsilon, mean = 0, sd = sqrt(sigma2), log = TRUE))
  return(-log_likelihood)
}





initial_params <- c(mean(df$y), rep(0, 3), sd(df$y))

lower <- c(rep(-Inf, 4), 0)
upper <- c(rep(Inf, 4), Inf)

fit <- optim(
  par = initial_params,
  fn = nll_lm,
  data = df,
  method = "L-BFGS-B",
  lower = lower,
  upper = upper,
  hessian = TRUE
)

beta_hat <- fit$par[1:(length(fit$par) - 1)]
sigma_hat <- abs(fit$par[length(fit$par)])

beta_hat
sigma_hat






#5.

X <- cbind(1, as.matrix(df[, -1]))
y <- df$y
solve(t(X) %*% X) %*% t(X) %*% y


#6.

epsilon_matrix <- y - X %*% beta_matrix
sqrt(sum(epsilon_matrix^2) / (nrow(X) - ncol(X)))
```

#8.

initial_params <- c(mean(df$y), rep(0, 3), sd(df$y))

lower <- c(rep(-Inf, 4), 0)
upper <- c(rep(Inf, 4), Inf)

fit <- optim(
  par = initial_params,
  fn = nll_lm,
  data = df,
  method = "L-BFGS-B",
  lower = lower,
  upper = upper,
  hessian = TRUE
)

beta_hat <- fit$par[1:(length(fit$par) - 1)]
sigma_hat <- abs(fit$par[length(fit$par)])

hessian_matrix <- fit$hessian
solve_matrix <- solve(hessian_matrix)
se_beta_hat <- sqrt(diag(solve_matrix))

beta_hat
sigma_hat
se_beta_hat

