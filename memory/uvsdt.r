# nr_s1 <- c(40, 45, 25, 50, 120, 170)
# nr_s2 <- c(240, 70, 20, 30, 50, 40)
# nr_s1 and nr_s2 are response frequency vectors for S1 and S2 stimuli, 
# ordered from highest confidence S2 to highest confidence S1 responses.
# add_constant = TRUE adds a small value to the response frequency vectors.


#'# Function for model fitting
fit_uvsdt_mle <- function(nr_s1, nr_s2, add_constant = FALSE) {
  
  # correction against extreme estimates 
  if (add_constant) {
    nr_s1 <- nr_s1 + (1 / length(nr_s1))
    nr_s2 <- nr_s2 + (1 / length(nr_s2))
  }
  
  # data description  
  rating_far <- cumsum(nr_s1) / sum(nr_s1)
  rating_hr <-  cumsum(nr_s2) / sum(nr_s2)
  n_ratings <-  length(nr_s1) / 2
  
  # initial guess for parameter values
  mu <- qnorm(rating_hr[n_ratings]) - qnorm(rating_far[n_ratings])
  sigma <- 1.5
  cri <-  -qnorm(rating_far)
  cri <-   cri[1:(2 * n_ratings - 1)]
  guess <- c(mu, sigma, cri)
  
  # model fit
  fit <- suppressWarnings(optim(uvsdt_logL, 
                                par = guess, 
                                inputs = list("n_ratings" = n_ratings, "nr_s1" = nr_s1, "nr_s2" = nr_s2), 
                                gr = NULL, method = "BFGS", control = list("maxit" = 10000)))
  
  # outputs
  mu <-    fit$par[1]
  sigma <- fit$par[2]
  da <-    mu / sqrt((1 + sigma^2) / 2)
  logL <- -fit$value
  cri <- data.frame(matrix(vector(), 0, 2 * n_ratings - 1))
  for (i in 1:(2 * n_ratings - 1)) {
    cri[1, i] <- fit$par[2 * n_ratings + 2 - i]
  }
  est <- data.frame(mu = mu, sigma = sigma, da = da, cri = cri, logL = logL)
  return(est)
  
}


#'# Likelihood function
uvsdt_logL <- function(x, inputs) {
  
  # target parameters
  mu <-    x[1]
  sigma <- x[2]
  cri <-   x[3:(2 * inputs$n_ratings + 1)]
  
  # empirical data
  nr_s1 <- inputs$nr_s1
  nr_s2 <- inputs$nr_s2
  n_ratings <- inputs$n_ratings
  
  # model predictions
  pred_far <- c(0, pnorm(0 - cri, 0, 1),            1)
  pred_hr <-  c(0, pnorm((mu - cri) / sigma, 0, 1), 1)
  pred_nr_s1 <- sum(nr_s1) * diff(pred_far)
  pred_nr_s2 <- sum(nr_s2) * diff(pred_hr)
  
  # log likelihood
  logL <- sum(nr_s1 * log(pred_nr_s1 / sum(nr_s1)) + nr_s2 * log(pred_nr_s2 / sum(nr_s2)))
  if (is.nan(logL)) {
    logL <- -Inf
  }
  logL <- -logL
  return(logL)
  
}