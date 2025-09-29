# uvsdt parameters
mu    <- 2.0
sigma <- 1.5

# da
da <- mu / sqrt(1 + sigma^2)
da

# auc
auc <- pnorm(mu / sqrt(1 + sigma^2))
auc

da * sqrt(2)
pnorm(1.568929 / sqrt(2))