fit_meta_d_bal <- function(nr_s1,
                           nr_s2,
                           s = 1,
                           add_constant = FALSE) {
  
  if (add_constant) {
    nr_s1 <- nr_s1 + (1 / length(nr_s1))
    nr_s2 <- nr_s2 + (1 / length(nr_s2))
  }
  
  n_ratings <- length(nr_s1) / 2
  
  s1_hr <- sum(nr_s1[1:n_ratings]) / sum(nr_s1)
  s1_fa <- sum(nr_s2[1:n_ratings]) / sum(nr_s2)
  
  d_prime <- (1 / s) * qnorm(s1_hr) - qnorm(s1_fa)
  t1c1 <- (-1 / (1 + s)) * (qnorm(s1_hr) + qnorm(s1_fa))
  
  Hp <- nr_s1[[1]] / sum(nr_s1[1:n_ratings])
  Hm <- nr_s2[[n_ratings * 2]] / sum(nr_s2[(n_ratings + 1):(n_ratings * 2)])
  Fm <- nr_s1[[n_ratings * 2]] / sum(nr_s1[(n_ratings + 1):(n_ratings * 2)])
  Fp <- nr_s2[[1]] / sum(nr_s2[1:n_ratings])
  
  theta <- -qnorm(s1_fa)
  theta_prime <- theta / d_prime
  x0 <- c(theta, d_prime)
  
  ep <- nleqslv::nleqslv(x0,
                         fit_metad_plus, method = "Newton",
                         th = theta_prime,
                         hp = Hp, fp = Fp,
                         global = "pwldog",
                         control = list(delta = "cauchy", ftol = 1e-06), xscalm = "auto")
  
  meta_d_plus <- ep$x[[2]]
  x0 <- c(theta_prime, d_prime)
  
  em <- nleqslv::nleqslv(x0, fit_metad_minus,
                         method = "Newton", th = theta_prime,
                         hm = Hm, fm = Fm, global = "pwldog",
                         control = list(delta = "cauchy", ftol = 1e-06),
                         xscalm = "auto")
  meta_d_neg <- em$x[[2]]
  
  htp <- 1 - pnorm(theta_prime * meta_d_plus, meta_d_plus, s)
  ftp <- 1 - pnorm(theta_prime * meta_d_plus, 0, 1)
  htm <- 1 - pnorm(theta_prime * meta_d_neg, meta_d_neg, s)
  ftm <- 1 - pnorm(theta_prime * meta_d_neg, 0, 1)
  
  if (any(c(htp, ftp, htm, ftm) > .95) || any(c(htp, ftp, htm, ftm) < .05)) {
    stable <- 0
  } else {
    stable <- 1
  }
  
  r <- (s1_hr + s1_fa) / 2
  meta_d_bal <- r * meta_d_plus + (1 - r) * meta_d_neg
  
  data.frame(d_prime = d_prime,
             meta_d_bal = meta_d_bal,
             meta_d_plus = meta_d_plus,
             meta_d_neg = meta_d_neg,
             meta_d_ratio = meta_d_bal / d_prime,
             meta_d_diff = meta_d_bal - d_prime,
             stable = stable,
             htp = htp,
             ftp = ftp,
             htm = htm,
             ftm = ftm,
             t1c1 = t1c1)
}

#' Internal function for fitting meta_d_plus
#'
#' @param x0 Starting parameters.
#' @param th Theta.
#' @param hp Hit rate for positive responses
#' @param fp False positive rate for positive responses.
#' @keywords internal

fit_metad_plus <- function(x0, th, hp, fp) {
  y1 <- (1 - pnorm(x0[[1]], x0[[2]], 1)) /
    (1 - pnorm(th * x0[[2]], x0[[2]], 1)) - hp
  y2 <- (1 - pnorm(x0[[1]], 0, 1)) / (1 - pnorm(th * x0[[2]], 0, 1)) - fp
  c(y1, y2)
}


#' Internal function for fitting meta_d_minus
#'
#' @param x0 Starting parameters.
#' @param th Theta
#' @param hm Hit rate for negative responses
#' @param fm False positive rate for negative responses
#' @keywords internal
fit_metad_minus <- function(x0, th, hm, fm) {
  y1 <- pnorm(x0[[1]], 0, 1) / pnorm(th * x0[[2]], 0, 1) - hm
  y2 <- pnorm(x0[[1]], x0[[2]], 1) / pnorm(th * x0[[2]], x0[[2]], 1) - fm
  c(y1, y2)
}

