#' @title Wald-type Test for Comparing Mean TIRs Across Groups
#' @description
#' Performs a Wald-type hypothesis test for the null hypothesis that the mean Time-in-Range (TIR)
#' values are equal across multiple groups, using bootstrap estimates.
#'
#' The null hypothesis is:
#' \deqn{H_0: \mu_{W,1} = \mu_{W,2} = \ldots = \mu_{W,K}}
#'
#' The test statistic is constructed as:
#' \deqn{\widehat{D}^\top \widehat{\Sigma}^{-1} \widehat{D}}
#' where \eqn{\widehat{D}} is the vector of differences in TIR estimates relative to group 1,
#' and \eqn{\widehat{\Sigma}} is the covariance matrix of the bootstrapped differences.
#'
#' @param estimates A numeric vector of TIR estimates for each group (length \eqn{K}).
#' @param boot_TIR_list A list of numeric vectors of bootstrap estimates for each group (each of length \eqn{B}).
#'
#' @return A list with components:
#' \describe{
#'   \item{statistic}{The Wald test statistic (numeric).}
#'   \item{df}{Degrees of freedom (K - 1).}
#'   \item{p.value}{P-value based on the chi-squared distribution.}
#' }
#'
#' @importFrom stats cov pchisq
#' @importFrom MASS ginv
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' est <- c(0.75, 0.68, 0.72)
#' boot_TIR <- list(
#'   rnorm(100, 0.75, 0.02),
#'   rnorm(100, 0.68, 0.03),
#'   rnorm(100, 0.72, 0.025)
#' )
#' wald_test_TIR(est, boot_TIR)
#' }
#' @export
wald_test_TIR <- function(estimates, boot_TIR_list) {
  K <- length(estimates)
  if (K < 2) return(NULL)

  # Step 1: truncate to the minimum number of replicates available across all groups
  B_vec <- sapply(boot_TIR_list, length)
  B <- min(B_vec)

  if (B < 5) {
    warning("Too few aligned bootstrap replicates for hypothesis testing.")
    return(list(statistic = NA, df = K - 1, p.value = NA))
  }

  # Step 2: align replicates
  boot_TIR_list_trimmed <- lapply(boot_TIR_list, function(x) x[seq_len(B)])

  # Step 3: compute differences and test
  mu_diff <- estimates[-1] - estimates[1]
  boot_diff_mat <- matrix(NA, nrow = B, ncol = K - 1)

  for (b in 1:B) {
    boot_estimates <- sapply(boot_TIR_list_trimmed, function(x) x[b])
    boot_diff_mat[b, ] <- boot_estimates[-1] - boot_estimates[1]
  }

  Sigma_D_hat <- cov(boot_diff_mat)

  inv_Sigma <- tryCatch(solve(Sigma_D_hat), error = function(e) MASS::ginv(Sigma_D_hat))
  stat <- as.numeric(t(mu_diff) %*% inv_Sigma %*% mu_diff)
  p_val <- 1 - stats::pchisq(stat, df = K - 1)

  list(statistic = stat, df = K - 1, p.value = p_val)
}
