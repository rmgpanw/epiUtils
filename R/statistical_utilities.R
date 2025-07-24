#' Calculate Standard Error for Proportions
#'
#' Calculates the standard error for a proportion using the formula:
#' SE = 1 / (sqrt(2 * p * (1 - p)) * N)
#'
#' This is commonly used in epidemiological studies to estimate the precision
#' of prevalence estimates or risk ratios.
#'
#' @param p Numeric vector of proportions (between 0 and 1)
#' @param n Numeric vector of sample sizes (positive values)
#'
#' @return Numeric vector of standard errors
#'
#' @details
#' The standard error quantifies the uncertainty in a proportion estimate.
#' Smaller standard errors indicate more precise estimates. The formula
#' assumes a binomial distribution for the underlying count data.
#'
#' **Requirements:**
#' - `p` values must be between 0 and 1 (exclusive for this calculation)
#' - `n` values must be positive
#' - `p` and `n` must have the same length or one must be length 1
#'
#' **Edge cases:**
#' - When p = 0 or p = 1, the standard error is undefined (returns NA with warning)
#' - Very small or large p values may produce unstable results
#'
#' @examples
#' # Single proportion
#' calculate_se_proportion(p = 0.1, n = 100)
#'
#' # Multiple proportions
#' calculate_se_proportion(p = c(0.05, 0.1, 0.2), n = c(50, 100, 200))
#'
#' # Same sample size for multiple proportions
#' calculate_se_proportion(p = c(0.05, 0.1, 0.2), n = 100)
#'
#' @seealso [calculate_z_score()], [calculate_f_statistic()]
#'
#' @export
calculate_se_proportion <- function(p, n) {
  # Validate inputs
  validate(p, arg_name = "p", allow_zero = FALSE, allow_one = FALSE)
  validate_sample_size(n, arg_name = "n", min_size = 1)
  
  # Check length compatibility
  if (length(p) != length(n) && length(p) != 1 && length(n) != 1) {
    cli::cli_abort(c(
      "x" = "Arguments {.arg p} and {.arg n} must have the same length or one must be length 1",
      "i" = "You provided {.arg p} with length {length(p)} and {.arg n} with length {length(n)}"
    ))
  }
  
  # Calculate standard error
  # SE = 1 / (sqrt(2 * p * (1 - p)) * N)
  se <- 1 / (sqrt(2 * p * (1 - p)) * n)
  
  # Check for any issues
  if (any(!is.finite(se))) {
    warning_indices <- which(!is.finite(se))
    cli::cli_warn(c(
      "!" = "Standard error calculation produced non-finite values at position{?s}: {warning_indices}",
      "i" = "This typically occurs when p is 0 or 1, or when sample sizes are very small"
    ))
  }
  
  return(se)
}

#' Calculate Z-Score from Effect Size and Standard Error
#'
#' Calculates the Z-score (standardized effect size) using the formula:
#' Z = beta / SE
#'
#' This is commonly used in statistical testing to assess the significance
#' of an effect relative to its uncertainty.
#'
#' @param beta Numeric vector of effect sizes (e.g., log odds ratios, regression coefficients)
#' @param se Numeric vector of standard errors (positive values)
#'
#' @return Numeric vector of Z-scores
#'
#' @details
#' The Z-score represents how many standard errors the effect size is away
#' from zero. Larger absolute Z-scores indicate stronger evidence against
#' the null hypothesis of no effect.
#'
#' **Requirements:**
#' - `se` values must be positive
#' - `beta` and `se` must have the same length or one must be length 1
#'
#' @examples
#' # Single effect size
#' calculate_z_score(beta = 0.5, se = 0.2)
#'
#' # Multiple effect sizes
#' calculate_z_score(beta = c(-0.3, 0.5, 1.2), se = c(0.15, 0.2, 0.4))
#'
#' @seealso [calculate_z_score_from_p()], [calculate_se_from_z()]
#'
#' @export
calculate_z_score <- function(beta, se) {
  # Validate inputs
  validate.default(beta, arg_name = "beta")
  validate.default(se, arg_name = "se", allow_negative = FALSE, allow_zero = FALSE)
  
  # Check length compatibility
  if (length(beta) != length(se) && length(beta) != 1 && length(se) != 1) {
    cli::cli_abort(c(
      "x" = "Arguments {.arg beta} and {.arg se} must have the same length or one must be length 1",
      "i" = "You provided {.arg beta} with length {length(beta)} and {.arg se} with length {length(se)}"
    ))
  }
  
  # Calculate Z-score
  z <- beta / se
  
  return(z)
}

#' Calculate Z-Score from P-Value
#'
#' Calculates the Z-score corresponding to a two-tailed p-value using:
#' Z = qnorm(1 - (p / 2))
#'
#' Note: This returns the absolute value of the Z-score. For directional
#' effects, use the sign from the original effect size.
#'
#' @param p Numeric vector of two-tailed p-values (between 0 and 1)
#'
#' @return Numeric vector of absolute Z-scores
#'
#' @details
#' This function converts p-values back to their corresponding Z-scores
#' assuming a standard normal distribution. The result is always positive
#' (absolute value) since p-values don't contain directional information.
#'
#' **Requirements:**
#' - `p` values must be between 0 and 1 (exclusive for meaningful Z-scores)
#'
#' **Edge cases:**
#' - p = 0 returns Inf (infinite Z-score)
#' - p = 1 returns 0 (no evidence against null)
#' - Very small p-values may produce very large Z-scores
#'
#' @examples
#' # Single p-value
#' calculate_z_score_from_p(p = 0.05)
#'
#' # Multiple p-values
#' calculate_z_score_from_p(p = c(0.001, 0.01, 0.05, 0.1))
#'
#' @seealso [calculate_z_score()]
#'
#' @export
calculate_z_score_from_p <- function(p) {
  # Validate inputs
  validate(p, arg_name = "p", allow_zero = TRUE, allow_one = TRUE)
  
  # Calculate absolute Z-score
  # Z = qnorm(1 - (p / 2))
  z <- stats::qnorm(1 - (p / 2))
  
  # Check for any issues
  if (any(!is.finite(z))) {
    warning_indices <- which(!is.finite(z))
    cli::cli_warn(c(
      "!" = "Z-score calculation produced non-finite values at position{?s}: {warning_indices}",
      "i" = "This typically occurs when p-values are 0 (infinite Z) or very close to 1"
    ))
  }
  
  return(z)
}

#' Calculate Standard Error from Z-Score and Effect Size
#'
#' Calculates the standard error using the formula:
#' SE = beta / Z
#'
#' This is useful when you have an effect size and its corresponding Z-score
#' but need to calculate the standard error.
#'
#' @param beta Numeric vector of effect sizes
#' @param z Numeric vector of Z-scores (non-zero values)
#'
#' @return Numeric vector of standard errors
#'
#' @details
#' This function rearranges the standard Z-score formula (Z = beta / SE)
#' to solve for the standard error. This is useful for back-calculating
#' standard errors from published results.
#'
#' **Requirements:**
#' - `z` values must be non-zero
#' - `beta` and `z` must have the same length or one must be length 1
#'
#' @examples
#' # Single values
#' calculate_se_from_z(beta = 0.5, z = 2.5)
#'
#' # Multiple values
#' calculate_se_from_z(beta = c(0.3, 0.5, 0.8), z = c(1.5, 2.5, 3.2))
#'
#' @seealso [calculate_z_score()], [calculate_se_proportion()]
#'
#' @export
calculate_se_from_z <- function(beta, z) {
  # Validate inputs
  validate.default(beta, arg_name = "beta")
  validate.default(z, arg_name = "z", allow_zero = FALSE)
  
  # Check length compatibility
  if (length(beta) != length(z) && length(beta) != 1 && length(z) != 1) {
    cli::cli_abort(c(
      "x" = "Arguments {.arg beta} and {.arg z} must have the same length or one must be length 1",
      "i" = "You provided {.arg beta} with length {length(beta)} and {.arg z} with length {length(z)}"
    ))
  }
  
  # Calculate standard error
  se <- beta / z
  
  # Check for negative standard errors (which indicate sign inconsistency)
  if (any(se < 0)) {
    negative_indices <- which(se < 0)
    cli::cli_warn(c(
      "!" = "Calculated negative standard error{?s} at position{?s}: {negative_indices}",
      "i" = "This suggests {.arg beta} and {.arg z} have inconsistent signs",
      "i" = "Standard errors should be positive - consider using absolute values"
    ))
  }
  
  return(abs(se))  # Return absolute value to ensure positive standard errors
}

#' Calculate F-Statistic
#'
#' Calculates the F-statistic using the formula:
#' F = (beta / SE)^2
#'
#' This is equivalent to Z^2 and is commonly used in ANOVA and regression
#' analysis to test the significance of effects.
#'
#' @param beta Numeric vector of effect sizes
#' @param se Numeric vector of standard errors (positive values)
#'
#' @return Numeric vector of F-statistics
#'
#' @details
#' The F-statistic is the square of the Z-score and follows an F-distribution
#' with 1 degree of freedom in the numerator. It's always non-negative and
#' larger values indicate stronger evidence against the null hypothesis.
#'
#' **Requirements:**
#' - `se` values must be positive
#' - `beta` and `se` must have the same length or one must be length 1
#'
#' **Relationship to other statistics:**
#' - F = Z^2
#' - F follows an F(1, df) distribution for simple effects
#' - Under the null hypothesis, F follows a chi-squared distribution with 1 df
#'
#' @examples
#' # Single effect size
#' calculate_f_statistic(beta = 0.5, se = 0.2)
#'
#' # Multiple effect sizes
#' calculate_f_statistic(beta = c(-0.3, 0.5, 1.2), se = c(0.15, 0.2, 0.4))
#'
#' # Verify relationship with Z-score
#' beta <- 0.5
#' se <- 0.2
#' z <- calculate_z_score(beta, se)
#' f <- calculate_f_statistic(beta, se)
#' all.equal(f, z^2)  # Should be TRUE
#'
#' @seealso [calculate_z_score()]
#'
#' @export
calculate_f_statistic <- function(beta, se) {
  # Validate inputs
  validate.default(beta, arg_name = "beta")
  validate.default(se, arg_name = "se", allow_negative = FALSE, allow_zero = FALSE)
  
  # Check length compatibility
  if (length(beta) != length(se) && length(beta) != 1 && length(se) != 1) {
    cli::cli_abort(c(
      "x" = "Arguments {.arg beta} and {.arg se} must have the same length or one must be length 1",
      "i" = "You provided {.arg beta} with length {length(beta)} and {.arg se} with length {length(se)}"
    ))
  }
  
  # Calculate F-statistic
  # F = (beta / SE)^2
  f_stat <- (beta / se)^2
  
  return(f_stat)
}