#' Validate Input Data
#'
#' S3 generic function for validating different types of input data used in
#' epidemiological calculations. Provides consistent validation with informative
#' error messages following the tidyverse style guide.
#'
#' @param x Object to validate
#' @param ... Additional arguments passed to methods
#'
#' @return Returns `x` invisibly if validation passes, otherwise throws an error
#'
#' @export
validate <- function(x, ...) {
  UseMethod("validate")
}

#' Validate Probability Values
#'
#' Validates that probability values are between 0 and 1 (inclusive).
#' Provides helpful error messages suggesting common issues.
#'
#' @param x Numeric vector of probability values
#' @param arg_name Character string naming the argument being validated
#' @param allow_zero Logical. If TRUE (default), allows probability values of 0
#' @param allow_one Logical. If TRUE (default), allows probability values of 1
#' @param ... Additional arguments (currently unused)
#'
#' @return Returns `x` invisibly if validation passes
#'
#' @details
#' This function checks that all values in `x` are between 0 and 1. If values
#' are outside this range, it provides helpful error messages suggesting
#' possible causes:
#' - Values > 1 might be percentages that need to be divided by 100
#' - Very small values (< 1e-10) might be -log10(p-values) that need conversion
#' - Very large values might be chi-squared statistics or similar
#'
#' @examples
#' # Valid probability values
#' validate(c(0.05, 0.1, 0.5), arg_name = "p_values")
#'
#' # This would throw an error:
#' # validate(c(1.5, 2.0), arg_name = "p_values")
#'
#' @export
validate.numeric <- function(x, arg_name = "x", allow_zero = TRUE, allow_one = TRUE, ...) {
  # Check for missing values
  if (any(is.na(x))) {
    cli::cli_abort(c(
      "x" = "Argument {.arg {arg_name}} contains missing values",
      "i" = "All probability values must be non-missing"
    ))
  }

  # Check for infinite values
  if (any(!is.finite(x))) {
    cli::cli_abort(c(
      "x" = "Argument {.arg {arg_name}} contains infinite values",
      "i" = "All probability values must be finite"
    ))
  }

  # Check range
  min_val <- min(x)
  max_val <- max(x)
  
  # Check lower bound
  if (min_val < 0) {
    cli::cli_abort(c(
      "x" = "Argument {.arg {arg_name}} contains negative values: {min_val}",
      "i" = "Probability values must be non-negative"
    ))
  }
  
  if (!allow_zero && min_val == 0) {
    cli::cli_abort(c(
      "x" = "Argument {.arg {arg_name}} contains zero values",
      "i" = "Zero probability values are not allowed in this context"
    ))
  }
  
  # Check upper bound with helpful suggestions
  if (max_val > 1) {
    # Suggest possible explanations
    suggestions <- c()
    
    if (all(x >= 1 & x <= 100)) {
      suggestions <- c(suggestions, "Values appear to be percentages - try dividing by 100")
    }
    
    if (any(x > 100)) {
      suggestions <- c(suggestions, "Very large values might be chi-squared statistics or test statistics")
    }
    
    cli::cli_abort(c(
      "x" = "Argument {.arg {arg_name}} contains values > 1: {max_val}",
      "i" = "Probability values must be between 0 and 1",
      if (length(suggestions) > 0) c("!" = suggestions)
    ))
  }
  
  if (!allow_one && max_val == 1) {
    cli::cli_abort(c(
      "x" = "Argument {.arg {arg_name}} contains values equal to 1",
      "i" = "Probability values of exactly 1 are not allowed in this context"
    ))
  }
  
  # Check for very small values that might be -log10(p-values)
  if (any(x > 0 & x < 1e-10)) {
    small_vals <- x[x > 0 & x < 1e-10]
    cli::cli_warn(c(
      "!" = "Argument {.arg {arg_name}} contains very small values: {min(small_vals)}",
      "i" = "These might be -log10(p-values) - if so, convert using 10^(-x)"
    ))
  }
  
  invisible(x)
}

#' Validate Effect Size Parameters
#'
#' Validates effect size parameters (beta coefficients, odds ratios, etc.)
#' used in statistical calculations.
#'
#' @param x Numeric vector of effect sizes
#' @param arg_name Character string naming the argument being validated
#' @param allow_negative Logical. If TRUE (default), allows negative effect sizes
#' @param allow_zero Logical. If TRUE (default), allows zero effect sizes
#' @param ... Additional arguments (currently unused)
#'
#' @return Returns `x` invisibly if validation passes
#'
#' @examples
#' # Valid effect sizes
#' validate(c(-0.5, 0, 0.3, 1.2), arg_name = "beta")
#'
#' @export
validate.default <- function(x, arg_name = "x", allow_negative = TRUE, allow_zero = TRUE, ...) {
  # Check if it's numeric
  if (!is.numeric(x)) {
    cli::cli_abort(c(
      "x" = "Argument {.arg {arg_name}} must be numeric",
      "i" = "You provided a {.cls {class(x)}}"
    ))
  }
  
  # Check for missing values
  if (any(is.na(x))) {
    cli::cli_abort(c(
      "x" = "Argument {.arg {arg_name}} contains missing values",
      "i" = "All values must be non-missing"
    ))
  }

  # Check for infinite values
  if (any(!is.finite(x))) {
    cli::cli_abort(c(
      "x" = "Argument {.arg {arg_name}} contains infinite values",
      "i" = "All values must be finite"
    ))
  }
  
  # Check constraints
  if (!allow_negative && any(x < 0)) {
    min_val <- min(x)
    cli::cli_abort(c(
      "x" = "Argument {.arg {arg_name}} contains negative values: {min_val}",
      "i" = "Negative values are not allowed in this context"
    ))
  }
  
  if (!allow_zero && any(x == 0)) {
    cli::cli_abort(c(
      "x" = "Argument {.arg {arg_name}} contains zero values",
      "i" = "Zero values are not allowed in this context"
    ))
  }
  
  invisible(x)
}

#' Validate Sample Size Parameters
#'
#' Validates sample size parameters, ensuring they are positive integers
#' or can be reasonably interpreted as such.
#'
#' @param x Numeric vector of sample sizes
#' @param arg_name Character string naming the argument being validated
#' @param min_size Minimum allowed sample size (default: 1)
#' @param ... Additional arguments (currently unused)
#'
#' @return Returns `x` invisibly if validation passes
#'
#' @examples
#' # Valid sample sizes
#' validate_sample_size(c(10, 100, 1000), arg_name = "n")
#'
#' @export
validate_sample_size <- function(x, arg_name = "n", min_size = 1, ...) {
  # Check if it's numeric
  if (!is.numeric(x)) {
    cli::cli_abort(c(
      "x" = "Argument {.arg {arg_name}} must be numeric",
      "i" = "You provided a {.cls {class(x)}}"
    ))
  }
  
  # Check for missing values
  if (any(is.na(x))) {
    cli::cli_abort(c(
      "x" = "Argument {.arg {arg_name}} contains missing values",
      "i" = "All sample sizes must be non-missing"
    ))
  }

  # Check for infinite values
  if (any(!is.finite(x))) {
    cli::cli_abort(c(
      "x" = "Argument {.arg {arg_name}} contains infinite values",
      "i" = "All sample sizes must be finite"
    ))
  }
  
  # Check for non-positive values
  if (any(x < min_size)) {
    min_val <- min(x)
    cli::cli_abort(c(
      "x" = "Argument {.arg {arg_name}} contains values below minimum: {min_val}",
      "i" = "Sample sizes must be >= {min_size}"
    ))
  }
  
  # Warn about non-integer values
  if (any(x != round(x))) {
    non_int_vals <- x[x != round(x)]
    cli::cli_warn(c(
      "!" = "Argument {.arg {arg_name}} contains non-integer values",
      "i" = "Sample sizes are typically integers - values will be used as provided"
    ))
  }
  
  invisible(x)
}