test_that("validate.numeric works correctly for valid probability values", {
  # Test valid probability values
  expect_silent(validate(c(0, 0.5, 1), arg_name = "test_p"))
  expect_silent(validate(0.05, arg_name = "test_p"))
  expect_silent(validate(c(0.001, 0.999), arg_name = "test_p"))
  
  # Test with allow_zero and allow_one FALSE
  expect_silent(validate(c(0.001, 0.5, 0.999), arg_name = "test_p", 
                        allow_zero = FALSE, allow_one = FALSE))
})

test_that("validate.numeric rejects invalid probability values", {
  # Test negative values
  expect_error(validate(c(-0.1, 0.5), arg_name = "test_p"),
               "contains negative values")
  
  # Test values > 1
  expect_error(validate(c(0.5, 1.5), arg_name = "test_p"),
               "contains values > 1")
  
  # Test zero when not allowed
  expect_error(validate(c(0, 0.5), arg_name = "test_p", allow_zero = FALSE),
               "contains zero values")
  
  # Test one when not allowed
  expect_error(validate(c(0.5, 1), arg_name = "test_p", allow_one = FALSE),
               "contains values equal to 1")
})

test_that("validate.numeric provides helpful error messages", {
  # Test percentage-like values
  expect_error(validate(c(5, 10, 15), arg_name = "test_p"),
               "percentages.*dividing by 100")
  
  # Test very large values
  expect_error(validate(c(200, 500), arg_name = "test_p"),
               "chi-squared.*test statistics")
})

test_that("validate.numeric warns about very small values", {
  # Test very small values (potential -log10 p-values)
  expect_warning(validate(c(1e-12, 0.05), arg_name = "test_p"),
                 "very small values.*-log10")
})

test_that("validate.numeric handles missing and infinite values", {
  # Test missing values
  expect_error(validate(c(0.5, NA), arg_name = "test_p"),
               "contains missing values")
  
  # Test infinite values
  expect_error(validate(c(0.5, Inf), arg_name = "test_p"),
               "contains infinite values")
  
  expect_error(validate(c(0.5, -Inf), arg_name = "test_p"),
               "contains infinite values")
})

test_that("validate.default works for general numeric validation", {
  # Test valid numeric values
  expect_silent(validate.default(c(-1, 0, 1, 2.5), arg_name = "test_x"))
  expect_silent(validate.default(c(0.1, 0.5, 0.9), arg_name = "test_x"))
  
  # Test with constraints
  expect_silent(validate.default(c(1, 2, 3), arg_name = "test_x", 
                        allow_negative = FALSE, allow_zero = FALSE))
})

test_that("validate.default rejects non-numeric input", {
  # Test character input
  expect_error(validate.default("not_numeric", arg_name = "test_x"),
               "must be numeric.*character")
  
  # Test logical input
  expect_error(validate.default(c(TRUE, FALSE), arg_name = "test_x"),
               "must be numeric.*logical")
})

test_that("validate.default handles constraints correctly", {
  # Test negative values when not allowed
  expect_error(validate.default(c(-1, 0, 1), arg_name = "test_x", allow_negative = FALSE),
               "contains negative values")
  
  # Test zero when not allowed
  expect_error(validate.default(c(0, 1, 2), arg_name = "test_x", allow_zero = FALSE),
               "contains zero values")
})

test_that("validate.default handles missing and infinite values", {
  # Test missing values
  expect_error(validate.default(c(1, NA, 3), arg_name = "test_x"),
               "contains missing values")
  
  # Test infinite values
  expect_error(validate.default(c(1, Inf, 3), arg_name = "test_x"),
               "contains infinite values")
})

test_that("validate_sample_size works correctly", {
  # Test valid sample sizes
  expect_silent(validate_sample_size(c(1, 10, 100), arg_name = "test_n"))
  expect_silent(validate_sample_size(50, arg_name = "test_n"))
  
  # Test with custom minimum
  expect_silent(validate_sample_size(c(5, 10, 20), arg_name = "test_n", min_size = 5))
})

test_that("validate_sample_size rejects invalid sample sizes", {
  # Test non-numeric input
  expect_error(validate_sample_size("not_numeric", arg_name = "test_n"),
               "must be numeric")
  
  # Test values below minimum
  expect_error(validate_sample_size(c(0, 1, 2), arg_name = "test_n"),
               "contains values below minimum")
  
  expect_error(validate_sample_size(c(3, 4), arg_name = "test_n", min_size = 5),
               "must be >= 5")
})

test_that("validate_sample_size warns about non-integer values", {
  # Test non-integer values
  expect_warning(validate_sample_size(c(10.5, 20.3), arg_name = "test_n"),
                 "contains non-integer values")
})

test_that("validate_sample_size handles missing and infinite values", {
  # Test missing values
  expect_error(validate_sample_size(c(10, NA), arg_name = "test_n"),
               "contains missing values")
  
  # Test infinite values
  expect_error(validate_sample_size(c(10, Inf), arg_name = "test_n"),
               "contains infinite values")
})

test_that("validate S3 method dispatch works correctly", {
  # Test that the correct method is called for numeric (should use probability validation)
  expect_silent(validate(c(0.1, 0.5, 0.9)))
  
  # Test that default method is called for other types
  expect_error(validate("character"), "must be numeric")
})

test_that("validate functions return input invisibly", {
  # Test that input is returned invisibly
  x <- c(0.1, 0.5, 0.9)
  result <- validate(x, arg_name = "test")
  expect_identical(result, x)
  
  # Test for sample sizes
  n <- c(10, 20, 30)
  result <- validate_sample_size(n, arg_name = "test")
  expect_identical(result, n)
})