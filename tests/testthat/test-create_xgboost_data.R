#' @author Anisha Singh
#' @author Eva Marques
#' @description
#' Unit testing for double_to_site() function
testthat::test_that("double_to_site_id function is working well", {
  expect_no_error(double_to_site_id(x = 12))
  expect_error(double_to_site_id(x = 123456789123456), "too many digits")
  expect_equal(double_to_site_id(1), "00000000000001")
  expect_error(double_to_site_id("1"),
               "non-numeric argument to mathematical function")
  expect_error(double_to_site_id(c(1,1)),
               "the condition has length > 1")
})

#' @author Anisha Singh
#' @author Eva Marques
#' @description
#' Unit testing for merge_covariates() function
testthat::test_that("merge_covariates function is working well", {
})
