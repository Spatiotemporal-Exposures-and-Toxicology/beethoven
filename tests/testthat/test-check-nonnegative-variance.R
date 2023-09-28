#' @author Ranadeep Daw and Lara Clark
#' @description
#' unit testing: are the model variances nonnegative?
#' 
#'

test_that("the model variances nonnegative", {
  
  # the test is running on the object named "model.output.var"
  expect_equal( any(model.output.var < 0), FALSE)
})
