test_that("test if f function is correct", {
  expect_equal(SampleSize(alpha = 0.05, power = 0.8, ta = 5, tf = 3, 
                       m0 = 9, delta = 1/1.75, k = 1.22), 88)
 } 
)