test_that("test if f function is correct", {
  expect_equal(power(alpha = 0.05, n = 88, ta = 5, tf = 3, 
                          m0 = 9, delta = 1/1.75, k = 1.22), 0.803)
} 
)