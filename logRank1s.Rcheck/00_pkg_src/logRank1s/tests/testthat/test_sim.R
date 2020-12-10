

test_that("test if f function is correct", {
  expect_equal(Simulation(n = 534, parameter = T, B = 1000, ta = 3, tf = 1, 
                          m0 = 1, delta = 1/1.2, k = 0.1), 
               0.043)
} 
)

test_that("test if f function is correct", {
  expect_equal(Simulation(n = 534, parameter = F, B = 1000, ta = 3, tf = 1, 
                          m0 = 1, delta = 1/1.2, k = 0.1), 
               0.908)
} 
)