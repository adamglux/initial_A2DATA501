context("Testing influenceR context")

## invalid inputs 
test_that("function influenceR gives helpful errors", 
          {
            expect_error(influenceR(model1, matrix(data1)), 
                         "Input data must be a data.frame.")
            
            expect_error(influenceR(mtcars, mtcars), 
                         "Input model must be an lm or glm object.")
            
            expect_error(influenceR(model6, data6),
                         "Input data must have at least 2 columns.")
            
            expect_error(influenceR(model5, data5), 
                         "Dataset is too small")
            
            expect_error(influenceR(model7, data7), 
                         "Dataset is the wrong dimension")
            
            expect_error(influenceR(model4, data4), 
                         "Input data contains infinite values.")
            
            expect_error(influenceR(model3, data3), 
                         "Input data contains more than 5% NA values.")
            
            expect_error(influenceR(model1, data1, plot = "phil collins"), 
                         "Input for plot must be either: none, all, cooks, DFFITS, or Hadi")
            
            output <- influenceR(model1, data1, plot = "none")
            expect_true(is.list(output))
            expect_equal(names(output$influenceRs), c("Cook's D", "DFFITs", "Hadi's Influence"))
            
            
            # Assuming pre-calculated values for a specific row for testing
            expect_equal(round(output[[1]][1, "Cook's D"], 3), 0.016)
            expect_equal(round(output[[1]][1, "DFFITs"], 3), -0.223)
            expect_equal(round(output[[1]][1, "Hadi's Influence"], 3), 1.076)
            
          })

## valid inputs
test_that("function influenceR works with dataframes, lm/glm objects, and plots", {
  
  pdf(NULL)
  on.exit(dev.off())
  
  expect_silent(influenceR(model1, mtcars, plot = "none"))
  expect_silent(influenceR(model1, mtcars, plot = "cooks"))
  expect_silent(influenceR(model1, mtcars, plot = "DFFITS"))
  expect_silent(influenceR(model1, mtcars, plot = "Hadi"))
  expect_silent(influenceR(model1, mtcars, plot = "all"))
  
})