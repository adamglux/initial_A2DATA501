context("Testing influencer context")

## invalid inputs 
test_that("function influencer gives helpful errors", 
          {
            expect_error(influencer(model1, matrix(data1)), 
                         "Input data must be a data.frame.")
            
            expect_error(influencer(mtcars, mtcars), 
                         "Input model must be an lm or glm object.")
            
            expect_error(influencer(model6, data6),
                         "Input data must have at least 2 columns.")
            
            expect_error(influencer(model5, data5), 
                         "Dataset is too small")
            
            expect_error(influencer(model7, data7), 
                         "Dataset is the wrong dimension")
            
            expect_error(influencer(model4, data4), 
                         "Input data contains infinite values.")
            
            expect_error(influencer(model3, data3), 
                         "Input data contains more than 5% NA values.")
            
            expect_error(influencer(model1, data1, plot = "phil collins"), 
                         "Input for plot must be either: none, all, cooks, DFFITS, or Hadi")
            
            output <- influencer(model1, data1, plot = "none")
            expect_true(is.list(output))
            expect_equal(names(output$influencers), c("Cook's D", "DFFITs", "Hadi's Influence"))
            
            
            # Assuming pre-calculated values for a specific row for testing
            expect_equal(round(output[[1]][1, "Cook's D"], 3), 0.016)
            expect_equal(round(output[[1]][1, "DFFITs"], 3), -0.223)
            expect_equal(round(output[[1]][1, "Hadi's Influence"], 3), 1.076)
            
          })

## valid inputs
test_that("function influencer works with dataframes, lm/glm objects, and plots", {
  
  pdf(NULL)
  on.exit(dev.off())
  
  expect_silent(influencer(model1, mtcars, plot = "none"))
  expect_silent(influencer(model1, mtcars, plot = "cooks"))
  expect_silent(influencer(model1, mtcars, plot = "DFFITS"))
  expect_silent(influencer(model1, mtcars, plot = "Hadi"))
  expect_silent(influencer(model1, mtcars, plot = "all"))
  
})