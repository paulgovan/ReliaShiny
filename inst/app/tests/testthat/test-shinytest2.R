library(shinytest2)

test_that("{shinytest2} recording: app", {
  app <- AppDriver$new(name = "app", seed = 123, height = 857, width = 1211)
  app$expect_values()
  app$set_inputs(sidebarItemExpanded = "LifeData")
  app$set_inputs(tabset1 = "Contour Plot")
  app$set_inputs(tabset1 = "Probability Plot")
  app$expect_values()
})
