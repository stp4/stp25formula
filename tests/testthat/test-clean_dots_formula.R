context("test-clean_dots_formula")

test_that("formula works", {
  data <- data.frame(x = NA, y = NA, z = NA)
  
  expect_equal(stp25formula:::clean_dots_formula(x ~ y, data),
               x ~ y)
  expect_equal(stp25formula:::clean_dots_formula(. ~ x + y, data),
               z ~ x + y)
  expect_equal(stp25formula:::clean_dots_formula(x + y ~ ., data),
               x + y ~ z)
  expect_equal(stp25formula:::clean_dots_formula(~ ., data),
               ~ x + y + z)
  expect_equal(
    stp25formula:::formula_split(a + b ~ x | y),
    list(
      formula =   a + b ~ x,
      condition = ~ y,
      facet_type = "facet_wrap"
    )
  )
  
})
