context("test-make_formula")

test_that("multiplication works", {
  expect_equal(make_formula("a", "b"),
               a ~ b)
  expect_equal(make_formula("a", c("b", "c")),
               a ~ b + c)
  expect_equal(make_formula("a", ~ b + c),
               a ~ b + c)
  expect_equal(make_formula(c("a", "d"), c("b", "c")),
               cbind(a, d) ~ b + c)
  expect_equal(stp25formula:::to_formula("a", "b"),
               a ~ b)
  expect_equal(stp25formula:::to_formula("a", c("b", "c")),
               a ~ b + c)
  expect_equal(stp25formula:::to_formula("a", ~
                                           b + c),
               a ~ b + c)
  expect_equal(stp25formula:::to_formula(c("a", "d"), c("b", "c")),
               a + d ~ b + c)
  
})



