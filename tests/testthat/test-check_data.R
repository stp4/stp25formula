context("test-check_data")

test_that("check works", {
  dat <-
    data.frame(
      sex = 1:2,
      m1 = 1:2,
      m2 = 1:2,
      m3 = 1:2,
      m4 = 1:2,
      m5 = 1:2,
      m6 = 1:2,
      geschl = 1:2
    )
  expect_false(stp25formula:::check_data(dat, c("m1", "m7")))
  expect_true(stp25formula:::check_data(dat, c("m1", "sex")))
})
