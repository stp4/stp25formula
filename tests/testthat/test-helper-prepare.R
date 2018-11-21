context("test-helper-prepare.R")

test_that("prepare_data works", {
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
  
  measure <- c("geschl", "1" , "3:5", 1)
  expect_equal(
    stp25formula:::makeNamesNum(measure,  data = dat),
    c("geschl", "sex",    "m2", "m3", "m4")
  )
  
  expect_equal(
    names(Formula_Data(m1[3] + m2 ~ geschl, dat)),
    c(
      "X_data" ,
      "Y_data"  ,
      "xname" ,
      "yname"   ,
      "Z_data"  ,
      "zname"  ,
      "formula" ,
      "condition"  ,
      "formula.orginal",
      "digits"  ,
      "type"
    )
  )
  
  expect_equal(
    names(Formula_Names(m1[3] + m2 ~ geschl, dat)),
    c(
      "yname"  ,
      "xname"  ,
      "zname" ,
      "formula" ,
      "condition",
      "digits" ,
      "type"
    )
  )
  
})
