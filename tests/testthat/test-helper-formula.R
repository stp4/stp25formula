context("test-internal-prepare_data")

test_that("cleaup_formula", {
  df <- tibble::tibble(
    month = c(1, 2, 3, 1, 2, 3),
    student = gl(2, 3, labels = c("Amy", "Bob")),
    a = c(9, 7, 6, 8, 6, 9),
    b = c(6, 7, 8, 5, 6, 7)
  )
  expect_equal(stp25formula:::cleaup_formula( ~ a, df, NULL)$digits,
               c(a = 2))
  
  expect_equal(
    stp25formula:::cleaup_formula( ~ a + b, df, NULL)$digits,
    stp25formula:::cleaup_formula(a + b ~ student, df, NULL)$digits
  )
  
  
  # stp25formula:::cleaup_formula(a[median, 1] + b[3] ~ student, df, NULL)$digits
  # stp25formula:::cleaup_formula(a  + b[3] ~ student, df, NULL)$digits
  # stp25formula:::cleaup_formula(a + b ~ student, df, NULL)$digits
  # 
  # x1<-stp25formula:::cleaup_formula(a[median, 1] + b[3] ~ student, df, NULL)
  # 
  # x2<-stp25formula:::cleaup_formula(a  + b  ~ student, df, NULL)
  # 
  # x1$digits
  # x2$digits
  # 
  # x1$measure
  # x2$measure
  
  
  expect_equal(stp25formula:::cleaup_formula(a[median, 1] + b[3] ~ student, df, NULL)$digits,
               c(a = 1, b = 3))
  
  
  #stp25formula:::cleaup_formula( a+b~student+month, df, NULL)
  
  
  
  expect_equal(
    stp25formula:::cleaup_formula(a + b ~ student + month, df, NULL),
    stp25formula:::cleaup_formula(. ~ student + month, df, NULL)
  )
  
})


test_that("select_data", {
  df <- tibble::tibble(
    month = c(1, 2, 3, 1, 2, 3),
    student = gl(2, 3, labels = c("Amy", "Bob")),
    a = c(9, 7, 6, 8, 6, 9),
    b = c(6, 7, 8, 5, 6, 7)
  )
  

  
  expect_equivalent(stp25formula:::select_data( ~ a + b, df),
               df[3:4])
  
  df2 <- df[c(3, 4, 2)]
  df2$b <- log(df2$b)
  expect_equivalent(stp25formula:::select_data(a + log(b) ~ student, df),
               df2)
  
})
