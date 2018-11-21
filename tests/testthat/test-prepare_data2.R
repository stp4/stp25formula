context("test-prepare_data2.R")

test_that("Formula equal Pipe", {
  fm1 <- prepare_data2(~ len + supp + dose, ToothGrowth)
  pp1 <- prepare_data2(ToothGrowth, len, supp, dose)
  expect_equal(names(fm1),
               names(pp1))
})

test_that("Formula equal Pipe measure.vars", {
  fm1 <- prepare_data2(~ len + supp + dose, ToothGrowth)
  pp1 <- prepare_data2(ToothGrowth, len, supp, dose)
  expect_equal(fm1$measure.vars,
               pp1$measure.vars)
})

test_that("Formula equal Pipe digits Klammer", {
  fm1 <- prepare_data2( ~ len[3] + supp + dose[4], ToothGrowth)
  pp1 <- prepare_data2(ToothGrowth, len[3], supp, dose[4])
  expect_equal(fm1$digits,
               pp1$digits)
})


test_that("prepare_data2 formula simpel", {
  dat <-
    data.frame(
      g = gl(2, 4, labels = c("Control", "Treat")),
      m1 = rnorm(8),
      m2 =  rnorm(8),
      m3 =  rnorm(8),
      m4 =  rnorm(8),
      m5 =  rnorm(8),
      m6 =  rnorm(8),
      geschl = gl(2, 4, labels = c("m", "f"))
    )
  r1 <- prepare_data2(~ m1 + m2 + m3 + m4, dat)
  
  expect_equal(r1$measure.vars,
               c("m1", "m2", "m3", "m4"))
  
  expect_equal(r1$group.vars,
               NULL)
  expect_equal(r1$condition.vars,
               NULL)
  
  expect_equal(r1$measure,
               c(
                 m1 = "numeric",
                 m2 = "numeric",
                 m3 = "numeric",
                 m4 = "numeric"
               ))
  
  expect_equal(r1$digits,
               c(
                 m1 = 2,
                 m2 = 2,
                 m3 = 2,
                 m4 = 2
               ))
  expect_equal(r1$by,
               "1")
  
  expect_that(is.data.frame(r1$data),
              is_true())
  
  expect_equal(r1$N,
               8)
  
})


test_that("prepare_data2 formula digits and measure", {
  dat <-
    data.frame(
      g = gl(2, 4, labels = c("Control", "Treat")),
      m1 = abs(rnorm(8)),
      m2 =  rnorm(8),
      m3 =  rnorm(8),
      m4 =  rnorm(8),
      m5 =  rnorm(8),
      m6 =  rnorm(8),
      geschl = gl(2, 4, labels = c("m", "f"))
    )
  r2 <- prepare_data2(~ log(m1) + m2 + m3 + m4, dat)
  r3 <- prepare_data2(~ m1[1] + m2 + m3 + m4, dat)
  r4 <- prepare_data2(~ m1[1] + m2 + m3[2, median] + m4, dat)
  
  expect_equivalent(r2$data$m1,
                    log(dat$m1))
  
  expect_equal(r3$measure.vars,
               c("m1", "m2", "m3", "m4"))
  
  expect_equal(r4$measure,
               c(
                 m1 = "numeric",
                 m2 = "numeric",
                 m3 = "median",
                 m4 = "numeric"
               ))
  
  expect_equal(r3$digits,
               c(
                 m1 = 1,
                 m2 = 2,
                 m3 = 2,
                 m4 = 2
               ))
  
  expect_equal(r3$digits,
               r4$digits)
  
})

test_that("prepare_data2 formula and data.frame simpel", {
  dat <-
    data.frame(
      g = gl(2, 4, labels = c("Control", "Treat")),
      m1 = rnorm(8),
      m2 =  rnorm(8),
      m3 =  rnorm(8),
      m4 =  rnorm(8),
      m5 =  rnorm(8),
      m6 =  rnorm(8),
      geschl = gl(2, 4, labels = c("m", "f"))
    )
  r1 <- prepare_data2(dat, m1 , m2 , m3, m4)
  r2 <- prepare_data2( ~ m1 + m2 + m3 + m4 , dat)
  
  expect_equal(r1$data,
               r2$data)
  expect_equivalent(r1$formula,
                    r2$formula)
  
  expect_equal(r1$measure.vars,
               r2$measure.vars)
  
  expect_equal(r1$measure,
               r2$measure)
  
  expect_equal(r1$digits,
               r2$digits)
  
})

test_that("prepare_data2 formula and data.frame digits", {
  dat <-
    data.frame(
      g = gl(2, 4, labels = c("Control", "Treat")),
      m1 = rnorm(8),
      m2 =  rnorm(8),
      m3 =  rnorm(8),
      m4 =  rnorm(8),
      m5 =  rnorm(8),
      m6 =  rnorm(8),
      geschl = gl(2, 4, labels = c("m", "f"))
    )
  r1 <- prepare_data2(dat, m1[3], m2[median], m3, m4[1, median])
  r2 <-
    prepare_data2( ~ m1[3] + m2[median] + m3 + m4[1, median], dat)
  
  expect_equal(r1$data,
               r2$data)
  expect_equivalent(r1$formula,
                    r2$formula)
  
  expect_equal(r1$measure.vars,
               r2$measure.vars)
  
  expect_equal(r1$measure,
               r2$measure)
  
  expect_equal(r1$digits,
               r2$digits)
  
  r3 <- prepare_data2(dat, m1 , m2, m3 = median)
  r4 <- prepare_data2(dat, m1 , m2, m3[median])
  r5 <- prepare_data2( ~ m1 + m2 + m3[median], dat)
  
  expect_equal(r3$measure,
               r4$measure)
  
  expect_equal(r3$measure,
               r5$measure)
  
})


test_that("prepare_data2  data.frame number and names", {
  dat <-
    data.frame(
      g = gl(2, 4, labels = c("Control", "Treat")),
      m1 = rnorm(8),
      m2 =  rnorm(8),
      m3 =  rnorm(8),
      m4 =  rnorm(8),
      m5 =  rnorm(8),
      m6 =  rnorm(8),
      geschl = gl(2, 4, labels = c("m", "f"))
    )
  r1 <- prepare_data2(dat, m1 , m2 , m3, m4)
  r6 <-  prepare_data2(dat, 2:5)
  
  expect_equal(r1$data,
               r6$data)
  expect_equivalent(r1$formula,
                    r6$formula)
  
  expect_equal(r1$measure.vars,
               r6$measure.vars)
  
  expect_equal(r1$measure,
               r6$measure)
  
  expect_equal(r1$digits,
               r6$digits)
  
  
})


test_that("prepare_data2 formula and data.frame groups", {
  dat <-
    data.frame(
      g = gl(2, 4, labels = c("Control", "Treat")),
      m1 = rnorm(8),
      m2 =  rnorm(8),
      m3 =  rnorm(8),
      m4 =  rnorm(8),
      m5 =  rnorm(8),
      m6 =  rnorm(8),
      geschl = gl(2, 4, labels = c("m", "f"))
    )
  r1 <- prepare_data2(dat, m1, m2, m3 , m4, by = ~ geschl)
  r2 <- prepare_data2(m1 + m2 + m3 + m4 ~ geschl, dat)
  r3 <-
    prepare_data2(dat, m1[4, median], m2, m3 , m4[5], by = ~ geschl)
  r4 <- prepare_data2(m1[4, median] + m2 + m3 + m4[5] ~ geschl, dat)
  
  expect_equal(r1$data,
               r2$data)
  expect_equal(r1$data,
               r3$data)
  expect_equal(r1$data,
               r4$data)
  
  expect_equivalent(r1$formula,
                    r2$formula)
  expect_equivalent(r1$formula,
                    r3$formula)
  expect_equivalent(r1$formula,
                    r4$formula)
  
  expect_equivalent(r1$group.vars,
                    r2$group.vars)
  expect_equivalent(r1$group.vars,
                    r3$group.vars)
  expect_equivalent(r1$group.vars,
                    r4$group.vars)
  
  expect_equal(r1$measure.vars,
               r2$measure.vars)
  
  expect_equal(r3$measure.vars,
               r4$measure.vars)
  
  expect_equal(r3$digits,
               r4$digits)
  
})
