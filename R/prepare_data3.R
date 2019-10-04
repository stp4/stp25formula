

#' prepare_data2.formula
#'
#' @param x formel
#' @param data  data.frame
#' @param na.action na.pass, na.omit
#' @param groups condition
#'
#' @export
prepare_data2.formula <-
  function(x,
           data,
           groups = NULL,
           na.action = na.pass,
           drop.unused.levels=FALSE) {
    
    lbl <- stp25aggregate::GetLabelOrName(data)
    fm <- cleaup_formula(x, data, groups)
 
    
    dat <- select_data(fm$all.vars, 
                       data, 
                       na.action,
                       drop.unused.levels)
    
  
    stp25Data <- list(
      data = tibble::as_tibble(dat) ,
      measure.vars = fm$measure.vars,
      group.vars = fm$group.vars,
      condition.vars = fm$condition.vars,
      formula = fm$formula,
      by = fm$by,
      measure = fm$measure,
      row_name = lbl[fm$measure.vars],
      col_name = lbl[fm$group.vars],
      measure.class = fm$measure.class,
      group.class = fm$group.class,
      condition.class = fm$condition.class,
      digits = fm$digits,
      N = nrow(dat)
    )
    
    class(stp25Data) <- c("stp25data", "list")
    stp25Data
    
    
  }


#' prepare_data2.data.frame
#'
#' @param ... Namen oder Nummern (y-Variablen))
#' @param by  x-Variablen
#'
#' @export
prepare_data2.data.frame <- function(data,
                                     ...,
                                     by = "1",
                                     groups = NULL,
                                     na.action = na.pass,
                                     drop.unused.levels=FALSE) {
  # measure.vars <-
  #   sapply(lazyeval::lazy_dots(...), function(x)
  #     as.character(x[1]))
  
  
  hsub<- "h__"
  hend<- "__h"
  sub_haeding<- c()
  measure.vars <-
    sapply(lazyeval::lazy_dots(...), function(x) {
      if (!is.character(x$expr))
        as.character(x[1])
      else{
        sub_haeding <<- c(sub_haeding, as.character(x[1]))
        paste0(hsub , length(sub_haeding), hend)
      }
    })
  
  
  
  if( !is.null(sub_haeding ) ){
    i<- length(sub_haeding)
    nn <- ncol(data)
    data[ paste0(hsub, seq_along(i), hend) ] <- NA
    
    for (n in seq_along(i))
      attr(data[[n+nn]], "label") <- sub_haeding [[n]]
  }
  
  
  
  
  measure.vars <- cleaup_names(measure.vars, data)
  # message("measure.vars:")
#  print(measure.vars)
  
  
  fm <-
    to_formula(
      measure.vars = measure.vars,
      group.vars = by,
      condition.vars = groups
    )
  prepare_data2.formula(x = fm, 
                        data = data, 
                        na.action=na.action,
                        drop.unused.levels=drop.unused.levels)
  
}


#' model.frame
#' Formula::Formula splitet log(m1) + m2 + m3 + m4 ~ g richtig auf
#' @noRd
select_data <-   function(formula,
                          data,
                          na.action = NULL,
                          drop.unused.levels = FALSE) {
  
 formula <-  Formula::Formula(formula)
 data <- if (is.null(na.action))
    stats::model.frame(formula, data,
                       drop.unused.levels = drop.unused.levels)
  else
    stats::model.frame(formula, data,
                       na.action = na.action,
                       drop.unused.levels = drop.unused.levels)
  
  names(data)<- all.vars(formula)
  data
}

#' Aufdröseln
#' @noRd
cleaup_formula <- function(formula, data, groups) {
  measure <- digits<- NA
  if (!is.null(groups)) {
    # das ist nicht schoen aber es funktioniert auch bei langen Formeln
    warnings(" prepare_data2.formula : benutze Gruppen als condition.vars!")
    condition.vars <- gsub("~", "", deparse(groups))
    formula <-  paste(deparse(formula), collapse = "")
    formula <- formula(paste(formula, "|", condition.vars))
  }
  
  
  formula <- clean_dots_formula(formula, names_data = names(data))
  frml <- formula_split(formula)
    # message("cleaup_formula:")
   #  print(frml)
    
  formula <- frml$formula
 
  
  if (any(all.names(formula[[2L]]) %in% '[')) {
    #  bei var[2,median] kommt der Median durch, error wegen  width.cutoff = 60L
    y_hsd <-
      gsub(" ", "", paste(deparse(formula[[2L]]), collapse = ""))
    y_hsd <- strsplit(y_hsd, "\\+")[[1]]
    
    measure.vars <- gsub("\\[.+\\]", "", y_hsd) # bereinigen von Klammern
    measure <- rep(NA, length(measure.vars))
    digits <- rep(NA, length(measure.vars))
    # Dedect Position
    pos <- grep('\\[', y_hsd)
    # dedect_string afer ,  var[2,median]  gsub("[^[:alpha:]]", "", "var[2,median]")
    dedect_string <- gsub("[^[:alpha:]]", "",
                          stringr::str_extract(y_hsd[pos], "\\[.+"))
    
    dedect_string <- stp25_stat_methode(dedect_string) # return:"mean"   "freq"   "median" NA
    
    dedect_number <- as.integer(gsub("[^0-9]", "",
                                     stringr::str_extract(y_hsd[pos], "\\[.+")))
    
    if (!is_empty2(dedect_string)) {
      for (i in  seq_len(length(pos)))
        if (!is_empty2(dedect_string[i]))
          measure[pos[i]] <- dedect_string[i]
    }
    
    if (!is_empty2(dedect_number)) {
      for (i in seq_len(length(pos)))
        if (!is_empty2(dedect_number[i]))
          digits[pos[i]] <- dedect_number[i]
        digits <- as.numeric(digits)
    }
    
    if (length(formula) == 2) {
      formula <- to_formula(measure.vars, NULL)
      
    } else {
      x_hsd <- strsplit(deparse(formula[[3L]]), " \\+ ")[[1]]
      group.vars <- gsub("\\[.+\\]", "", x_hsd)
      formula <- to_formula(measure.vars, group.vars)
      
    }
    
    
  }
  
  measure.vars <- all.vars(formula[[2L]]) 
 # message("measure.vars:")
 # print(measure.vars)
  
  measure.class <- get_classes(data[measure.vars])

  if (any(is.na(measure)))
    measure <- default_measure(measure, measure.vars, measure.class)
  if (any(is.na(digits)))
    digits <- default_digits(digits, measure.vars, measure.class)
  
  if (length(formula) == 3L ){
    group.vars <-  all.vars(formula[[3L]])
    by <- formula(paste("~", paste(group.vars, collapse="+")))
    group.class <- get_classes(data[group.vars])
  }
  else{
    group.vars<-  group.class<- NULL
    by<- "1"
  }
  
  if(!is.null(frml$condition)){ 
    condition.vars <- all.vars(frml$condition)
    condition.class <- get_classes(data[condition.vars]) 
    }
    else{
      condition.vars<-condition.class <- NULL
      
    }
 
 
  
  list(
    formula = formula,
    by =   by,
    measure.vars = measure.vars,
    group.vars = group.vars,
    condition.vars = condition.vars,
    measure = measure,
    digits = digits,
    measure.class = measure.class,
    group.class = group.class,
    condition.class = condition.class,
    all.vars = if(is.null(condition.vars)) formula 
       else update(formula, 
                   formula(paste("~ . +", paste(
                     condition.vars, collapse="+"))))
  )
}

#' auswertungs Methode
#' @noRd
default_measure <- function(measure, measure.vars, measure.class) {
  
  #    message("measure.vars:")
  # print(measure)
  # print(measure.vars)
  # print(measure.class)
  
  if (length(measure) == 1) {
    measure <- measure.class
  }
  else{
    nas <- which(is.na(measure))
    measure[nas] <- measure.class[nas]
  }
  names(measure) <- measure.vars
  measure
}

#' digits
#' @noRd
default_digits <- function(digits, measure.vars, measure.class) {
  if (length(digits) == 1) {
    digits <-  ifelse(
      measure.class == "factor",
      stp25rndr::default_stp25("digits", "prozent"),
      stp25rndr::default_stp25("digits", "mittelwert")
    )
  }
  else{
    nas <- which(is.na(digits))
    digits[nas]<-
    ifelse(
      measure.class[nas] == "factor",
      stp25rndr::default_stp25("digits", "prozent"),
      stp25rndr::default_stp25("digits", "mittelwert")
    )
  }
  names(digits) <- measure.vars
  digits
}

#' Variablen als Nummer
#' @noRd
cleaup_names <- function(measure.vars, data ) {
  measure <- makeNamesNum(measure.vars, data)
  
  if(any( measure == "" | is.na(measure) )){
    measure <- measure[measure != ""]
    measure <- measure[!is.na(measure)]
  }
  
  
  # hier fehlt noch der Fehlercheck
  # 
  # names(data) %in% measure
  # message("Prüfe ob Variablen vorhenden find")
  # print(measure.vars)
  # print(names(data) %in% measure)
  measure
}





# 
# dat <- data.frame(
#   sex = c("m", "f", "m", "f", "m"),  treatment = c("A", "A", "B", "B", "A"),
#   m1 = c(1, NA, 2, 1,1),  m2 = 1:5,  m3 = 1:5,  m4 = 1:5,  m5 = 1:5,  m6 = 1:5
# )
# dat <- stp25aggregate::Label(dat, sex="Geschlecht", m1="Cohlesterin", m2="Billirubin")
# 
# a <- prepare_data2(m1 ~ sex, dat)
# b <- prepare_data3(m1 ~ sex , dat)
# 
# 
# testthat::expect_equal(
#   prepare_data3(m1 ~ sex | treatment, dat),
#   prepare_data2(m1 ~ sex |
#                                 treatment, dat)
# )
# 
# prepare_data3(~ m1[2] + m2[3] + log(m3)[median] + m4[0, median],  dat)
# #prepare_data3(m1[2] + m2[3] + log(m3)[median] + m4[0, median] ~ sex,  dat)
# 
# 
# 
# a<-prepare_data3(~ m1[2] + m2[3] + m3[median] + m4[0, median],  dat)
# b<-  prepare_data2(~ m1[2] + m2[3] + m3[median] + m4[0, median],  dat)
#  
# 
# a$measure.class
# b$measure.class
# #testthat::expect_equal(a, b)
# 
# 
# 
# 
# Tabelle3 <- function(x, data, ..., subset){
#   # orginal subset.data.frame
#   r <- if (missing(subset)) 
#     rep_len(TRUE, nrow(data))
#   else {
#     e <- substitute(subset)
#     r <- eval(e, data, parent.frame())
#     if (!is.logical(r)) 
#       stop("'subset' must be logical")
#     r & !is.na(r)
#   }
#   
#   data <- data[r,]
#   
#  
#  stp25stat::Tabelle(x, data, ...)
#   
#  # data
# }
# 
# 
# require(stpvers)
# 
#  Tabelle3(~ m1[2] + m2[3] + m3[median] + m4[0, median],  dat, subset= sex=="m")
# 
#  Tabelle(~ m1[2] + m2[3] + m3[median] + m4[0, median],  dat)
#  Tabelle3(~ m1[2] + m2[3] + m3[median] + m4[0, median],  dat)