#' prepare_data2
#'
#'  Funktion wird zum Aufbereiten der Daten verwendet. Die Daten werden als
#'  tibble::as_tibble() weitergegeben.
#'
#'
#'  Tabelle.default()
#'  errate_statistik3()
#'  APA.formula()
#'  corr_plot.formula()
#'  Hmisc_rcorr() also APA_Correlation
#'
#' @return Liste mit Namen und Daten
#' data,
#' measure.vars ,
#' group.vars,
#' condition.vars,
#' formula,
#' by,
#' measure,
#' row_name,
#' col_name,
#' measure.class,
#' group.class,
#' condition.class,
#' digits
#' @export
#' @examples
#'
#' dat<- data.frame(sex=1:2, m1=1:2,m2=1:2, m3=1:2, m4=1:2, m5=1:2, m6=1:2, geschl=1:2)
#'
#' prepare_data2(~ m1 + m2 + m3 + m4, dat)
#' prepare_data2(~ log(m1) + m2 + m3 + m4, dat)
#' prepare_data2(~ m1[1] + m2 + m3 + m4, dat)
#' prepare_data2(~ m1[1] + m2 + m3[4,median] + m4, dat)
#' prepare_data2(dat, m1, m2, m3, m4)
#' prepare_data2(dat, 4:7)
#' prepare_data2(dat, m1[1], m2, m3, m4)
#' prepare_data2(dat, m1[1], m2, m3[4,median], m4)
#' prepare_data2(dat, m1 , m2, m3=median, m4)
#' prepare_data2(dat, m1, m2, m3 , m4, by =  ~ geschl)
#' prepare_data2(dat, m1[4, median], m2, m3 , m4[5], by =  ~ geschl)
#'
#'

prepare_data2 <- function(...){
  UseMethod("prepare_data2")
}

#' @rdname prepare_data2
#' @export
prepare_data2.NULL <- function() {
  res <- list(
    data = NULL,
    measure.vars = NULL,
    group.vars = NULL,
    condition.vars = NULL,
    formula = NULL,
    by = NULL,
    measure = NULL,
    row_name = NULL,
    col_name = NULL,
    measure.class = NULL,
    group.class = NULL,
    condition.class = NULL,
    digits = NULL,
    N = NULL
  )
  class(res) <- c("stp25data", class(res))
  res
  
  
}

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
    
    lbl <- stp25aggregate::get_label(data, include.units = TRUE)
    fm <- cleaup_formula(x, data, groups)
    dat <- select_data(fm$all.vars, 
                       data, 
                       na.action,
                       drop.unused.levels)

    stp25Data <- list(
      data =    dat,
      measure.vars = fm$measure.vars,
      group.vars = fm$group.vars,
      condition.vars = fm$condition.vars,
      formula = fm$formula,
      by = fm$by,
      measure = fm$measure,
      measure.test =fm$measure.test,
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
    #i<- length(sub_haeding)
    
    nn <- ncol(data)
    data[ paste0(hsub, seq_along(sub_haeding), hend) ] <- NA
    
    for (n in seq_along(sub_haeding))
      attr(data[[n+nn]], "label") <- sub_haeding [[n]]
  }
  
  measure.vars <- cleaup_names(measure.vars, data)
  
  # Fehlercheck
  if (length(setdiff(measure.vars, names(data))) > 0) {
    missing_measure.vars <- setdiff(measure.vars, names(data))
  #  i <- length(missing_measure.vars)
    
    nn <- ncol(data)
    data[missing_measure.vars] <- NA

    for (n in seq_along(missing_measure.vars))
      attr(data[[n + nn]], "label") <- paste("Error:", missing_measure.vars[n], "dose not exist!")
  }

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
    stats::model.frame(formula,
                       data,
                       na.action = na.action,
                       drop.unused.levels = drop.unused.levels)
  
  names(data) <- all.vars(formula)
  
  if (tibble::is_tibble(data))
    data
  else
    tibble::as_tibble(data)
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
  formula <- frml$formula
  dedect_string_test <- NULL
  
  if (any(all.names(formula[[2L]]) %in% '[')) {
    #  bei var[2,median] kommt der Median durch, error wegen  width.cutoff = 60L
    y_hsd <-
      gsub(" ", "", paste(deparse(formula[[2L]]), collapse = ""))
    y_hsd <- strsplit(y_hsd, "\\+")[[1]]
    
    measure.vars <- gsub("\\[.+\\]", "", y_hsd) # bereinigen von Klammern
    measure <- as.character(rep(NA, length(measure.vars)))
    dedect_string_test <- measure
    digits <- as.integer(rep(NA, length(measure.vars)) )
    names(digits) <- measure.vars
    names(measure) <- measure.vars
  
    # Dedect Position
    pos <- grep('\\[', y_hsd)
    # dedect_string afer ,  var[2,median]  gsub("[^[:alpha:]]", "", "var[2,median]")
    dedect_string <- gsub("[^[:alpha:]]", "",
                          stringr::str_extract(y_hsd[pos], "\\[.+"))
    
    dedect_test <- stp25_test_methode(dedect_string)
    # return:"mean"   "freq"   "median" NA
    dedect_string <- stp25_stat_methode(dedect_string) 
    
    dedect_number <- as.integer(gsub("[^0-9]", "",
                                     stringr::str_extract(y_hsd[pos], "\\[.+")))
    
    
    if (!is.null(dedect_test)) {
     for (i in  seq_len(length(pos)))
        if (!is_empty2(dedect_test[i]))
           dedect_string_test[pos[i]] <- dedect_test[i]
    }
    
    if (!is_empty2(dedect_string)) {
      for (i in  seq_len(length(pos)))
        if (!is_empty2(dedect_string[i]))
          measure[pos[i]] <- dedect_string[i]
    }
    
    if (!is_empty2(dedect_number)) {
      for (i in seq_len(length(pos)))
        if (!is_empty2(dedect_number[i]))
          digits[pos[i]] <- dedect_number[i]
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
 
 
  # clean measre 
  measure <- gsub("freq", "factor", measure)
  
#' Texte also Überschfifte werden zu logical mit NA
#' daher hie die Heder vergeben

   if (any(measure == "logical")) {
    logik <-  which(measure == "logical")
    any_missing <-
      sapply(data[measure.vars[logik]], function(x)
        length(na.omit(x)))
  
    measure[logik] <-
      ifelse(
        measure[logik]  == "logical" & (any_missing == 0),
        "header", measure[logik])
   }
  # 
  list(
    formula = formula,
    by =   by,
    measure.vars = measure.vars,
    group.vars = group.vars,
    condition.vars = condition.vars,
    measure = measure,
    measure.test = which_test(measure, group.class[1], dedect_string_test),
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



#' which_test
#' 
#' 
#' @noRd
#' @examples 
#' which_test( "factor", NULL)
#' which_test( "factor", "logical")
#' which_test( "numeric", "factor")
#' 
#' which_test(c("median", "mean", "logical", "numeric", "multi"),
#' "factor",
#' c(NA, "ttest", NA, NA, NA))
#' 
#' c(
#'   median = "contest" ,
#'   mean = "ttest",
#'   logical = "cattest" ,
#'   numeric = "contest" ,
#'   multi = "notest"
#' )

which_test <-
  function(measure,
           group.class=NULL,
           measure.test = NULL,
           # test = c("catTest", "conTest", "ordTest", "noTest", "corTest")
           catTest = c("factor", "freq", "logical", "multi"),
           conTest = c("numeric", "integer", "mean", "median")
           ) {
    
  #  cat("\n m: ", measure,"\ng: ",group.class, "\nt: ", measure.test, "\n" )
    rslt <-  sapply(measure, function(measure) {
      if (is.null(group.class)) { "notest"
      }
      else if (group.class == "factor") {
        if (measure %in% catTest) "cattest"
        else if (measure %in% conTest) "contest"
        
        else
          "notest"
      } else if (group.class == "numeric") {
        if (measure %in%  conTest) "cortest"
        else  "notest"
      } else "notest"
    })
    
    if (!is.null(measure.test)) {
      i <-  which(!is.na(measure.test))
      rslt[i] <- measure.test[i]
    }
    rslt
  }







#' auswertungs Methode
#' @noRd
default_measure <- function(measure, measure.vars, measure.class) {
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
    digits[nas] <-
      ifelse(
        measure.class[nas] %in% c("factor", "logical"),
        stp25rndr::default_stp25("digits", "prozent"),
        stp25rndr::default_stp25("digits", "mittelwert")
      )
  }
  names(digits) <- measure.vars
  digits
}

#' Variablen als Nummer
#' @noRd
cleaup_names <- function(measure.vars, data) {
  measure <- makeNamesNum(measure.vars, data)
  
  if (any(measure == "" | is.na(measure))) {
    measure <- measure[measure != ""]
    measure <- measure[!is.na(measure)]
  }
  
  measure
}

