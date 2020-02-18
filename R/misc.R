#' @rdname formula_helper
#' @description check_data: prueft ob variablen vorhanden sind bzw ob Faelle NA sind.
#' @param  vars in check_data
#' @return check_data: Logical und wen FALSE ueber cat die  Objekt die falsch sind
#' @examples
#'
#' dat<- fata.frame(sex=1:2, m1=1:2, m3=1:2)
#' check_data(dat, c("m1", "m2") )
#' check_data(dat, c("m1", "sex") )
#'
#
check_data <- function (data, 
                        vars = NULL
                        ){
  if (!is.null(vars)) {
    vars_match <- names(data) %in% vars
    names_match <- names(data)[vars_match]
    
    #  print( vars  %in% names_match)
    if (all(vars %in% names_match)) {
      gibts_na <- sapply(data[vars],   function(x)
        any(!is.na(x)))
      if (all(gibts_na))
        TRUE
      else {
        cat("\nInput: ", vars, "\nVorhanden mit NA: ", vars[!gibts_na], "\n")
        FALSE
      }
    }
    else{
      cat("\nInput: ", vars, "\nVorhanden: ", names_match, "\n")
      FALSE
    }
  }
  else  {
    TRUE
  }
}


#' @rdname formula_helper
#' @description makeNamesNum: aus Nummern die Namen extrahieren
#' @param  data Daten als data.frame
#' @param  measure Variablen
#' @param  meAsNum  logical welche sind Zahlen
#' @return   string( )
#' @examples
#'
#'   measure <- c("geschl", "1" , "3:5", 1)
#'   stp25formula:::makeNamesNum(measure,  data=dat)
#'
makeNamesNum <- function(measure,
                         data,
                         meAsNum = grepl("^[[:digit:]]", measure)
                         ){
  if (sum(meAsNum) == 0)
    return(measure)
  measure_number <- NULL
  for (i in seq_len(length(meAsNum))) {
    if (meAsNum[i]) {
      if (grepl("[^[:digit:]]", measure[i])) {
        n <- stringr::str_split(measure[i], ":", 2)
        
        measure_number <- c(measure_number,
                            names(data)[seq(n[[1]][1], n[[1]][2])])
      }
      else
        measure_number <-
          c(measure_number, names(data)[as.numeric(measure[i])])
    }
    else
      measure_number <- c(measure_number, measure[i])
  }
  unique(measure_number)
}


#' class  
#'
#' Arbeiten mit mehrfachen Classen.
#'
#' @param data data.frame
#'
#' @noRd
get_classes <-
  function(data) {
    sapply(data, function(x)
      setdiff(class(x), "labelled"))
  }

#' stp25_stat_methode  
#'
#' in Tabelle verwendet
#'
#' @param x string
#' @param mymet Meine Methoden
#'
#' @noRd
stp25_stat_methode <- function(x,
                               mymet = c("freq",
                                         "mean",
                                         "median",
                                         "multiresponse",
                                         "pie",
                                         "dot",
                                         "hist",
                                         "box",
                                         "bar"
                                         
                                         )) {
  mymet[match(x, mymet)]
}
