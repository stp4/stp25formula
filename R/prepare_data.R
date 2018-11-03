#' Formula_Data
#'
#' Funktion zum aufbereiten der formula. Output List mit neuen Daten.
#' Die Funktion Formula kan nur \code{Formula(y1 +y2 +y3 ~ .)} aufarbeinten aber nicht
#' \code{Formula(. ~ y1 +y2 +y3  )} daher wird über \code{clean_dots_formula()} die
#' Formel vorbereitet
#'
#' errate_statistik2()
#'
#' @param  x   Formel
#' @param  data Daten als data.frame
#' @param  subset geht mit data$variable
#' @param  na.action nicht aendern default ist na.pass
#' @return Formula_Data: list mit "X_data","Y_data","xname","yname",
#' "Z_data",  "zname", "formula", "condition", "formula.orginal", "digits", "type"
#' @export
#' @examples
#' names(Formula_Data(m1[3]+m2~geschl, varana))
#'
Formula_Data <- function(x,
                         data,
                         subset,
                         na.action = na.pass,
                         ...) {
  fm <- Formula_Names(x, data)
  if (is.null(fm$condition)) {
    data_condition <- NULL
    zname <- NULL
    condition <- NULL
  }
  else {
    zname <- all.vars(fm$condition)
    condition <- fm$condition
    data_condition <- data[zname] #1 #which(names(data) %in% )
  }
  #formula<- fm$formula
  my_formula <- Formula::Formula(fm$formula)
  # all.vars(s~a+g)
  if (!check_data(data, all.vars(my_formula))) {
    print(names(data))
    print(all.vars(my_formula))
    warning("Namen und Daten passen nicht zusammen oder es gibt NA's")
  }
  
  if (!missing(subset) && length(subset))
    data <- stats::model.frame(my_formula,
                               data = data,
                               subset = subset,
                               na.action = na.action)
  
  else
    data <- stats::model.frame(my_formula,
                               data = data,
                               na.action =
                                 na.action)
  
  X_data <- Formula::model.part(my_formula, data = data, rhs = 1)
  Y_data <- Formula::model.part(my_formula, data = data, lhs = 1)
  
  xname  <- names(X_data)
  yname  <- names(Y_data)
  
  if (!length(yname)) {
    #- Einzelvergeich
    #-  kein Y_data und wir werten ueber Y_data aus daher
    Y_data <- X_data
    yname  <- names(Y_data)
    xname  <- NULL
    X_data <- NULL
    
  }
  
  list(
    X_data = X_data,
    Y_data = Y_data,
    xname = xname,
    yname = yname,
    Z_data = data_condition,
    zname = zname,
    formula = my_formula,
    condition = condition,
    formula.orginal = x,
    digits = fm$digits,
    type = fm$type
  )
}



#' @rdname Formula_Data
#' @description Formula_Names in model_info.formula verwendet
#' @export
#' @examples
#' names(Formula_Names(m1[3]+m2~geschl, varana))
#'
Formula_Names <- function(x, data = NULL, ...) {
  #- default einstellungen
  digits_pos <- stp25rndr:::default_stp25("digits", "mittelwert")
  
  type <- "auto"
  frml <- formula_split(x)
  # print(frml)
  x <- frml$formula
  #- Formel vom Type 'a+b[2]~c' kann auch  'a+b[2] ~ . ' sein
  if (any(all.names(x[[2L]]) %in% '[')) {
    # y_names_vars <- all.vars(x[[2L]])
    #hier war ein Fehler wegen  width.cutoff = 60L
    y_hsd <-  strsplit(deparse(x[[2L]],
                               width.cutoff = 500L), " \\+ ")[[1]]
    
    y_names <- gsub("\\[.+\\]", "", y_hsd) # bereinigen von Klammern
    
    if (length(x) == 2) {
      x <-  formula(paste("~",
                          paste(y_names, collapse = "+")))
    } else{
      x_hsd <-  strsplit(deparse(x[[3L]]), " \\+ ")[[1]]
      x_names <- gsub("\\[.+\\]", "", x_hsd)
      
      x <- formula(paste(
        paste(y_names, collapse = "+")
        ,
        "~",
        paste(x_names, collapse = "+")
      ))
    }
    
    pos <- grep("\\[", y_hsd)
    
    stat <- gsub("[^[:alpha:]]", "",
                 stringr::str_extract(y_hsd[pos],
                                      ",.+"))
    
    dig <- as.integer(gsub("[^0-9]", "",
                           stringr::str_extract(y_hsd[pos],
                                                "\\[.")))
    
    digits_pos <- lapply(1:length(y_names), function(j)
      digits_pos)
    type <- lapply(1:length(y_names), function(j)
      type)
    
    # print(stat)
    for (i in seq_len(length(pos))) {
      digits_pos[[pos[i]]] <- rep(dig[i], 2)
      type[[pos[i]]] <-  stat[i]
    }
  } #- end
  
  x <-  clean_dots_formula(x, data)
  
  ##  mf input: formula output: formula
  if (length(x) == 2)
    list(
      yname = all.vars(x[[2L]]),
      xname = NULL,
      zname = all.vars(frml$condition),
      formula = x,
      condition = frml$condition,
      digits = digits_pos,
      type = type
    )
  else if ((length(x) == 3))
    list(
      yname = all.vars(x[[2L]]),
      xname = all.vars(x[[3L]]),
      zname = all.vars(frml$condition),
      formula = x,
      condition = frml$condition,
      digits = digits_pos,
      type = type
    )

  else {
    warning("Weis nicht was tun - sorry!")
    list(
      yname = NULL,
      xname = NULL,
      zname = NULL,
      formula = x,
      condition = frml$condition,
      digits = digits_pos,
      type = type
    )
  }
}
