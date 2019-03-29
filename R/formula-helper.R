 
#' Formel-Objekte bearbeiten
#'
#' Formulas 
#'
#' @param ... weitere einstellungen
#'
#' @return liste 
#' @export
#'
formula_helper <- function(...) {
  formula(...)
}



#' @rdname formula_helper
#' @param x  Formula
#' @param data,names_data data.frame
#' 
#' @description clean_dots_formula: Formel bereinigen
#' @return clean_dots_formula: formula - Objekt
#' 
#' @examples
#'
#' data <- data.frame(x = NA, y = NA, z = NA)
#' stp25formula:::clean_dots_formula(x ~ y, data)
#' stp25formula:::clean_dots_formula(. ~ x + y, data)
#' stp25formula:::clean_dots_formula(x + y ~ ., data)
#' stp25formula:::clean_dots_formula(~., data)
#' stp25formula:::formula_split(a+b~x|y)
#'
clean_dots_formula <- function(x,
                               data = NULL,
                               names_data = names(data)) {
  myvars <- all.vars(x)
  
  if (any(myvars %in% ".")) {
    if (length(myvars) == 1) {
      return(formula(paste(
        " ~ ", paste(names_data, collapse = "+")
      )))
    } else if (myvars[1] == ".") {
      var_dots <- names_data[!names_data %in% myvars[-1]]
      return(formula(paste(
        paste(var_dots, collapse = "+"),
        " ~ ",
        paste(myvars[-1], collapse = "+")
      )))
    } else if (myvars[length(myvars)] == ".") {
      var_dots <- names_data[!names_data %in% myvars[-length(myvars)]]
      return(formula(paste(
        paste(myvars[-length(myvars)],
              collapse = "+"),
        " ~ ",
        paste(var_dots, collapse = "+")
      )))
    }
    
  } else {
    return(x)
  }
}



#' @rdname formula_helper
#' @description formula_split stolen from mosaic ggformula
#' @return formula_split: liste  formula, condition, facet_type
#'
formula_split <- function(x) {
  # split A | B into formula <- A; condition <- B
  fs <-
    stringr::str_split(paste(deparse(x), collapse=""), "\\|")[[1]]
  # try to split, else leave formula unchanged and set condition to NULL
  if ((length(fs) != 2) ||
      !tryCatch({
        formula_string <- fs[1]
        condition_string <- fs[2]
        if (!grepl("~", condition_string)) {
          condition_string <- paste0("~", condition_string)
          condition <-
            as.formula(condition_string, env = environment(x))
          facet_type <- "facet_wrap"
        } else {
          condition <-
            as.formula(condition_string, env = environment(x))
          facet_type <- "facet_grid"
        }
        x <-
          as.formula(formula_string, env = environment(x))
        TRUE
      }
      , error = function(e) {
        warning(e)
        FALSE
      })) {
    condition <- NULL
    facet_type <- "none"
  }
  list(formula = x,
       condition = condition,
       facet_type = facet_type)
}




#' @param x A object to be tested
#' @noRd
is_formula2 <- function (x) {
  inherits(x, "formula")
}



#' @param x A object to be tested
#' @noRd
is_empty2 <- function (x) {
  if (length(x) == 0)
    TRUE
  else if (length(x) == 1) {
    if (is.null(x))
      TRUE
    else if (is.na(x))
      TRUE
    else if (x == "")
      TRUE
    else
      FALSE
  }
  else
    FALSE
}
