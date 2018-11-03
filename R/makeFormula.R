#' @rdname formula_helper
#' @description makeFormula: Formel erstellen
#' @param  measurevar Variable  in makeFormula
#' @param groupvars Variable in makeFormula
#' @return makeFormula: formula - Objekt
#' @export
#' @examples
#'
#'
#' makeFormula("a", "b")
#' makeFormula("a", c("b","c"))
#' makeFormula("a", ~b+c)
#' makeFormula(c("a", "d"), c("b","c"))
#'
makeFormula <- function(measurevar,
                        groupvars) {
  if (is_formula2(groupvars))
    groupvars <- paste0(all.vars(groupvars), collapse = "+")
  else
    groupvars <- paste0(groupvars, collapse = "+")
  
  if (is_formula2(groupvars)) {
    measurevar <- all.vars(measurevar)
    if (length(measurevar) != 1)
      measurevar <-
        paste("cbind(", paste0(measurevar, collapse = ", "), ")")
  }
  else {
    if (length(measurevar) != 1)
      measurevar <-
        paste("cbind(", paste0(measurevar, collapse = ", "), ")")
  }
  
  formula(paste(measurevar, "~", groupvars))
}

to_formula <-
  function(measure.vars,
           group.vars,
           condition.vars = NULL) {
    if (is.null(group.vars)) {
      fm <- paste0("~", paste(measure.vars, collapse = "+"))
    }
    else if (group.vars[1] == "1") {
      fm <- paste0("~", paste(measure.vars, collapse = "+"))
    }
    else {
      if (is_formula2(group.vars))
        fm <- paste0(paste(measure.vars, collapse = "+"),
                     "~",
                     paste(all.vars(group.vars), collapse = "+"))
      else
        fm <- paste0(paste(measure.vars, collapse = "+"),
                     "~",
                     paste(group.vars, collapse = "+"))
    }
    
    formula(fm)
  }
