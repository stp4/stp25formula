#' @rdname formula_helper
#' @description make_formula: Formel erstellen in \code{berechne_all(...)} verwendet. 
#' Hier wird \code{cbind(a,b,c)~g} ausgegebeb.
#' @param  measurevar,groupvars  mamen als strings
#' @export
#' @examples
#'
#'
#' make_formula("a", "b")
#' make_formula("a", c("b","c"))
#' make_formula("a", ~b+c)
#' make_formula(c("a", "d"), c("b","c"))
#' 
make_formula <- function(measurevar,
                        groupvars=NULL) {
  if (is.null(groupvars))
    return(formula(paste("~", paste(
      measurevar, collapse = "+"
    ))))
  
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


#' formel generieren
#'
#' in prepare_data2 
#'
#' @param measure.vars.group.vars,condition.vars mamen als strings
#'
#' @noRd
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
