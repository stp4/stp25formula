# stp25DataObjekt
#
# Ueberprüft die Objektstrucktur also ob
#  measure usw vorhanden ist.
#  Die Uebergebenen Daten werden falls notwendig transformiert.

# @param data data.frame (bereinigt)
# @param measure.vars,group.vars,condition.vars String
# @param formula,by neu aus den Daten erstellt (.~)
# @param measure,digits aus Formula und Daten erstellt
# @param row_name,col_name labels
# @param measure.class,group.class,condition.class abweichend von measure die Classen
# 
# @noRd
# stp25DataObjekt <- function(data = NULL,
#                             measure.vars = NULL,
#                             group.vars = NULL,
#                             condition.vars = NULL,
#                             formula = NULL,
#                             by = NULL,
#                             measure = NULL,
#                             row_name = NULL,
#                             col_name = NULL,
#                             measure.class = NULL,
#                             group.class = NULL,
#                             condition.class = NULL,
#                             digits = NULL) {
#   
#   if (!is.null(measure.vars)) {
#     if (is.null(measure.class))
#       measure.class <- get_classes(data[measure.vars])
#     if (is.null(measure))
#       measure  <- measure.class
#     if (is.null(row_name))
#       row_name <- stp25aggregate::GetLabelOrName(data[measure.vars])
#     if (is.null(digits))
#       digits <- ifelse(
#         measure == "factor",
#         stp25rndr::default_stp25("digits", "prozent"),
#         stp25rndr::default_stp25("digits", "mittelwert")
#       )
#   }
#   
#   if (!is.null(group.vars)) {
#     if (is.null(group.class))
#       group.class <- get_classes(data[group.vars])
#     
#     if (is.null(col_name))
#       col_name <- stp25aggregate::GetLabelOrName(data[group.vars])
#     
#   } else{
#     group.class <- col_name <- NULL
#   }
#   
#   if (!is.null(condition.vars) & length(condition.vars > 0)) {
#     if (is.null(condition.class))
#       condition.class <- get_classes(data[condition.vars])
#   } else{
#     condition.class <- condition.vars <- NULL
#   }
#   
#   if (is.null(by)) {
#     if (is.null(group.vars))
#       by <- "1"
#     else{
#       by <- formula(paste("~", paste(group.vars, collapse = "+")))
#     }
#   }
#   
#   if (is.null(formula)) {
#     formula <- to_formula(measure.vars, group.vars, condition.vars)
#   }
#   
#   measure_class <- ifelse(measure == "mean", "numeric",
#                    ifelse(measure == "mean", "numeric",
#                    ifelse(measure == "freq", "factor",
#                    measure)
#                           ))
#   if (!all(measure.class == measure_class)) {
#     x <- which(!measure.class == measure_class)
#     for (i in x) {
#       #  print(i)
#       if (measure_class[i] == "numeric") {
#         data[[i]] <- as.numeric(data[[i]])
#       }
#       else if (measure_class[i] == "factor") {
#         data[[i]] <- factor(data[[i]])
#       }
#       measure.class[i] <- measure_class[i]
#     }
#   }
#  
#   res <- list(
#     data = data,
#     measure.vars = measure.vars,
#     group.vars = group.vars,
#     condition.vars = condition.vars,
#     formula = formula,
#     by = by,
#     measure = measure,
#     row_name = row_name,
#     col_name = col_name,
#     measure.class = measure.class,
#     group.class = group.class,
#     condition.class = condition.class,
#     digits = digits,
#     N = nrow(data)
#   )
#   class(res) <- c("stp25data", class(res))
#   res
# }


#' Print Methode
#' 
#' @export
#' @noRd
print.stp25data <- function(x, ...) {
  cat("\nformula: ")
  print(x$formula)
  cat("\nmeasure.vars: ", paste(x$measure.vars, collapse = ", "))
  cat("\nmeasure: ", paste(x$measure , collapse = ", "))
  cat("\nmeasure.class: ", paste(x$measure.class , collapse = ", "))
  cat("\ndigits: ", paste(x$digits, collapse = ", "))
  cat("\nrow_name: ", paste(x$row_name, collapse = ", "))
  cat("\nby: ")
  print(x$by)
  cat("\ngroup.vars: ", paste(x$group.vars, collapse = ", "), "\n")
  
  #  cat("\ncol_name: ", paste(x$col_name, collapse=", "),"\n")
  print(head(x$data))
}
