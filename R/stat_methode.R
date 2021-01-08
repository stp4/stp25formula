# stat_methode <- function(x,
#                          my_met = c(),
#                          my_stat = c(
#                            "spearman2",
#                            "chisq",
#                            "fisher",
#                            "wilcox",
#                            "utest",
#                            "htest",
#                            "kruskal",
#                            "ttest",
#                            "aov",
#                            "anova",
#                            "Hmisc"
#                          )) {
#   search.string.met <-
#     paste0(my_met, collapse = "|") # Construct the reg. exprs.
#   search.string.stat <-
#     paste0(my_stat, collapse = "|") # Construct the reg. exprs.
#   x <- tolower(x)
#   list(
#     stringr::str_extract(x, search.string.met),
#     stringr::str_extract(x, search.string.stat)
#   )
#   
# }


# stp25_stat_methode <- function(x,
#                                mymet = c("freq",
#                                          "mean",
#                                          "median",
#                                          "multiresponse",
#                                          "pie",
#                                          "dot",
#                                          "hist",
#                                          "box",
#                                          "bar"
#                                          
#                                )) {
#   mymet[match(x, mymet)]
# }



#' stp25_stat_methode  
#'
#' in Tabelle verwendet
#'
#' @param x string
#' @param search_string Meine Methoden
#'
#' @noRd
stp25_stat_methode <- function(x,
                               search_string =
                                 c(
                                   "freq",
                                   "mean",
                                   "median",
                                   "multiresponse",
                                   "multi",
                                   "pie",
                                   "dot",
                                   "hist",
                                   "box",
                                   "bar"
                                 )) {
  stringr::str_extract(tolower(x), paste0(search_string, collapse = "|"))
}


stp25_test_methode <- function(x,
                               search_string = stp25stat:::stattest) {
  rslt <-
    stringr::str_extract(tolower(x), paste0(search_string, collapse = "|"))
  if (all(is.na(rslt)))
    NULL
  else
    rslt
}



# stp25_test_methode <- function(x,
#                                search_string =
#                                  c("contest",
#                                    "cattest",
#                                    "ordtest",
#                                    "notest",
#                                    "spearman2",
#                                    "chisq",
#                                    "fisher",
#                                    "wilcox",
#                                    "utest",
#                                    "htest",
#                                    "kruskal",
#                                    "ttest",
#                                    "aov",
#                                    "anova",
#                                    "hmisc"
#                                  )
#                                
# ) {
#   rslt <- stringr::str_extract(tolower(x), paste0(search_string, collapse = "|"))
#   if(all(is.na(rslt))) NULL else rslt
# }


# stat_methode("mean")
# stat_methode("mean, wilcox")
# 
# 
# formula <-
#   m1[mean, 1] + m2[1, freq, wilcox] + m3[3, Median] + m4[4] ~ sex
# measure.vars <- c("m1", "m2", "m3", "m4", "sex")
# 
# y_hsd <-
#   gsub(" ", "", paste(deparse(formula[[2L]]), collapse = ""))
# y_hsd <- strsplit(y_hsd, "\\+")[[1]]
# 
# measure.vars <-
#   gsub("\\[.+\\]", "", y_hsd) # bereinigen von Klammern
# measure <- as.character(rep(NA, length(measure.vars)))
# digits <- as.integer(rep(NA, length(measure.vars)))
# names(digits) <- measure.vars
# names(measure) <- measure.vars
# 
# # Dedect Position
# pos <- grep('\\[', y_hsd)
# # dedect_string afer ,  var[2,median]  gsub("[^[:alpha:]]", "", "var[2,median]")
# dedect_string <- gsub("[^[:alpha:]]", "",
#                       stringr::str_extract(y_hsd[pos], "\\[.+"))
# # return:"mean"   "freq"   "median" NA
# dedect_string_test <- stp25_test_methode(dedect_string)
# dedect_string <-  stp25_stat_methode(dedect_string)
# 
# dedect_number <- as.integer(gsub("[^0-9]", "",
#                                  stringr::str_extract(y_hsd[pos], "\\[.+")))
# 
# 
# dedect_string_test
# dedect_string