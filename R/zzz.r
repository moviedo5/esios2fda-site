#' @import httr
#' @import jsonlite
#' @import lubridate
#' @import dplyr
#' @import fda.usc
#' @import parallel
#' @import foreach
#' @importFrom utils globalVariables read.csv2 write.csv2
#' @importFrom stats xtabs time ts     
#' @importFrom lubridate ymd_hms with_tz ymd hms yday hour minute seconds
#' @importFrom httr GET status_code timeout add_headers
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate transmute filter bind_rows %>%
#' @importFrom fda.usc fdata
#' @importFrom magrittr %>%
#' @importFrom foreach %dopar% getDoParWorkers getDoParRegistered getDoParName getDoParVersion registerDoSEQ setDoSeq setDoPar
#' @importFrom doParallel registerDoParallel
#' @importFrom iterators icount
#' @importFrom knitr knit_print
#' @importFrom rmarkdown html_vignette
NULL

utils::globalVariables(c(
  "datetime", "ts", "time", "value",
  "tm", "instant", "val",  
  "geocode", "geolabel",
  "timed", "hourd", "minuted"
))

