#' @encoding UTF-8
#' @title Read ESIOS CSVs or RDAs and build daily curves into \code{ldata} object
#'
#' @description
#'   Reads ESIOS CSVs or RDAs and builds a single \code{ldata} object:
#'   a list with a common metadata \code{data.frame} and one combined \code{fdata} per variable.
#'
#' @param years       Integer vector of years to process.
#' @param var_names   Character vector of variable names (e.g. "Solar_PV_119").
#' @param resolution  Character: "hour" (24 points/day) or "min" (144 points/day of 10-min intervals).
#' @param input_dir   Directory containing input CSVs or RDAs.
#' @param mode        Character: "csv" to read CSV files, "rda" to load existing RDA files.
#' @param output_dir  Directory to save generated \code{fdata} files; created if missing. Optional.
#' @param verbose     Logical; if TRUE, prints progress messages.
#'
#' @return A list of class c("ldata","list"): first element is the common metadata \code{df},
#'   then each variable as a combined \code{fdata} object.
#'
#' @importFrom lubridate yday hour minute leap_year ymd_hms dst
#' @importFrom stats xtabs
#' @importFrom fda.usc fdata
#' @importFrom dplyr mutate
#' @export
esios2ldata <- function(years, var_names, resolution = c("hour","min"),
                        input_dir, mode = c("csv","rda"),
                        output_dir = NULL, verbose = FALSE) {
  resolution <- match.arg(resolution)
  suffix     <- if (resolution == "min") "m" else "h"
  mode       <- match.arg(mode)
  tz         <- "Europe/Madrid"
  if (!is.null(output_dir) && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 1) Build master metadata once
  start_day <- as.Date(sprintf("%04d-01-01", min(years)), tz = tz)
  end_day   <- as.Date(sprintf("%04d-12-31", max(years)), tz = tz)
  days      <- seq(start_day, end_day, by = "day")
  nD        <- length(days)
  leap      <- as.integer(leap_year(days))
  noon      <- as.POSIXct(paste(days, "12:00:00"), tz = tz)
  dst_flag  <- c(0, diff(as.integer(dst(noon))))
  df_master <- data.frame(
    index = seq_len(nD),
    date  = days,
    leap  = leap,
    dst   = dst_flag,
    stringsAsFactors = FALSE
  )
  
  result <- list()
  
  for (var in var_names) {
    if (verbose) message("Processing ", var)
    mat_all  <- NULL
    skip_var <- FALSE
    
    for (yr in years) {
      base <- sprintf("%s_%s%04d", var, suffix, yr)
      
      if (mode == "csv") {
        csvf <- file.path(input_dir, paste0(base, ".csv"))
        df   <- tryCatch(
          read.csv2(csvf, stringsAsFactors = FALSE) %>%
            mutate(time = ymd_hms(instant, tz = tz)),
          error = function(e) {
            warning(sprintf("Variable '%s': file '%s' not found; skipping.", var, basename(csvf)))
            skip_var <<- TRUE
            return(NULL)
          }
        )
        if (skip_var) break
        
        df2 <- df %>% mutate(
          yeard   = yday(time),
          hourd   = hour(time),
          minuted = minute(time),
          grid    = if (resolution == "min")
            (hourd * 6 + minuted / 10 + 1)
          else
            (hourd + 1)
        )
        mat <- if (resolution == "min")
          xtabs(val ~ yeard + grid, df2)
        else
          xtabs(val ~ yeard + hourd, df2)
        
      } else {
        rdaf <- file.path(input_dir, paste0(base, ".rda"))
        obj  <- tryCatch(
          load(rdaf),
          error = function(e) {
            warning(sprintf("Variable '%s': file '%s' not found; skipping.", var, basename(rdaf)))
            skip_var <<- TRUE
            return(NULL)
          }
        )
        if (skip_var) break
        fd   <- get(obj)
        mat  <- fd$data
      }
      
      mat_all <- if (is.null(mat_all)) mat else rbind(mat_all, mat)
    }
    
    if (skip_var) next
    
    # 2) Check expected number of columns
    exp_cols <- if (resolution == "min") 144 else 24
    if (ncol(mat_all) != exp_cols) {
      warning(sprintf(
        "Variable '%s' does not have data at the requested resolution; its values are omitted.",
        var
      ))
      next
    }
    
    # 3) Create fdata and save
    argvals <- if (resolution == "min") seq(0, 143) / 144 * 24 else seq(0.5, 23.5, length = 24)
    fdobj   <- fdata(mat_all, argvals,
                     names = list(
                       main = var,
                       xlab = if (resolution == "min") "10-min intervals" else "Hours",
                       ylab = "Value"
                     ))
    if (!is.null(output_dir)) {
      save(fdobj, file = file.path(output_dir, paste0(var, "_", suffix, "ALL.rda")))
    }
    result[[var]] <- fdobj
  }
  
  out <- c(list(df = df_master), result)
  class(out) <- c("ldata", "list")
  invisible(out)
}





