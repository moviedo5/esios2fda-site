#' @encoding UTF-8
#' @title Build daily curves into \code{ldata} object from ESIOS data
#'
#' @description
#'   Fetches ESIOS time-series for a specified resolution and date range via \code{esios2df()},
#'   then assembles a single \code{ldata} list: common metadata (daily index, date, leap, dst)
#'   plus one \code{fdata} per variable with daily curves.
#'
#' @param var_names   Character vector of indicator names in "ShortName_ID" format.
#' @param start_date  String "YYYY-MM-DD" interval start (Europe/Madrid).
#' @param end_date    String "YYYY-MM-DD" interval end (Europe/Madrid).
#' @param api_key     Your ESIOS API key.
#' @param resolution  Character: one of
#'                    \code{"1min","5min","10min","15min","30min",
#'                    "hour","1hour","4hour","1day"}.
#' @param verbose     Logical; if \code{TRUE}, prints progress messages.
#'
#' @return A \code{ldata} list: element \code{df} is a data.frame (index, date, leap, dst),
#'   followed by each variable as a daily \code{fdata} object.
#'
#' @importFrom lubridate ymd yday leap_year dst ymd_hms hour minute with_tz
#' @importFrom dplyr filter
#' @importFrom stats xtabs
#' @importFrom fda.usc fdata
#' @export
esios2lfdata <- function(var_names,
                         start_date,
                         end_date,
                         api_key,
                         resolution = c("1min","5min","10min","15min",
                                        "30min","hour","1hour","4hour","1day"),
                         verbose = FALSE) {
  resolution <- match.arg(resolution)
  tz         <- "Europe/Madrid"
  
  # Build master metadata
  d0   <- as.Date(start_date, tz = tz)
  d1   <- as.Date(end_date,   tz = tz)
  days <- seq(d0, d1, by = "day")
  nD   <- length(days)
  leap <- as.integer(leap_year(days))
  noon <- as.POSIXct(paste(days, "12:00:00"), tz = tz)
  dstf <- c(0, diff(as.integer(dst(noon))))
  df   <- data.frame(index = seq_len(nD), date = days,
                     leap = leap, dst = dstf,
                     stringsAsFactors = FALSE)
  
  # Determine slot settings
  numeric_min <- switch(resolution,
                        "hour"   = 60,
                        "1hour"  = 60,
                        "4hour"  = 240,
                        "1day"   = 1440,
                        as.numeric(sub("min", "", resolution)))
  slots   <- 1440 / numeric_min
  pos     <- (2*60)/numeric_min + 1
  argvals <- seq(0, slots-1) / slots * 24
  
  # Fetch all data via esios2df
  if (verbose) message("Fetching data via esios2df()...")
  ts_long <- esios2df(var_names, start_date, end_date,
                      api_key, resolution = resolution,
                      verbose = verbose)
  
  out <- list(df = df)
  
  for (var in var_names) {
    if (verbose) message("Processing ", var)
    sub <- ts_long %>% filter(var_name == var)
    if (nrow(sub) == 0) {
      warning(var, ": no data; skipping")
      next
    }
    
    # Build daily matrix
    m <- matrix(NA_real_, nrow = nD, ncol = slots)
    datetime <- sub$datetime
    values   <- sub$value
    day_i    <- as.integer(difftime(datetime, as.POSIXct(days[1], tz = tz), units = "days")) + 1
    minute_of_day <- hour(datetime)*60 + minute(datetime)
    slot_i   <- floor(minute_of_day / numeric_min) + 1
    ok <- which(day_i >=1 & day_i <= nD & slot_i >=1 & slot_i <= slots)
    m[cbind(day_i[ok], slot_i[ok])] <- values[ok]
    
    # Smooth DST anomalies
    while (ncol(m) < slots) {
      lo <- m[, pos-1]; hi <- m[, pos]
      m <- cbind(m[,1:(pos-1)], (lo+hi)/2, m[,pos:ncol(m)])
    }
    while (ncol(m) > slots) {
      m[,pos] <- (m[,pos] + m[,pos+1]) / 2
      m <- m[,-(pos+1)]
    }
    
    # Create fdata object
    fdobj <- fda.usc::fdata(m, argvals,
                            names = list(
                              main = var,
                              xlab = if (numeric_min==60) "Hour" else paste(numeric_min, "min intervals"),
                              ylab = "Value"
                            ))
    out[[var]] <- fdobj
  }
  
  class(out) <- c("ldata","list")
  invisible(out)
}
