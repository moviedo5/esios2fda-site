#' @encoding UTF-8
#' @title Fetch ESIOS Time-Series as Data Frame
#'
#' @description
#'   \code{esios2df} retrieves raw time-series data for one or more ESIOS indicators
#'   over a specified date-time interval and returns a consolidated \code{data.frame}
#'   with one row per timestamp. Supported resolutions include:
#'   \itemize{
#'     \item \code{"1min"}: 1 minute
#'     \item \code{"5min"}: 5 minutes
#'     \item \code{"10min"}: 10 minutes
#'     \item \code{"15min"}: 15 minutes
#'     \item \code{"30min"}: 30 minutes
#'     \item \code{"1hour"}: 1 hour
#'     \item \code{"4hour"}: 4 hours
#'     \item \code{"1day"}: 24 hours
#'   }
#'
#' @param var_names  Character vector of indicators in "ShortName_ID" format.
#' @param start_date Character string defining the interval start ("YYYY-MM-DD" or
#'   "YYYY-MM-DD HH:MM:SS").
#' @param end_date   Character string defining the interval end (same formats as start_date).
#' @param api_key    Your personal ESIOS API key.
#' @param resolution Character: one of
#'   \code{"1min","5min","10min","15min","30min","1hour","4hour","1day"}.
#' @param verbose    Logical; if \code{TRUE}, prints progress messages.
#' @param encoding   Character: JSON encoding (default "UTF-8").
#'
#' @return A \code{data.frame} with columns:
#'   \describe{
#'     \item{var_name}{Indicator name as character}
#'     \item{datetime}{POSIXct timestamp in Europe/Madrid timezone}
#'     \item{value}{Numeric value}
#'     \item{geocode}{Integer geo-code or \code{NA}}
#'     \item{geolabel}{Character geo-label or \code{NA}}
#'   }
#'
#' @importFrom httr GET add_headers timeout status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate ymd ymd_hms with_tz hour minute second
#' @importFrom dplyr bind_rows transmute
#' @export
esios2df <- function(var_names,
                     start_date,
                     end_date,
                     api_key,
                     resolution = c("1min","5min","10min","15min","30min","1hour","4hour","1day"),
                     verbose = FALSE,
                     encoding = "UTF-8") {
  resolution <- match.arg(resolution)
  tz <- "Europe/Madrid"
  
  # Map resolution labels to API time_trunc and expected seconds
  trunc_map <- list(
    "1min"  = "minute",
    "5min"  = "five_minutes",
    "10min" = "ten_minutes",
    "15min" = "fifteen_minutes",
    "30min" = "thirty_minutes",
    "1hour" = "hour",
    "4hour" = "four_hours",
    "1day"  = "day"
  )
  exp_sec_map <- c(
    "1min"  = 60,
    "5min"  = 5 * 60,
    "10min" = 10 * 60,
    "15min" = 15 * 60,
    "30min" = 30 * 60,
    "1hour" = 3600,
    "4hour" = 4 * 3600,
    "1day"  = 24 * 3600
  )
  
  # Helper to parse dates
  parse_dt <- function(x, is_start = TRUE) {
    if (nchar(x) == 10) {
      dt <- lubridate::ymd(x, tz = tz, quiet = TRUE)
      if (!is_start) dt <- dt + hours(23) + minutes(59) + seconds(59)
      dt
    } else {
      lubridate::ymd_hms(x, tz = tz, quiet = TRUE)
    }
  }
  start_dt <- parse_dt(start_date, TRUE)
  end_dt   <- parse_dt(end_date, FALSE)
  
  to_iso_utc <- function(dt) format(with_tz(dt, "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  
  fetch_range <- function(vid, s, e, trunc) {
    GET(
      sprintf(
        "https://api.esios.ree.es/indicators/%d?start_date=%s&end_date=%s&time_trunc=%s&locale=es",
        vid, to_iso_utc(s), to_iso_utc(e), trunc
      ),
      add_headers(
        Accept        = "application/json; application/vnd.esios-api-v1+json",
        `Content-Type`= "application/json",
        `x-api-key`   = api_key
      ),
      timeout(120)
    )
  }
  
  out_list <- list()
  for (var in var_names) {
    if (verbose) message("Fetching ", var, " @ ", resolution)
    if (!grepl("_\\d+$", var)) stop("Invalid format: ", var)
    vid   <- as.integer(sub(".*_(\\d+)$", "\\1", var))
    trunc <- trunc_map[[resolution]]
    
    resp <- fetch_range(vid, start_dt, end_dt, trunc)
    if (status_code(resp) == 504) {
      mid <- start_dt + (end_dt - start_dt) / 2
      r1 <- fetch_range(vid, start_dt, mid, trunc)
      r2 <- fetch_range(vid, mid + seconds(1), end_dt, trunc)
      stopifnot(status_code(r1) == 200, status_code(r2) == 200)
      j1 <- fromJSON(content(r1, "text", encoding = encoding), simplifyDataFrame = TRUE)
      j2 <- fromJSON(content(r2, "text", encoding = encoding), simplifyDataFrame = TRUE)
      raw <- bind_rows(j1$indicator$values, j2$indicator$values)
    } else if (status_code(resp) == 200) {
      j   <- fromJSON(content(resp, "text", encoding = encoding), simplifyDataFrame = TRUE)
      raw <- j$indicator$values
    } else {
      stop(sprintf("HTTP %d for %s", status_code(resp), var))
    }
    
    df <- as.data.frame(raw, stringsAsFactors = FALSE)
    if (!"datetime" %in% names(df) && "date" %in% names(df)) {
      df$datetime <- paste0(df$date, "T00:00:00Z")
    }
    iso <- grepl("^\\d{4}-\\d{2}-\\d{2}$", df$datetime)
    df$datetime[iso] <- paste0(df$datetime[iso], "T00:00:00Z")
    
    df2 <- df %>% transmute(
      var_name = var,
      datetime = ymd_hms(datetime, tz = tz, quiet = TRUE),
      value    = value,
      geocode  = if ("geocode" %in% names(df)) geocode else NA_integer_,
      geolabel = if ("geolabel" %in% names(df)) geolabel else NA_character_
    )
    
    # Dynamic alignment check
    iv  <- as.numeric(diff(df2$datetime), units = "secs")
    ok1 <- length(iv) > 0 && all(abs(iv - exp_sec_map[[resolution]]) < 1e-6)
    dt0 <- df2$datetime[1]
    sec0 <- second(dt0) + 60 * minute(dt0) + 3600 * hour(dt0)
    ok2 <- (sec0 %% exp_sec_map[[resolution]]) == 0
    if (!ok1 || !ok2) {
      warning(sprintf("'%s' not aligned to %s; omitting.", var, resolution))
      next
    }
    
    out_list[[var]] <- df2
  }
  
  bind_rows(out_list)
}
