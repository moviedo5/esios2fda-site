#' @encoding UTF-8
#' @title Fetch ESIOS Time-Series and Save as CSV Files
#'
#' @description
#'   \code{esios2csv} downloads raw ESIOS indicator data from Spain's API at
#'   specified temporal resolution and writes individual CSV files per variable
#'   and year, named <var_name><suffix><year>.csv. Supported resolutions are:
#'   \itemize{
#'     \item \code{"1min"}: 1-minute
#'     \item \code{"5min"}: 5-minute
#'     \item \code{"10min"}: 10-minute
#'     \item \code{"15min"}: 15-minute
#'     \item \code{"30min"}: 30-minute
#'     \item \code{"hour"} / \code{"1hour"}: hourly
#'     \item \code{"4hour"}: 4-hour
#'     \item \code{"1day"}: daily
#'   }
#'   It enforces the \code{instant} column format ("YYYY-MM-DD HH:MM:SS" in Europe/Madrid),
#'   handles large intervals by recursively splitting on HTTP 504, and issues
#'   warnings on errors.
#'
#' @param var_names  Character vector of indicator names in "ShortName_ID" format.
#' @param years      Integer vector of years to download (e.g. \code{2023:2024}).
#' @param api_key    String: your personal ESIOS API key.
#' @param resolution Character: one of
#'   \code{"1min","5min","10min","15min","30min","hour","1hour","4hour","1day"}.
#' @param output_dir String: directory to save CSV files (created if missing).
#' @param verbose    Logical; if \code{TRUE}, prints progress messages.
#' @param encoding   Character: JSON encoding for API responses (default "UTF-8").
#'
#' @return Invisibly, a character vector of saved file paths.
#'
#' @importFrom httr GET add_headers timeout status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate ymd_hms with_tz seconds
#' @importFrom dplyr mutate transmute
#' @export

esios2csv <- function(var_names, years, api_key,
                      resolution = c("1min","5min","10min","15min","30min","hour","1hour"),
                      output_dir, verbose = FALSE, encoding = "UTF-8") {
  resolution <- match.arg(resolution)
  tz <- "Europe/Madrid"
  
  suffix_map <- c(
    "1min"  = "1m",  "5min"  = "5m",  "10min" = "10m",
    "15min" = "15m", "30min" = "30m", "hour"  = "h",  "1hour" = "h"
  )
  trunc_map <- c(
    "1min"  = "minute",       "5min"  = "five_minutes",
    "10min" = "ten_minutes",  "15min" = "fifteen_minutes",
    "30min" = "thirty_minutes","hour"  = "hour",         "1hour" = "hour"
  )
  suffix     <- suffix_map[[resolution]]
  time_trunc <- trunc_map[[resolution]]
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Helper: recursively fetch values, splitting on 504
  fetch_range <- function(var_id, s_dt, e_dt) {
    start_iso <- format(lubridate::with_tz(s_dt, "UTC"), "%Y-%m-%dT%H:%M:%SZ")
    end_iso   <- format(lubridate::with_tz(e_dt, "UTC"), "%Y-%m-%dT%H:%M:%SZ")
    url <- sprintf(
      "https://api.esios.ree.es/indicators/%d?start_date=%s&end_date=%s&time_trunc=%s&locale=es",
      var_id, start_iso, end_iso, time_trunc
    )
    resp <- tryCatch(
      httr::GET(url,
                httr::add_headers(
                  Accept      = "application/json; application/vnd.esios-api-v1+json",
                  `Content-Type` = "application/json",
                  `x-api-key` = api_key
                ),
                httr::timeout(120)),
      error = function(e) e
    )
    if (inherits(resp, "error")) return(NULL)
    code <- httr::status_code(resp)
    if (code == 504) {
      # split interval
      mids <- s_dt + (e_dt - s_dt) / 2
      df1 <- fetch_range(var_id, s_dt, mids)
      df2 <- fetch_range(var_id, mids + lubridate::seconds(1), e_dt)
      if (is.null(df1) && is.null(df2)) return(NULL)
      return(rbind(df1, df2))
    }
    if (code != 200) return(NULL)
    parsed <- tryCatch(
      jsonlite::fromJSON(httr::content(resp, "text", encoding = encoding), simplifyDataFrame = TRUE),
      error = function(e) NULL
    )
    if (is.null(parsed$indicator$values)) return(NULL)
    as.data.frame(parsed$indicator$values, stringsAsFactors = FALSE)
  }
  
  saved <- character()
  for (var_name in var_names) {
    var_id <- as.integer(sub(".*_(\\d+)$", "\\1", var_name))
    if (is.na(var_id)) {
      warning(sprintf("Invalid var '%s' (cannot extract ID); skipping.", var_name))
      next
    }
    for (yr in years) {
      if (verbose) message(sprintf("Processing '%s' (ID=%d) for %d...", var_name, var_id, yr))
      s_dt <- as.POSIXct(sprintf("%d-01-01 00:00:00", yr), tz = tz)
      e_dt <- as.POSIXct(sprintf("%d-12-31 23:59:59", yr), tz = tz)
      df_raw <- fetch_range(var_id, s_dt, e_dt)
      if (is.null(df_raw) || nrow(df_raw)==0) {
        warning(sprintf("'%s' has no %s data for %d; skipping.", var_name, resolution, yr)); next
      }
      # ensure datetime
      if (!"datetime" %in% names(df_raw) && "date" %in% names(df_raw)) {
        df_raw$datetime <- paste0(df_raw$date, "T00:00:00Z")
      }
      # parse timestamps
      tm <- tryCatch(
        lubridate::ymd_hms(df_raw$datetime, tz = "UTC") %>% lubridate::with_tz(tz),
        error = function(e) NULL
      )
      if (is.null(tm)) { warning(sprintf("'%s': timestamp parse error; skipping.")); next }
      
      df_out <- df_raw %>%
        dplyr::mutate(
          instant = format(tm, "%Y-%m-%d %H:%M:%S")
        ) %>%
        dplyr::transmute(
          instant = instant,
          val     = value,
          geocode = if ("geocode" %in% names(df_raw)) geocode else NA_integer_,
          geolabel = if ("geolabel" %in% names(df_raw)) geolabel else NA_character_
        )
      fn <- file.path(output_dir, sprintf("%s_%s%04d.csv", var_name, suffix, yr))
      tryCatch({
        write.csv2(df_out, fn, row.names = FALSE, quote = TRUE)
        saved <<- c(saved, fn)
        if (verbose) message(sprintf("  Saved %s (%d rows)", basename(fn), nrow(df_out)))
      }, error = function(e) warning(sprintf("Failed to save '%s': %s", fn, e$message)))
    }
  }
  invisible(saved)
}
