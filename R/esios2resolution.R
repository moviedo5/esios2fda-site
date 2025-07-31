#' @encoding UTF-8
#' @title Detect Supported Time Resolutions for ESIOS Indicators
#'
#' @description
#' Adds binary flags to an ESIOS indicators metadata \code{data.frame} denoting
#' whether each indicator supports various time-series resolutions.  
#' Supported resolutions include: 1 minute, 5 minutes, 10 minutes, 15 minutes,
#' 30 minutes, 1 hour, 4 hours, and 1 day. The function samples a specified date
#' (default: yesterday) and queries the ESIOS API to inspect actual timestamp
#' intervals.
#'
#' @param indicators A \code{data.frame} as returned by \code{esios2indicators()}. 
#'   If \code{NULL}, the full indicator catalog will be fetched automatically.
#' @param api_key    String: your personal ESIOS API key.
#' @param encoding   Character: JSON encoding for API responses (default: "UTF-8").
#' @param check_date Date or string "YYYY-MM-DD" used for sampling; defaults
#'   to \code{Sys.Date() - 1}.
#' @param verbose    Logical; if \code{TRUE}, progress messages are printed.
#' @param csv_file   Optional string path: if provided, writes the annotated
#'   metadata to this CSV file (directories are created as needed).
#'
#' @return The input \code{indicators} \code{data.frame} extended with integer
#'   columns \code{res1min}, \code{res5min}, \code{res10min}, \code{res15min},
#'   \code{res30min}, \code{res1hour}, \code{res4hour}, and \code{res1day},
#'   each set to 1 if the indicator supports that resolution, or 0 otherwise.
#'
#' @examples
#' \dontrun{
#' # Annotate only Wind_551
#' api_key <- Sys.getenv("ESIOS_API_KEY")
#' inds <- esios2indicators(api_key, output_dir = "data_indicators")
#' res  <- esios2resolution(
#'   indicators = inds[inds$short_name_en == "Wind_551", ],
#'   api_key     = api_key,
#'   verbose     = TRUE
#' )
#' print(res[, c("short_name_en", "res5min", "res10min", "res1hour")])
#' }
#'
#' @importFrom httr GET add_headers timeout status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate with_tz ymd_hms
#' @export
esios2resolution <- function(indicators = NULL,
                             api_key,
                             encoding   = "UTF-8",
                             check_date = NULL,
                             verbose    = FALSE,
                             csv_file   = NULL) {
  # 1) metadatos
  if (is.null(indicators)) {
    if (verbose) message("Descargando catálogo completo...")
    indicators <- esios2indicators(api_key, encoding = encoding)
  }
  # 2) fecha de muestreo
  if (is.null(check_date)) check_date <- Sys.Date() - 1
  cd <- as.Date(check_date)
  tz <- "Europe/Madrid"
  start_dt <- as.POSIXct(paste0(cd, " 00:00:00"), tz = tz)
  end_dt   <- as.POSIXct(paste0(cd, " 23:59:59"), tz = tz)
  
  # 3) segundos esperados según trunc
  exp_secs <- c(
    minute          = 60,
    five_minutes    = 5 * 60,
    ten_minutes     = 10 * 60,
    fifteen_minutes = 15 * 60,
    thirty_minutes  = 30 * 60,
    hour            = 3600,
    four_hours      = 4 * 3600,
    day             = 24 * 3600
  )
  to_iso_utc <- function(dt) {
    format(lubridate::with_tz(dt, "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  }
  
  # 4) helper robusto para extraer vectores de intervalos (secs)
  fetch_vals <- function(id, trunc) {
    if (!(trunc %in% names(exp_secs))) return(NULL)
    url <- sprintf(
      "https://api.esios.ree.es/indicators/%d?start_date=%s&end_date=%s&time_trunc=%s&locale=es",
      id, to_iso_utc(start_dt), to_iso_utc(end_dt), trunc
    )
    resp <- tryCatch(
      httr::GET(url,
                httr::add_headers(
                  Accept      = "application/json; application/vnd.esios-api-v1+json",
                  `x-api-key` = api_key
                ),
                httr::timeout(30)),
      error = function(e) NULL
    )
    if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
    j <- tryCatch(
      jsonlite::fromJSON(httr::content(resp, "text", encoding = encoding),
                         simplifyDataFrame = TRUE),
      error = function(e) NULL
    )
    df <- j$indicator$values
    if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
    if (!"datetime" %in% names(df) && "date" %in% names(df)) {
      df$datetime <- paste0(df$date, "T00:00:00Z")
    }
    times <- lubridate::ymd_hms(df$datetime, tz = tz)
    as.numeric(diff(times), units = "secs")
  }
  
  # 5) inicializar flags
  n <- nrow(indicators)
  flags <- c("res1min","res5min","res10min","res15min","res30min","res1hour","res4hour","res1day")
  indicators[flags] <- 0L
  
  # 6) loop sobre cada indicador y cada trunc
  for (i in seq_len(n)) {
    vid  <- indicators$id[i]
    name <- indicators$short_name_en[i]
    if (verbose) message("Muestreando ID=", vid, " (", name, ")...")
    for (tr in names(exp_secs)) {
      iv <- fetch_vals(vid, tr)
      if (is.null(iv)) next
      # existe al menos un intervalo idéntico al esperado?
      if (any(abs(iv - exp_secs[tr]) < 1e-6)) {
        col <- switch(tr,
                      minute          = "res1min",
                      five_minutes    = "res5min",
                      ten_minutes     = "res10min",
                      fifteen_minutes = "res15min",
                      thirty_minutes  = "res30min",
                      hour            = "res1hour",
                      four_hours      = "res4hour",
                      day             = "res1day"
        )
        indicators[[col]][i] <- 1L
      }
    }
  }
  
  # 7) volcar CSV si pide
  if (!is.null(csv_file)) {
    dir <- dirname(csv_file)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    write.csv2(indicators, file = csv_file, row.names = FALSE, quote = TRUE)
    if (verbose) message("Guardado CSV en ", csv_file)
  }
  
  invisible(indicators)
}
