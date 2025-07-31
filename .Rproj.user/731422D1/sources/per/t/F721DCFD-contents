#' @title Annotate Dates or Date-Times with Calendar Attributes
#'
#' @description
#'   Converts a vector of POSIXct date-times —or a data.frame with a `date`
#'   column— into a data.frame enriched with calendar and temporal attributes
#'   (year, month, day-of-week, DST flags, holidays, etc.). If input is a
#'   data.frame containing a column `date` (of class Date or POSIXct), the
#'   output will be that same data.frame with the new attributes bound.
#'
#' @param datetimes Either a POSIXct (or coercible) vector of date-time stamps,
#'   or a data.frame that includes a column named `date` (Date or POSIXct).
#' @param tz         Character string; target time zone for all date-time ops
#'   (default: "Europe/Madrid").
#' @param components Character vector specifying which attributes to include.
#'   Supported names:
#'   * "year": calendar year
#'   * "month": month (January–December)
#'   * "day_of_year": day of year (1–365/366)
#'   * "day_of_month": day of month (1–31)
#'   * "day_of_week": day of week (Monday–Sunday, 1–7)
#'   * "is_leap": leap year flag (0/1)
#'   * "is_dst": in DST flag (0/1)
#'   * "dst_change": DST transition at this timestamp (0/1)
#'   * "is_easter": Easter Sunday (0/1)
#'   * "is_good_friday": Good Friday (0/1)
#'   * "is_easter_mo": Easter Monday (0/1)
#'   * "is_holiday": national holiday (0/1)
#'   * "laborable": non-working day (weekend or holiday) (0/1)
#'
#' @return
#'   If `datetimes` is a vector, returns a data.frame with a column
#'   `datetime` and one column per requested component.  
#'   If `datetimes` is a data.frame with column `date`, returns that
#'   data.frame with the new attribute columns appended.
#'
#' @importFrom lubridate year month yday mday wday leap_year dst with_tz
#' @export
date2calendar <- function(datetimes,
                          tz = "Europe/Madrid",
                          components = c("year","month","day_of_year",
                                         "day_of_month","day_of_week",
                                         "is_leap","is_dst","dst_change",
                                         "is_easter","is_good_friday",
                                         "is_easter_mo","is_holiday",
                                         "laborable")) {
  # If a data.frame with 'date' column is passed, extract it and remember df
  input_df <- NULL
  if (is.data.frame(datetimes) && "date" %in% names(datetimes)) {
    input_df <- datetimes
    datetimes <- datetimes$date
  }
  # Coerce to POSIXct in target tz
  dt <- with_tz(as.POSIXct(datetimes), tz)
  dates <- as.Date(dt)
  yrs   <- unique(year(dt))
  
  # Compute Easter and related movable holidays
  easter_date <- function(y) {
    a <- y %% 19; b <- y %/% 100; c <- y %% 100
    d <- b %/% 4; e <- b %% 4; f <- (b + 8) %/% 25; g <- (b - f + 1) %/% 3
    h <- (19*a + b - d - g + 15) %% 30
    i <- c %/% 4; k <- c %% 4
    L <- (32 + 2*e + 2*i - h - k) %% 7
    m <- (a + 11*h + 22*L) %/% 451
    month <- (h + L - 7*m + 114) %/% 31
    day   <- ((h + L - 7*m + 114) %% 31) + 1
    as.Date(sprintf("%04d-%02d-%02d", y, month, day))
  }
  easters  <- unlist(lapply(yrs, easter_date))
  goodFri  <- easters - 2
  easterMo <- easters + 1
  fixed    <- as.Date(c(paste0(yrs, "-01-01"), paste0(yrs, "-01-06"),
                        paste0(yrs, "-05-01"), paste0(yrs, "-08-15"),
                        paste0(yrs, "-10-12"), paste0(yrs, "-11-01"),
                        paste0(yrs, "-12-06"), paste0(yrs, "-12-08"),
                        paste0(yrs, "-12-25")))
  all_hols <- unique(c(easters, goodFri, easterMo, fixed))
  
  # Build attribute list
  dow <- wday(dt, week_start = 1)
  attrs <- list(
    year           = factor(year(dt)),
    month          = factor(month(dt), levels = 1:12, labels = month.name),
    day_of_year    = factor(yday(dt)),
    day_of_month   = factor(mday(dt)),
    day_of_week    = factor(dow, levels = 1:7,
                            labels = c("Monday","Tuesday","Wednesday",
                                       "Thursday","Friday","Saturday","Sunday")),
    is_leap        = factor(as.integer(leap_year(dt)), levels = 0:1),
    is_dst         = factor(as.integer(dst(dt)), levels = 0:1),
    dst_change     = factor(as.integer(c(0, diff(as.integer(dst(dt))) != 0)), levels = 0:1),
    is_easter      = factor(as.integer(dates %in% easters), levels = 0:1),
    is_good_friday = factor(as.integer(dates %in% goodFri), levels = 0:1),
    is_easter_mo   = factor(as.integer(dates %in% easterMo), levels = 0:1),
    is_holiday     = factor(as.integer(dates %in% all_hols), levels = 0:1),
    laborable      = factor(as.integer(!(dates %in% all_hols) & !dow %in% c(6,7)), levels = 0:1)
  )
  
  # Filter only requested components
  components <- intersect(components, names(attrs))
  
  # Build output
  if (is.null(input_df)) {
    df <- data.frame(datetime = dt, stringsAsFactors = FALSE)
    df[components] <- attrs[components]
    return(df)
  } else {
    # Append to original data.frame
    for (cmp in components) {
      input_df[[cmp]] <- attrs[[cmp]]
    }
    return(input_df)
  }
}
