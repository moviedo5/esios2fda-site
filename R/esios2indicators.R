#' @encoding UTF-8
#' @title Download ESIOS indicators metadata and augment with units
#'
#' @description
#' \code{esios2indicators} fetches the list of available indicators
#' from Spain's ESIOS API in both Spanish and English, cleans their
#' short names (replacing spaces or hyphens with "_"), appends the ID
#' to ensure uniqueness, assigns units manually, and saves
#' the result to CSV and RDA files.
#'
#' @param api_key String: your ESIOS API key.
#' @param output_dir Directory where files will be saved (default ".").
#' @param encoding Encoding used to read the JSON (default "UTF-8").
#'
#' @return Invisible data.frame with columns \code{id}, \code{short_name_es},
#'   \code{name_es}, \code{short_name_en}, \code{name_en}, and \code{units}.
#' @export
esios2indicators <- function(api_key, output_dir = ".", encoding = "UTF-8") {
  
  # Helper to retrieve indicators for a given locale
  get_ind <- function(locale) {
    res <- httr::GET(
      "https://api.esios.ree.es/indicators",
      query = list(locale = locale),
      httr::add_headers(
        Accept    = "application/json; application/vnd.esios-api-v1+json",
        `x-api-key` = api_key
      )
    )
    if (httr::status_code(res) != 200) {
      stop(sprintf(
        "HTTP error %d while fetching indicators for locale '%s'", 
        httr::status_code(res), locale
      ))
    }
    dat <- jsonlite::fromJSON(
      httr::content(res, as = "text", encoding = encoding),
      simplifyDataFrame = TRUE
    )
    dat$indicators[, c("id", "short_name", "name")]
  }
  
  # Download Spanish and English indicators
  inds_es <- get_ind("es")
  inds_en <- get_ind("en")
  
  # Build data.frame with cleaned short names
  df <- data.frame(
    id            = inds_es$id,
    short_name_es = gsub("[ \\-]", "_", inds_es$short_name),
    name_es       = inds_es$name,
    short_name_en = paste0(gsub("[ \\-]", "_", inds_en$short_name), "_", inds_en$id),
    name_en       = inds_en$name,
    stringsAsFactors = FALSE
  )
  
  # Assign units manually
  df$units <- ""
  df$units[df$id %in% 541:552] <- "MWh"
  df$units[df$id %in% c(573,574,578,579,600,602,603,805)] <- "EUR/MWh"
  df$units[df$id %in% 1293:1297] <- "MWh"
  
  # Save to CSV and RDA
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  write.csv2(
    df, file.path(output_dir, "indicators_with_units.csv"),
    row.names = FALSE
  )
  save(df, file = file.path(output_dir, "indicators_with_units.rda"))
  
  invisible(df)
}
