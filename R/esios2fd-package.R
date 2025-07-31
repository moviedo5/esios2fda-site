#' @encoding UTF-8
#' @title esios2fd: Download and Convert ESIOS Indicators to Functional Data
#'
#' @description
#' The esios2fd package provides tools to fetch time-series data from Spain's
#' ESIOS API and transform raw series into standardized formats and functional
#' data objects. With esios2fd you can:
#' \itemize{
#'   \item Retrieve and save indicator metadata (IDs, names, units) via esios2indicators().
#'   \item Download raw time-series CSV files with uniform timestamps via esios2csv().
#'   \item Convert CSV data into functional data (`fdata`) objects, handling DST shifts, via esios2fdata().
#'   \item Assemble daily curves into an `ldata` object for multivariate analysis via esios2lfdata().
#'   \item Directly fetch time-series into a data.frame with esios2df().
#' }
#'
#' @details
#' Functions in this package follow ESIOS API usage guidelines: you must supply
#' a personal API token and respect rate limits by caching responses when
#' exposing public applications. See https://api.esios.ree.es for more info.
#'
#' @author
#' Manuel Oviedo de la Fuente [\email{manuel.oviedo@udc.es}]
#'
#' @name esios2fd
NULL