# Clean one year of raw EPA measurements to one row per calendar day.
# path: path to one AQ_YYYY.csv file with the supplied column schema.
# o3_units: ozone output units, "ppm" (default) or "ppb"; input ozone is ppm.
# Returns: a data frame with Date-valued date, co (ppm), pm25 (ug/m3 LC),
# o3 (chosen units), and unit columns; absent/all-missing days remain NA.
process_air_quality <- function(path, o3_units = c("ppm", "ppb")) {
  o3_units <- match.arg(o3_units)
  raw <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  schema <- c("Date", "Daily Max 8-hour CO Concentration", "units_CO",
              "Daily Mean PM2.5 Concentration", "units_PM",
              "Daily Max 8-hour Ozone Concentration", "units_O3")
  if (!all(schema %in% names(raw))) stop("Required columns are missing")
  raw <- unique(raw[schema]) # Deduplicate exact records before altering values.
  names(raw) <- c("date", "co", "units_co", "pm25", "units_pm25", "o3", "units_o3")
  raw$date <- as.Date(raw$date, "%m/%d/%Y")
  if (!nrow(raw) || anyNA(raw$date)) stop("No rows or invalid date")
  if (length(unique(format(raw$date, "%Y"))) != 1L) stop("Expected a single year")
  expected_units <- c(units_co = "ppm", units_pm25 = "ug/m3 LC", units_o3 = "ppm")
  for (u in names(expected_units)) {
    if (any(!is.na(raw[[u]]) & raw[[u]] != expected_units[[u]])) stop("Unexpected input units")
  }
  # Correct individual negative readings before calculating daily averages.
  pollutants <- c("co", "pm25", "o3")
  for (p in pollutants) {
    if (!is.numeric(raw[[p]])) stop("Concentrations must be numeric")
    raw[[p]][!is.na(raw[[p]]) & raw[[p]] < 0] <- 0
  }
  dates <- seq(min(raw$date), max(raw$date), by = "day")
  daily <- data.frame(date = dates)
  for (p in pollutants) {
    daily[[p]] <- vapply(dates, function(day) {
      values <- raw[[p]][raw$date == day]
      if (!length(values) || all(is.na(values))) NA_real_ else mean(values, na.rm = TRUE)
    }, numeric(1))
  }
  if (o3_units == "ppb") daily$o3 <- daily$o3 * 1000
  daily$units_co <- "ppm"
  daily$units_pm25 <- "ug/m3 LC"
  daily$units_o3 <- o3_units
  daily
}
