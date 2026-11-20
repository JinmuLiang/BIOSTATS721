# Build reproducible summaries and save processed data from all three years.
# This is the main orchestrator function. It sources the cleaning helper,
# builds the pooled daily dataset across 2018-2020, then derives five
# summary outputs (daily data, annual ranges, pooled monthly stats,
# PM2.5 monthly-by-year stats, and a cleaning audit). Each output is
# written to data/processed/*.csv and the in-memory list is returned.
build_analysis <- function() {
  # Bring the daily-cleaning function into scope.
  source("R/process_air_quality.R")
  # Make sure the output directories exist (no error if already present).
  dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
  dir.create("figures", showWarnings = FALSE)
  # Years covered by the analysis; used to build raw file paths.
  years <- 2018:2020
  # One raw CSV per year, following the naming convention AQ_<year>.csv.
  paths <- sprintf("data/raw/AQ_%d.csv", years)
  # Clean each year's file and row-bind them into a single daily dataframe.
  daily <- do.call(rbind, lapply(paths, process_air_quality))
  # Extract calendar year and month from the date column for grouping.
  daily$year <- as.integer(format(daily$date, "%Y"))
  daily$month <- as.integer(format(daily$date, "%m"))
  # Pollutants of interest, matching the column names in the cleaned data.
  pollutants <- c("co", "pm25", "o3")
  # Per-year, per-pollutant summary: non-missing count, min, and max.
  # Built with nested lapply so every (year, pollutant) pair gets one row.
  ranges <- do.call(rbind, lapply(years, function(yr) {
    do.call(rbind, lapply(pollutants, function(p) {
      v <- daily[daily$year == yr, p]
      data.frame(year = yr, pollutant = p, n = sum(!is.na(v)),
                 minimum = min(v, na.rm = TRUE), maximum = max(v, na.rm = TRUE))
    }))
  }))
  # Pool daily observations by calendar month, not averages of annual means.
  # For each pollutant × month, compute mean, SD, and a 95% CI half-width
  # (1.96 * SD / sqrt(n)) so we can plot confidence intervals later.
  monthly <- do.call(rbind, lapply(pollutants, function(p) {
    do.call(rbind, lapply(1:12, function(m) {
      v <- daily[daily$month == m, p]
      n <- sum(!is.na(v)); avg <- mean(v, na.rm = TRUE); s <- sd(v, na.rm = TRUE)
      data.frame(pollutant = p, month = m, n = n, mean = avg, sd = s,
                 lower = avg - 1.96 * s / sqrt(n), upper = avg + 1.96 * s / sqrt(n))
    }))
  }))
  # PM2.5-only monthly means, kept stratified by year, for the by-year trend plot.
  # Only months actually present in that year are included (sort(unique(...))).
  pm_monthly <- do.call(rbind, lapply(years, function(yr) {
    months <- sort(unique(daily$month[daily$year == yr]))
    do.call(rbind, lapply(months, function(m) {
      v <- daily$pm25[daily$year == yr & daily$month == m]
      data.frame(year = yr, month = m, n = sum(!is.na(v)), mean = mean(v, na.rm = TRUE))
    }))
  }))
  # Data-quality audit: for each year, compare raw vs. cleaned data.
  # - raw_rows: original row count
  # - duplicates_removed: number of duplicated raw rows
  # - recorded_dates: unique dates present in the raw file
  # - calendar_days: rows in the cleaned, gap-filled daily series
  # - absent_dates: missing calendar days that had to be added
  # - negative_measurements: negative values in raw pollutant columns (2,4,6)
  # - co_days / pm25_days / o3_days: non-missing day counts per pollutant
  audit <- do.call(rbind, lapply(seq_along(paths), function(i) {
    raw <- read.csv(paths[i], check.names = FALSE)
    clean <- process_air_quality(paths[i])
    data.frame(year = years[i], raw_rows = nrow(raw), duplicates_removed = sum(duplicated(raw)),
               recorded_dates = length(unique(raw$Date)), calendar_days = nrow(clean),
               absent_dates = nrow(clean) - length(unique(raw$Date)),
               negative_measurements = sum(as.matrix(unique(raw)[, c(2,4,6)]) < 0, na.rm = TRUE),
               co_days = sum(!is.na(clean$co)), pm25_days = sum(!is.na(clean$pm25)),
               o3_days = sum(!is.na(clean$o3)))
  }))
  # Bundle all outputs in a named list; names drive the CSV filenames.
  outputs <- list(daily_air_quality = daily, annual_ranges = ranges,
                  monthly_pooled = monthly, pm25_monthly_by_year = pm_monthly,
                  cleaning_audit = audit)
  # Write one CSV per output. Empty strings (na = "") mark missing values.
  for (nm in names(outputs)) write.csv(outputs[[nm]], paste0("data/processed/", nm, ".csv"),
                                       row.names = FALSE, na = "")
  # Return the key objects so downstream code can reuse them without re-reading CSVs.
  list(daily = daily, ranges = ranges, monthly = monthly, pm_monthly = pm_monthly, audit = audit)
}

# Two panels preserve ppm units while allowing readable independent y ranges.
# CO and ozone share the ppm unit, but their typical magnitudes differ, so a
# stacked two-row layout with free y-axes keeps both readable.
plot_co_ozone <- function(monthly) {
  # Save and restore graphical parameters so this function leaves no side effects.
  old <- par(no.readonly = TRUE); on.exit(par(old))
  # 2 rows x 1 column; margins tuned for labels; las = 1 for horizontal y labels.
  par(mfrow = c(2, 1), mar = c(3.2, 5.6, 2.2, 1), mgp = c(4.2, 1, 0), oma = c(1.2, 0, 2, 0),
      family = "sans", las = 1, col.axis = "#374151", col.lab = "#374151")
  # Fixed pollutant order and color-blind-friendly palette (Okabe-Ito).
  pollutants <- c("co", "o3"); colors <- c("#0072B2", "#D55E00")
  for (i in seq_along(pollutants)) {
    # Subset to the current pollutant's monthly summary rows.
    d <- monthly[monthly$pollutant == pollutants[i], ]
    # Use ppm for both panels since both CO and ozone are in ppm here.
    label <- if (i == 1) "CO (ppm)" else "Ozone (ppm)"
    # Empty plot to set up axes; x limited to month range with a little padding.
    plot(d$month, d$mean, type = "n", xlim = c(.6, 12.4),
         ylim = extendrange(c(d$lower, d$upper), f = .12), xaxt = "n",
         xlab = "", ylab = label, main = label, bty = "n")
    # Light horizontal gridlines at default tick positions.
    abline(h = axTicks(2), col = "#E5E7EB", lwd = .8)
    # Month labels on x axis; tick = FALSE keeps the panel visually clean.
    axis(1, at = 1:12, labels = month.abb, tick = FALSE, cex.axis = .9)
    # Vertical error bars for the 95% CI, with arrowheads on both ends.
    arrows(d$month, d$lower, d$month, d$upper, angle = 90, code = 3,
           length = .045, col = colors[i], lwd = 1.4)
    # Overlay the monthly mean points on top of the error bars.
    points(d$month, d$mean, pch = 19, cex = 1.1, col = colors[i])
    # Small legend naming the pollutant for this panel.
    legend("topright", legend = if (i == 1) "CO" else "Ozone", col = colors[i],
           pch = 19, bty = "n", cex = .85)
  }
  # Shared x-axis title and figure title placed in the outer margins.
  mtext("Month", side = 1, outer = TRUE, line = .2)
  mtext("Monthly CO and ozone: pooled daily means and 95% CIs", side = 3,
        outer = TRUE, font = 2, line = .4, cex = 1.05)
}

# Monthly mean points and lines emphasize temporal patterns without daily clutter.
# One line per year for PM2.5, so year-over-year seasonal shapes are comparable.
plot_pm25 <- function(pm_monthly) {
  # Save/restore graphical parameters to avoid leaking state.
  old <- par(no.readonly = TRUE); on.exit(par(old))
  # Single panel with margins sized for month labels and legend.
  par(mar = c(4, 4.4, 3.2, 1), family = "sans", las = 1,
      col.axis = "#374151", col.lab = "#374151")
  # Empty plot with padding on both axes; y range spans all years' monthly means.
  plot(NA, xlim = c(.7, 12.3), ylim = extendrange(pm_monthly$mean, f = .18),
       xlab = "Month", ylab = "PM2.5 (ug/m3 LC)", xaxt = "n", bty = "n",
       main = "Monthly PM2.5 patterns by year")
  # Light horizontal gridlines for readability.
  abline(h = axTicks(2), col = "#E5E7EB", lwd = .8)
  # Month labels on x axis; no visible tick marks.
  axis(1, at = 1:12, labels = month.abb, tick = FALSE, cex.axis = .9)
  # Year-specific color and point shape for three series (Okabe-Ito palette).
  years <- 2018:2020; colors <- c("#0072B2", "#D55E00", "#009E73")
  shapes <- c(16, 17, 15)
  for (i in seq_along(years)) {
    # Subset to one year, then draw connected points for that year's monthly means.
    d <- pm_monthly[pm_monthly$year == years[i], ]
    lines(d$month, d$mean, type = "b", col = colors[i], pch = shapes[i], lwd = 1.6, cex = 1)
  }
  # Horizontal legend at top-right summarizing the year series.
  legend("topright", legend = years, col = colors, pch = shapes, lty = 1,
         bty = "n", horiz = TRUE, cex = .9, title = "Year")
}
