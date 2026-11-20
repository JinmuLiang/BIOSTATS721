source("R/process_air_quality.R")
# Duplicated positive readings and one negative distinguish the processing order.
x <- data.frame(Date = c("01/01/2018", "01/01/2018", "01/01/2018", "01/03/2018"),
                co = c(-2, 4, 4, NA), units_co = "ppm", pm25 = c(-2, 4, 4, NA),
                units_pm = "ug/m3 LC", o3 = c(.02, .04, .04, NA), units_o3 = "ppm")
names(x) <- c("Date", "Daily Max 8-hour CO Concentration", "units_CO",
              "Daily Mean PM2.5 Concentration", "units_PM",
              "Daily Max 8-hour Ozone Concentration", "units_O3")
f <- tempfile(fileext = ".csv"); write.csv(x, f, row.names = FALSE)
z <- process_air_quality(f); zz <- process_air_quality(f, "ppb")
stopifnot(nrow(z) == 3L, z$co[1] == 2, z$pm25[1] == 2,
          is.na(z$co[2]), is.na(z$co[3]), abs(z$o3[1] - .03) < 1e-12,
          abs(zz$o3[1] - 30) < 1e-10, all(zz$units_o3 == "ppb"),
          inherits(try(process_air_quality(f, "invalid"), silent = TRUE), "try-error"))
unlink(f)
for (yr in 2018:2020) {
  path <- sprintf("data/raw/AQ_%d.csv", yr)
  a <- process_air_quality(path); b <- process_air_quality(path, "ppb")
  stopifnot(!anyDuplicated(a$date), inherits(a$date, "Date"),
            nrow(a) == if (yr == 2020) 121L else 365L,
            all(as.matrix(a[c("co", "pm25", "o3")]) >= 0, na.rm = TRUE),
            isTRUE(all.equal(a$o3 * 1000, b$o3)))
}
cat("PASS: correction order, duplicates, dates, missing values, units, and coverage\n")
