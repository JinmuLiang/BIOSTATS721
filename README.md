# Air quality at RDU, 2018–April 2020

Author: Jinmu Liang  
Date: November 20, 2026  
GitHub: <https://github.com/JinmuLiang/BIOSTATS721>

This project cleans EPA air quality measurements and describes daily ranges and
monthly patterns in CO, PM2.5, and ozone. These exposure data motivate research
on sickle cell disease in Durham County; no patient outcomes were supplied.

## Data source

The three supplied CSV files contain EPA monitor measurements at RDU airport,
North Carolina, for 2018, 2019, and January–April 2020. Original files are kept
unchanged. No monitor ID or original EPA download URL was supplied.

## Files

- `Jinmu_Liang_Project2.Rproj`: RStudio project.
- `data/raw/`: original CSVs.
- `R/process_air_quality.R`: documented, reusable cleaning function.
- `R/analysis.R`: summaries and plot functions.
- `R/validate.R`: meaningful cleaning-order and coverage checks.
- `report.Rmd`: report source; code is hidden in the output.
- `report.docx`: rendered Word report.
- `data/processed/`: daily values, annual ranges, monthly summaries, and audit.
- `figures/`: two PNG figures.
- `render.R`: render entry point.
- `session-info.txt`: rendering environment and package versions.

## Reproduce

Install R (4.1 or newer), Pandoc, and the R packages `rmarkdown` and `knitr`.
RStudio includes Pandoc. Open this project's `.Rproj` and run `source("render.R")`,
or run `Rscript render.R` from this folder. Missing R packages are installed
from CRAN. Run `Rscript R/validate.R` for validation. All paths are relative to
this project folder. Other computation and plots use base R only.

## Decisions

Exact duplicate records are removed before negative measurements are set to
zero. Available readings are then averaged within each day. Equal pollutant
values on otherwise different records remain measurements because observation
identifiers are unavailable. All-missing pollutant-days remain NA; four absent
September 2018 dates are explicitly represented as missing. No dates beyond
April 30, 2020 are added. Ozone defaults to ppm and can be requested in ppb.

CO and ozone intervals pool all observed daily values by calendar month across
available years and use mean ± 1.96 × SD / sqrt(n). January–April include three
years; later months include two. These prescribed intervals do not adjust for
serial correlation. Separate CO/ozone panels preserve units and show patterns
on independent y scales. PM2.5 is shown as monthly means by year.
