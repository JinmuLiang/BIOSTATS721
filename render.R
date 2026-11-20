packages <- c("rmarkdown", "knitr")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
rmarkdown::render("report.Rmd", output_format = "word_document", quiet = TRUE)
writeLines(capture.output(sessionInfo()), "session-info.txt")
