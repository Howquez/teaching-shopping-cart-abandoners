# This script runs the whole project from raw data to the PDF report.
# We recommend to first open the .Rproj file, then open and run this file from there.
# Note that each step runs in a fresh R session. Each step writes a log file to
# outputs/logs/ that shows every line of code next to its output, including
# warnings and errors. These logs document what happened on the author's machine.

# Packages -----
# note that I do not use a package manager like groundhog for this package as I
# expect that it is stable enough
# the {here}-package builds file paths from the project root, wherever you run the code
library(package = "here")


# Prep -----
dir.create(path = here("outputs", "logs"), recursive = TRUE, showWarnings = FALSE)
r_program <- file.path(R.home(component = "bin"), "R")   # the R program on this machine

# R CMD BATCH cannot handle folder names with spaces, so the paths are relative
# to the project root (which is why you should open the .Rproj file).
run_script <- function(script) {
  log_file <- paste0("outputs/logs/", tools::file_path_sans_ext(basename(script)), ".Rout")
  system2(command = r_program,
          args = c("CMD", "BATCH", "--no-save", "--no-restore", script, log_file))
}


# Document the raw data -----
# md5sum() computes a hash/fingerprint (checksum) for each file. If anyone else gets
# the same fingerprints, they have exactly the same data, byte for byte.
raw_files <- list.files(path = here("data", "raw"), full.names = TRUE)
writeLines(text = paste(tools::md5sum(files = raw_files), basename(raw_files)),
           con = here("outputs", "logs", "data_checksums.txt"))


# Run full pipeline -----

run_script(script = "scripts/01_read_data.R")
run_script(script = "scripts/02_analyze.R")
run_script(script = "scripts/03_power_simulation.R")
run_script(script = "scripts/04_figures_tables.R")


# Render the report -----
system2(command = "quarto",
        args = c("render", shQuote(string = here("report", "report.qmd"))),
        stdout = here("outputs", "logs", "report.log"),
        stderr = here("outputs", "logs", "report.log"))


# Document the Quarto version -----
# Package versions are documented at the end of each script's log (see sessionInfo()).
writeLines(text = system2(command = "quarto", args = "--version", stdout = TRUE),
           con = here("outputs", "logs", "quarto_version.txt"))