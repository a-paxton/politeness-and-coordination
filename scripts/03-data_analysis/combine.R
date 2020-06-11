dir_output <- Sys.getenv("SCRATCH_DIR", unset = NA)
dir_output <- ifelse(is.na(dir_output), ".", dir_output)
files_output <- Sys.glob(file.path(dir_output, "*.RData"))

results <- data.frame()
for (file in files_output) {
    load(file)
    results <- rbind(results, result)
}

# FIXME: Instead of print() use readr::write_csv to save this
# somewhere sensible!  You probably don't want to save the combined
# file in SCRATCH_DIR though.

print(results)
