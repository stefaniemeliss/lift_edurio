# Clear the workspace and run garbage collection
rm(list = ls())
gc()

# source functions
source_code(target_repo = "lift_edurio", file_name = "functions.R")

# Define the base directory
dir <- get_directory()
dir_data <- file.path(dir, "data")
dir_misc <- file.path(dir, "misc")

# get file stem name
file_stem <- get_file_stem()

# copy files #

# dir_source <- gsub("lift_edurio", "edu_stats", dir)
# file.copy(from = list.files(file.path(dir_source, "data"), pattern = "data_", full.name = T),
#           to = dir_data)
