#### SETUPS ####

# Clear the workspace and run garbage collection
rm(list = ls())
gc()
set.seed(123)

# load libraries
library(dplyr)

# create function to source code
source_code <- function(root_dir_name = "code", target_repo = "helper_functions", branch = "main", file_name = "file.R") {
  
  # construct URL
  git_url <- paste0("https://raw.githubusercontent.com/stefaniemeliss/", target_repo, "/", branch, "/", file_name)
  
  # attempt to download from github
  tempp_file <- tempfile(fileext = ".R")
  message <- curl::curl_download(git_url, tempp_file, quiet = F)
  
  if(!grepl("Error", message)) {
    
    # if successful, source file
    source(tempp_file)
    remove(tempp_file)
    
  } else { # load local copy of file
    
    # Get the current working directory
    current_dir <- getwd()
    
    # Split the current directory into its components
    dir_components <- strsplit(current_dir, "/")[[1]]
    
    # Identify the root directory dynamically based on the provided root directory name
    root_index <- which(dir_components == root_dir_name)
    if (length(root_index) == 0) {
      stop(paste("Root directory", root_dir_name, "not found in the current path"))
    }
    root_dir <- do.call(file.path, as.list(dir_components[1:root_index]))
    
    # Identify the subdirectory one level below the root and construct its absolute path
    project_repo <- dir_components[root_index + 1]
    dir <- file.path(root_dir, project_repo)
    
    if (target_repo != project_repo) {
      dir <- gsub(project_repo, target_repo, dir) 
    }
    
    # Construct the full file path
    file_path <- file.path(dir, file_name)
    
    # Print the directory and file path for debugging
    print(paste("Directory:", dir))
    print(paste("File path:", file_path))
    
    # Source the file into the parent frame
    source(file_path, local = parent.frame())
  }
}

# source functions
source_code(target_repo = "lift_edurio", file_name = "functions.R")

# Define the base directory
dir <- get_directory()
dir_data <- file.path(dir, "data")
dir_misc <- file.path(dir, "misc")
dir_in <- gsub("code/lift_edurio", "research_projects/2025_Q2_Lift_edurio/data", dir)

# check if directories exist 
if (! dir.exists(dir_data)) dir.create(dir_data)
if (! dir.exists(dir_misc)) dir.create(dir_misc)

# get file stem name and file directory
get_file_info()

# copy DfE data #
files <- list.files(path = file.path(gsub("lift_edurio", "DfE_Data", dir_data)), pattern = ".csv")
for (file in files) {
  file.copy(
    file.path(gsub("lift_edurio", "DfE_Data", dir_data), file), dir_data, overwrite = T)
}

# get data on LIFT schools #

# read in GIAS estab data
lift <- read.csv(file.path(dir_data, "data_gias_estab.csv"), na.strings = "")

# select cols
cols <- c("urn", "laestab", "establishmentname", "opendate",
          "typeofestablishment",
          "phaseofeducation", "statutorylowage", "statutoryhighage", "nurseryprovision", "officialsixthform",
          "gender", "senpru", "specialclasses", "boarders", "religiouscharacter", "admissionspolicy",
          "urbanrural", "la", "gor")

# select rows
# lift <- lift[!is.na(lift$group_uid) & lift$group_uid == 2053, ]
lift <- lift[!is.na(lift$group_uid) & lift$group_uid == 2053, cols]

# get Edurio data #

# get pw
pw <- read.delim(file = file.path(dir_in, "pw.txt"), header = F)[1,1]

# get name of zipped folder contained the edurio data
zipped <- list.files(dir_in, pattern = ".zip", full.names = T)

# unzip file in data directory
unzip(zipped, exdir = dir_data) 

# get name of folder
folder <- list.dirs(dir_data, full.names = F, recursive = F)

# overwrite dir_in
dir_in <- file.path(dir_data, folder)

# get survey periods
periods <- xlsx::read.xlsx(file = file.path(dir_in, "SurveyPeriodList.xlsx"), sheetIndex = 1, startRow = 3, endRow = 9, colIndex = 2:7)

# change col names in periods
names(periods)  <- tolower(gsub(".", "_", names(periods), fixed = T))

# add time_period
periods$time_period <- c(202324, 202223, 202122, 202021, 201920, 201819)
periods$academic_year <- insert_slash(periods$time_period)

# get years
years <- periods$year

# get file names
files <- file.path(dir_in, periods$file_name)