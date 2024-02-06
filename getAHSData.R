library(stringr)
library(readr)
library(dplyr)
library(magrittr)

file_urls <- c(
  "https://www2.census.gov/programs-surveys/ahs/2021/AHS%202021%20National%20PUF%20v1.0%20Flat%20CSV.zip",
  "https://www2.census.gov/programs-surveys/ahs/2019/AHS%202019%20National%20PUF%20v1.1%20Flat%20CSV.zip",
  "https://www2.census.gov/programs-surveys/ahs/2017/AHS%202017%20National%20PUF%20v3.1%20Flat%20CSV.zip",
  "https://www2.census.gov/programs-surveys/ahs/2015/AHS%202015%20National%20PUF%20v3.1%20Flat%20CSV.zip",
  "https://www2.census.gov/programs-surveys/ahs/2013/AHS%202013%20National%20PUF%20v2.0%20Flat%20CSV.zip",
  "https://www2.census.gov/programs-surveys/ahs/2011/AHS%202011%20National%20PUF%20v3.0%20Flat%20CSV.zip",
  "https://www2.census.gov/programs-surveys/ahs/2009/AHS%202009%20National%20PUF%20v2.0%20Flat%20CSV.zip",
  "https://www2.census.gov/programs-surveys/ahs/2007/AHS%202007%20National%20PUF%20v2.0%20Flat%20CSV.zip",
  "https://www2.census.gov/programs-surveys/ahs/2005/AHS%202005%20National%20PUF%20v2.0%20Flat%20CSV.zip",
  "https://www2.census.gov/programs-surveys/ahs/2003/AHS%202003%20National%20PUF%20v2.0%20Flat%20CSV.zip",
  "https://www2.census.gov/programs-surveys/ahs/2001/AHS%202001%20National%20PUF%20v2.0%20Flat%20CSV.zip",
  "https://www2.census.gov/programs-surveys/ahs/1999/AHS%201999%20National%20PUF%20v2.0%20Flat%20CSV.zip",
  "https://www2.census.gov/programs-surveys/ahs/1997/AHS%201997%20National%20PUF%20v2.0%20Flat%20CSV.zip",
  "https://www2.census.gov/programs-surveys/ahs/1995/AHS%201995%20National%20PUF%20v2.1%20CSV.zip",
  "https://www2.census.gov/programs-surveys/ahs/1993/AHS%201993%20National%20PUF%20v2.0%20CSV.zip",
  "https://www2.census.gov/programs-surveys/ahs/1991/AHS%201991%20National%20PUF%20v2.0%20CSV.zip",
  "https://www2.census.gov/programs-surveys/ahs/1989/AHS%201989%20National%20PUF%20v2.0%20CSV.zip",
  "https://www2.census.gov/programs-surveys/ahs/1987/AHS%201987%20National%20PUF%20v2.0%20CSV.zip",
  "https://www2.census.gov/programs-surveys/ahs/1985/AHS%201985%20National%20PUF%20v2.0%20CSV.zip"
)

columns_of_interest <- c("CONTROL", "STATUS", "INTSTATUS", "WEIGHT", "WGT90GEO", "HINCP", "ZINC2", "TENURE",
                         "UNITSF", "UNITSIZE", "AMMORT", "MORTAMT", "MORTSTAT")

variable_crosswalk <- read_csv('./data/ahs_2015_variable_crosswalk.csv')

#' Get the survey year from the URL
#' @param url The survey file URL
#' @return The survey year as a string
get_survey_year <- function(url) {
  str_extract(url, "(/ahs/)([0-9]{4})", group = 2)
}

#'Downloads data from source to temp folder
#'@param urls Character vector of urls to download from
#'@param folder Directory (relative to current working directory) to download files to. Will create
#'directory if doesn't exist.
download_data_to_tmp <- function(urls, folder = ".tmp") {
  # Set timeout to 60 min
  original_timeout <- getOption("timeout")
  options(timeout = max(60*60, original_timeout))
  # Create temp directory, if it doesn't exist
  if (!file.exists(folder)) {
    dir.create(folder)
  }
  # Exclude files that are currently in the directory
  file_names <- paste(getwd(), folder, paste("ahs_", get_survey_year(urls), str_sub(urls, -4), sep = ""), sep = "/")
  current_files <- paste(getwd(), folder, list.files(paste(getwd(), folder, sep="/"), all.files = TRUE, no..=TRUE), sep = "/")
  urls <- urls[!(file_names %in% current_files)]
  file_names <- setdiff(file_names, current_files)
  
  # Download files and use libcurl to support simultaneous downloads
  stopifnot(length(urls) == length(file_names))
  
  if (length(urls) > 0) {
    download.file(urls, file_names, mode = "wb", method = "libcurl")
  } else {
    warning("no files downloaded.")
  }
  # Reset timeout
  options(timeout = original_timeout)
}

#' Unzip files listed in folder
#' @param folder Directory (relative to current working directory) in which to find .zip files
#' @return A character vector of the created csv files
unzip_data_files <- function(folder =".tmp") {
 zip_files <- paste(getwd(), folder, list.files(paste(getwd(), folder, sep = "/"), pattern = ".zip$", all.files = TRUE, no..=TRUE), sep = "/")
 expected_files <- paste(getwd(), folder, paste("ahs", str_extract(zip_files, "(ahs_)([0-9]{4})", group = 2), "n.csv", sep = ""), sep = "/")
 current_files <- paste(getwd(), folder, list.files(paste(getwd(), folder, sep = "/"), pattern = ".csv$", all.files = TRUE, no..=TRUE), sep = "/")
 
 unlist(lapply(zip_files[!(expected_files %in% current_files)], unzip, exdir = paste(getwd(), folder, sep = "/")))
}

#' Remaps variables from < 2015 to their equivalent 2015+ name
#' if variable does not exist, will keep previous variable name
#' @param df A data.frame like object to rename columns from
rename_ahs_cols <- function(df) {
  matching_idx <- match(names(df), variable_crosswalk$`2011/2013 Variable Name`)
  is_match <- names(df) %in% variable_crosswalk$`2011/2013 Variable Name`
  names(df)[is_match] <- variable_crosswalk$`2015 Variable Name`[matching_idx[is_match]]
  df
}

convert_unitsize <- function(df, year) {
  if (year < 2015 && 'UNITSF' %in% names(df)) {
    df$UNITSF <- as.numeric(df$UNITSF)
    df$UNITSF <- case_when(
     df$UNITSF < 500 ~ "'1'",
     df$UNITSF < 750 ~ "'2'",
     df$UNITSF < 1000 ~ "'3'",
     df$UNITSF < 1500 ~ "'4'",
     df$UNITSF < 2000 ~ "'5'",
     df$UNITSF < 2500 ~ "'6'",
     df$UNITSF < 3000 ~ "'7'",
     df$UNITSF < 4000 ~ "'8'",
     df$UNITSF >= 4000 ~ "'9'"
    )
  }
  df
}

# For each data file
# select columns of interest
# filter by ISTATUS == '1'
# rename columns to post 2015 names
# remove missing/invalid values
preprocess_ahs_data <- function(folder = ".tmp") {
  files <- list.files(paste(getwd(), folder, sep = "/"), pattern = "ahs[0-9]{4}n.csv$", all.files = TRUE, no.. = TRUE)
  processed_data <- lapply(files, function(file) {
    year <- as.numeric(str_extract(file, "ahs([0-9]{4})n.csv$", group = 1))
    # Read data and select colummns of interest
    dat <- read_csv(paste(getwd(), folder, file, sep = "/"), col_select = any_of(columns_of_interest), na = c(".", ""))
    # Rename columns to post 2015 names
    dat <- dat %>% 
      # Convert pre-2015 unitsf to categories matching unitsize var 2015+
      convert_unitsize(year = year) %>%
      # Rename pre-2015 cols
      rename_ahs_cols() %>%
      # Rename 90s census weightings
      rename(any_of(c(WEIGHT = "WGT90GEO"))) %>%
      # Filter by completed interview status
      filter(INTSTATUS == "'1'" & TENURE == "'1'" & UNITSIZE != "'-9") %>%
      # Remove instatus
      select(-c(INTSTATUS, TENURE)) %>%
      # Convert double columns to their appropriate types
      mutate(WEIGHT = as.double(WEIGHT), HINCP = as.double(HINCP), MORTAMT = as.double(MORTAMT)) %>% 
      filter(HINCP != -6 & HINCP != -9 & MORTAMT >= 0) %>%
      # Remove rows with missing data
      na.omit() %>%
      mutate(YEAR = rep(year, nrow(.)))
  })
  do.call(bind_rows, processed_data)
}
