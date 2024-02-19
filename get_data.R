library(stringr)
library(readr)
library(dplyr)
library(magrittr)
library(arrow)

# Retrieves data from census band saves in parquet format

urls <- c(
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
    "https://www2.census.gov/programs-surveys/ahs/1991/AHS%201991%20National%20PUF%20v2.0%20CSV.zip",
    "https://www2.census.gov/programs-surveys/ahs/1989/AHS%201989%20National%20PUF%20v2.0%20CSV.zip",
    "https://www2.census.gov/programs-surveys/ahs/1985/AHS%201985%20National%20PUF%20v2.0%20CSV.zip"
)

# Uses this file to hold the set of already dowloaded files
DOWNLOADED_FILES_PATH <- "downloaded_files.txt"

#' Loads AHS national data from census website to data folder
#' in partitioned parquet format. Renames and recodes columns to match 2021 format
#' @param url The url location of the National Flat file for a particular year.
#' must match, ^https://www2.census.gov/programs-surveys/ahs/[0-9]{4}.*Flat%20CSV.zip$
#' @param data_path Relative path of folder in which to place the output files
load_ahs_data <- function(url, data_path = "data") {
    if (!grepl("^https://www2.census.gov/programs-surveys/ahs/[0-9]{4}.*CSV.zip$", url)) {
        stop(paste("url does not match", "^https://www2.census.gov/programs-surveys/ahs/[0-9]{4}.*CSV.zip$"))
    }

    # Skip if this URL has already been downloaded
    downloaded_files <- c()
    if (file.exists(DOWNLOADED_FILES_PATH)) {
        downloaded_files <- scan(DOWNLOADED_FILES_PATH, what = character(), sep = "\n", quiet = TRUE)
    }

    if (url %in% downloaded_files) {
        message(paste("URL", url, "has already been downloaded. Skipping..."))
        return()
    }


    survey_year <- as.numeric(stringr::str_extract(url, "^https://www2.census.gov/programs-surveys/ahs/([0-9]{4}).*CSV.zip$", group = 1))
    temp <- tempfile()
    download.file(url, temp)
    # CONTROL - Unique identifier (character)
    # WEIGHT/WGT90GEO - Weighting factor (numeric)
    # INTSTATUS/ISTATUS - Interview status (character)
    # TENURE - Ownership status (character)
    # MARKETVAL/VALUE - Market value of house (numeric)
    # HINCP/ZINC2 - Household income (numeric)
    # TOTHCAMT/ZSMHC - Total monthly housing costs (numeric)
    if (survey_year >= 2015) {
        data <- read_csv(
            temp,
            col_select = c("CONTROL", "WEIGHT", "INTSTATUS", "TENURE", "MARKETVAL", "HINCP", "TOTHCAMT"),
            col_types = "cdccddd"
            ) %>% mutate(YEAR = survey_year)
    } else if (survey_year >= 2001) { # Variable names changed in 2015
        data <- read_csv(
            temp,
            col_select = c("CONTROL", "WGT90GEO", "ISTATUS", "TENURE", "VALUE", "ZINC2", "ZSMHC"),
            col_types = "cdccddd", 
            na = c("", "NA", ".", paste0("'", toupper(letters), "'")) # Missing data indicated with . and, in some cases, letters
            ) %>% 
            rename(WEIGHT = WGT90GEO, INTSTATUS = ISTATUS, MARKETVAL = VALUE, HINCP = ZINC2, TOTHCAMT = ZSMHC) %>% 
            mutate(YEAR = survey_year)
    } else { # WEIGHTING VARIABLE CHANGED IN 2001
        data <- read_csv(
            temp, 
            col_select = c("CONTROL", "WEIGHT", "ISTATUS", "TENURE", "VALUE", "ZINC2", "ZSMHC"),
            col_types = "cdccddd", 
            na = c("", "NA", ".", paste0("'", toupper(letters), "'")) # Missing data indicated with . and, in some cases, letters
            ) %>% 
            rename(INTSTATUS = ISTATUS, MARKETVAL = VALUE, HINCP = ZINC2, TOTHCAMT = ZSMHC) %>% 
            mutate(YEAR = survey_year)
    }
    write_dataset(
        data,
        file.path(getwd(), data_path, "ahs"),
        format = "parquet",
        partitioning = "YEAR",

    )

    unlink(temp)
    # Write this survey year to downloaded files cache
    cat(url, sep = "\n", file = DOWNLOADED_FILES_PATH, append = TRUE)
}

lapply(urls, load_ahs_data)
