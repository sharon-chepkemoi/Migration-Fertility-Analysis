################################################################################
### Restart R
.rs.restartR()

### Start with a clean environment by removing objects in workspace
rm(list=ls())

### Setting work directory
working_directory <- base::setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
working_directory <- base::setwd(".")

### Load Rdata
Rdata_files <- list.files(path = working_directory, pattern = "*.RData", full.names = T)

if ( length(Rdata_files) >0) {
  invisible(lapply(Rdata_files,load,.GlobalEnv))
} else {
  paste(c(".RData files", "do not exist"), collapse = " ")
}

### Install required packages and create output folders
source("./1.setup/requirements.R")

## load helper functions
source("./1.setup/helperfuns_read_excel_sheets.R")
source("./1.setup/helperfuns_gt_table.R")
source("./1.setup/helperfuns_save_theme.R")


# load data and recode files
source("./2.load_data_and_clean/load_data_local.R")
source("./2.load_data_and_clean/load_recode_file.R")

# clean each data, rename variables and reset labels
source("./2.load_data_and_clean/cleaning_basse_hdss.R")
source("./2.load_data_and_clean/cleaning_hararge_hdss.R")
source("./2.load_data_and_clean/cleaning_iganga_hdss.R")
source("./2.load_data_and_clean/cleaning_meiru_hdss.R")
source("./2.load_data_and_clean/cleaning_ouagadougou_hdss.R")
source("./2.load_data_and_clean/cleaning_niakhar_hdss.R")


# Merge all datasets and save merged data and dictionary
source("./2.load_data_and_clean/merge_data.R")
source("./2.load_data_and_clean/merge_data_final.R")

# filter variables and save modeling data
source("./2.load_data_and_clean/filter_modeling_data.R")

# Run descriptives and save plots
source("./3.analysis/descriptives.R")


################################################################################

## Save workspace at the end without working directory path

save(list = ls(all.names = TRUE)[!ls(all.names = TRUE) %in% c("working_directory", "mainDir", "subDir_data", "data_Dir",
                                                              "subDir_output", "output_Dir","Rdata_files"
                                                              )],
     file = "gbd_analysis.RData",
     envir = .GlobalEnv #parent.frame()
     )

################################################################################

## Run all files in Rstudio
source("main.R")

################################################################################

