#' Name: EX domain
#'
#' Label: R program to create EX Domain
#' 
#' Input 
#' raw data: exposure_raw_data.csv
#' study_controlled_terminology : sdtm_ct_pharmasug.csv
#' dm domain : pharmaversesdtm::dm
#'
library(sdtm.oak)
library(dplyr)
library(pharmaversesdtm)

# Read CT Specification
study_ct <- read.csv("datasets/sdtm_ct_pharmasug.csv")

# Read in raw data
ex_raw <- read.csv("./datasets/exposure_raw_data.csv", 
                   stringsAsFactors = FALSE) 

ex_raw <- admiral::convert_blanks_to_na(ex_raw)

# Derive oak_id_vars
ex_raw <- ex_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ex_raw"
  )

# Read in DM domain to derive study day
dm <- pharmaversesdtm::dm

dm <- admiral::convert_blanks_to_na(dm)
