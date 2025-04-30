#' Name: DS domain
#'
#' Label: R program to create DS Domain
#' 
#' Input 
#' raw data: subject_disposition_raw_data.csv
#' study_controlled_terminology : sdtm_ct_pharmasug.csv
#' dm domain : pharmaversesdtm::dm
#'

library(sdtm.oak)
library(dplyr)

# Read CT Specification
study_ct <- read.csv("./datasets/sdtm_ct_pharmasug.csv")

# Read in raw data
ds_raw <- read.csv("./datasets/subject_disposition_raw_data.csv",
                   stringsAsFactors = FALSE)

ds_raw <- admiral::convert_blanks_to_na(ds_raw)

# Derive oak_id_vars
ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

# Read in DM domain to derive study day
dm <- pharmaversesdtm::dm

dm <- admiral::convert_blanks_to_na(dm)

# Create DS domain.

# Map topic variable DSTERM using assign_no_ct algorithm
ds <- 
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSTERM",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "OTHERSP",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) %>%
  # Map DSDECOD using assign_ct algorithm
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    ct_spec = study_ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "OTHERSP",
    tgt_var = "DSDECOD",
    ct_spec = study_ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  ) %>%
  # Map DSCAT using hardcode_ct algorithm
  hardcode_ct(
    raw_dat = condition_add(ds_raw, IT.DSTERM %in% c("Randomized")),
    raw_var = "IT.DSTERM",
    tgt_var = "DSCAT",
    tgt_val = "PROTOCOL MILESTONE",
    ct_spec = study_ct,
    ct_clst = "C74558",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = condition_add(ds_raw, !is.na(OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSCAT",
    tgt_val = "OTHER EVENT",
    ct_spec = study_ct,
    ct_clst = "C74558",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = condition_add(ds_raw, !IT.DSTERM %in% c("Final Lab Visit", "Final Retrieval Visit", "Randomized")),
    raw_var = "IT.DSTERM",
    tgt_var = "DSCAT",
    tgt_val = "DISPOSITION EVENT",
    ct_spec = study_ct,
    ct_clst = "C74558",
    id_vars = oak_id_vars()
  ) %>%
  # Map DSDTC using assign_datetime 
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = c("DSDTCOL", "DSTMCOL"),
    tgt_var = "DSDTC",
    raw_fmt = c("dd-mm-yyyy", "HH:MM"),
    id_vars = oak_id_vars()
  ) %>%
  # Map DSSDTC using assign_datetime
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = "IT.DSSTDAT",
    tgt_var = "DSSTDTC",
    raw_fmt = "dd-mm-yyyy",
    id_vars = oak_id_vars()
  ) %>%
  mutate(STUDYID = ds_raw$STUDY,
         DOMAIN = "DS",
         USUBJID = paste0("01-", ds_raw$PATNUM)) %>%
  # Map VISIT using assign_ct
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISIT",
    ct_spec = study_ct,
    ct_clst = "VISIT",
    id_vars = oak_id_vars()
  ) %>%
  # Map VISITNUM using assign_ct
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISITNUM",
    ct_spec = study_ct,
    ct_clst = "VISITNUM",
    id_vars = oak_id_vars()
  ) %>%
  # Derive DY variables
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "DSSTDTC",
    refdt = "RFXSTDTC",
    study_day_var = "DSSTDY"
  ) %>%
  # Derive sequence number
  derive_seq(tgt_var = "DSSEQ",
             rec_vars= c("USUBJID", "DSTERM")) %>%
  select("STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD", 
         "DSCAT", "VISITNUM", "VISIT", "DSDTC", "DSSTDTC", "DSSTDY")
