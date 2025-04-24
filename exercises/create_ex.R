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

# Read CT Specification
study_ct <- read.csv("./datasets/sdtm_ct_pharmasug.csv")

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

# Create EX domain.

# Map topic variable EXTRT using assign_no_ct algorithm
ex <-
  assign_no_ct(
    raw_dat = ex_raw,
    raw_var = "DRUGAD",
    tgt_var = "EXTRT",
    id_vars = oak_id_vars()
  ) %>%
  # Map EXDOSE using assign_no_ct algorithm
  assign_no_ct(
    raw_dat = ex_raw,
    raw_var = "IT.ECDSTXT",
    tgt_var = "EXDOSE",
    id_vars = oak_id_vars()
  ) %>%
  # Map EXDOSU using assign_ct algorithm
  assign_ct(
    raw_dat = ex_raw,
    raw_var = "IT.ECDOSU",
    tgt_var = "EXDOSU",
    ct_spec = study_ct,
    ct_clst = "C71620",
    id_vars = oak_id_vars()
  ) %>%
  # Map EXDOSFRM using assign_ct algorithm
  assign_ct(
    raw_dat = ex_raw,
    raw_var = "DOSFM",
    tgt_var = "EXDOSFRM",
    ct_spec = study_ct,
    ct_clst = "C66726",
    id_vars = oak_id_vars()
  ) %>%
  # Map EXDOSFRQ using assign_ct algorithm
  assign_ct(
    raw_dat = ex_raw,
    raw_var = "DOSFRQ",
    tgt_var = "EXDOSFRQ",
    ct_spec = study_ct,
    ct_clst = "C71113",
    id_vars = oak_id_vars()
  ) %>%
  # Map EXROUTE using assign_ct algorithm
  assign_ct(
    raw_dat = ex_raw,
    raw_var = "IT.ECROUTE",
    tgt_var = "EXROUTE",
    ct_spec = study_ct,
    ct_clst = "C66729",
    id_vars = oak_id_vars()
  ) %>%
  # Map EXSTDTC using assign_datetime
  assign_datetime(
    raw_dat = ex_raw,
    raw_var = "IT.ECSTDAT",
    tgt_var = "EXSTDTC",
    raw_fmt = c("dd-mmm-yyyy"),
    id_vars = oak_id_vars()
  ) %>%
  # Map EXENDTC using assign_datetime
  assign_datetime(
    raw_dat = ex_raw,
    raw_var = "IT.ECENDAT",
    tgt_var = "EXENDTC",
    raw_fmt = c("dd-mmm-yyyy"),
    id_vars = oak_id_vars()
  ) %>%
  mutate(STUDYID = ex_raw$STUDY,
         DOMAIN = "EX",
         USUBJID = paste0("01-", ex_raw$PATNUM),
         EXREFID = ex_raw$IT.ECREFID) %>%
  # Map VISIT using assign_ct
  assign_ct(
    raw_dat = ex_raw,
    raw_var = "VISITNAME",
    tgt_var = "VISIT",
    ct_spec = study_ct,
    ct_clst = "VISIT",
    id_vars = oak_id_vars()
  ) %>%
  # Map VISITNUM using assign_ct
  assign_ct(
    raw_dat = ex_raw,
    raw_var = "VISITNAME",
    tgt_var = "VISITNUM",
    ct_spec = study_ct,
    ct_clst = "VISITNUM",
    id_vars = oak_id_vars()
  ) %>%
  # Derive DY variables
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "EXSTDTC",
    refdt = "RFXSTDTC",
    study_day_var = "EXSTDY"
  ) %>%
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "EXENDTC",
    refdt = "RFXENDTC",
    study_day_var = "EXENDY"
  ) %>%
  # Derive sequence number
  derive_seq(tgt_var = "EXSEQ",
             rec_vars= c("USUBJID", "EXTRT")) %>%
  select("STUDYID", "DOMAIN", "USUBJID", "EXSEQ",  "EXTRT", "EXDOSE", "EXDOSU", "EXDOSFRM", "EXDOSFRQ", "EXROUTE",  "VISITNUM", "VISIT",
         "EXSTDTC", "EXENDTC", "EXSTDY", "EXENDY")
