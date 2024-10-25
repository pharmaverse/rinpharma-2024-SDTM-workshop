# Name: CM domain
#
# Label: R program to create CM Domain
#
# Input raw data: cm_raw
# study_controlled_terminology : study_ct
#

library(sdtm.oak)
library(dplyr)


# Read Specification

study_ct <- read.csv("./datasets/sdtm_ct.csv")

# Read in raw data

cm_raw <- read.csv("./datasets/cm_raw_data_cdash.csv", 
                   stringsAsFactors = FALSE,
                   na.strings = "")

# derive oak_id_vars
cm_raw <- cm_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "cm_raw"
  )

dm <- read.csv("./datasets/dm.csv")

# Create CM domain. The first step in creating CM domain is to create the topic variable

cm <-
  # Derive topic variable
  # Map CMTRT using assign_no_ct, raw_var=IT.CMTRT,tgt_var=CMTRT
  assign_no_ct(
    raw_dat = cm_raw,
    raw_var = "IT.CMTRT",
    tgt_var = "CMTRT"
  ) %>%
  # Map CMINDC using assign_no_ct, raw_var=IT.CMINDC,tgt_var=CMINDC
  assign_no_ct(
    raw_dat = cm_raw,
    raw_var = "IT.CMINDC",
    tgt_var = "CMINDC",
    id_vars = oak_id_vars()
  ) %>%
  # Map CMDOSTXT using condition_add and assign_no_ct, raw_var=IT.CMDSTXT,tgt_var=CMDOS
  # If IT.CMDSTXT is numeric, map it to CMDOS
  assign_no_ct(
    raw_dat = condition_add(cm_raw, grepl("^-?\\d*(\\.\\d+)?(e[+-]?\\d+)?$", cm_raw$IT.CMDSTXT)),
    raw_var = "IT.CMDSTXT",
    tgt_var = "CMDOS",
    id_vars = oak_id_vars()
  ) %>%
  # Map qualifier CMDOSTXT using condition_add & assign_no_ct, raw_var=IT.CMDSTXT,tgt_var=CMDOSTXT
  # If IT.CMDSTXT is character, map it to CMDOSTXT
  assign_no_ct(
    raw_dat = condition_add(cm_raw, grepl("[^0-9eE.-]", cm_raw$IT.CMDSTXT)),
    raw_var = "IT.CMDSTXT",
    tgt_var = "CMDOSTXT",
    id_vars = oak_id_vars()
  ) %>%
  # Map CMDOSU and apply CT using assign_ct, raw_var=IT.CMDOSU,tgt_var=CMDOSU
  assign_ct(
    raw_dat = cm_raw,
    raw_var = "IT.CMDOSU",
    tgt_var = "CMDOSU",
    ct_spec = study_ct,
    ct_clst = "C71620",
    id_vars = oak_id_vars()
  ) %>%
  # Map CMDOSFRM and apply CT using assign_ct, raw_var=IT.CMDOSFRM,tgt_var=CMDOSFRM
  assign_ct(
    raw_dat = cm_raw,
    raw_var = "IT.CMDOSFRM",
    tgt_var = "CMDOSFRM",
    ct_spec = study_ct,
    ct_clst = "C66726",
    id_vars = oak_id_vars()
  ) %>%
  # Map CMDOSFRQ using assign_ct, raw_var=IT.CMDOSFRQ,tgt_var=CMDOSFRQ
  assign_ct(
    raw_dat = cm_raw,
    raw_var = "IT.CMDOSFRQ",
    tgt_var = "CMDOSFRQ",
    ct_spec = study_ct,
    ct_clst = "C71113",
    id_vars = oak_id_vars()
  ) %>%
  # Map CMROUTE using assign_ct, raw_var=IT.CMROUTE,tgt_var=CMROUTE
  assign_ct(
    raw_dat = cm_raw,
    raw_var = "IT.CMROUTE",
    tgt_var = "CMROUTE",
    ct_spec = study_ct,
    ct_clst = "C66729",
    id_vars = oak_id_vars()
  ) %>%
  # Map CMSTDTC using assign_no_ct, raw_var=IT.CMSTDAT,tgt_var=CMSTDTC
  assign_datetime(
    raw_dat = cm_raw,
    raw_var = "IT.CMSTDAT",
    tgt_var = "CMSTDTC",
    raw_fmt = c("d-m-y"),
    raw_unk = c("UN", "UNK")
  ) %>%
  # Map CMENDTC using assign_no_ct, raw_var=IT.CMENDAT,tgt_var=CMENDTC
  assign_datetime(
    raw_dat = cm_raw,
    raw_var = "IT.CMENDAT",
    tgt_var = "CMENDTC",
    raw_fmt = c("d-m-y"),
    raw_unk = c("UN", "UNK")
  ) %>%
  dplyr::mutate(
    STUDYID = "test_study",
    DOMAIN = "CM",
    CMCAT = "GENERAL CONMED",
    USUBJID = paste0("test_study", "-", cm_raw$PATNUM)
  ) %>%
  derive_seq(tgt_var = "CMSEQ",
             rec_vars= c("USUBJID", "CMTRT")) %>%
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "CMENDTC",
    refdt = "RFXSTDTC",
    study_day_var = "CMENDY"
  ) %>%
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "CMSTDTC",
    refdt = "RFXSTDTC",
    study_day_var = "CMSTDY"
  ) %>%
  dplyr::select("STUDYID", "USUBJID", everything())
