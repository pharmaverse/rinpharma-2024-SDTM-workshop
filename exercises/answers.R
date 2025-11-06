library(sdtm.oak)
library(pharmaverseraw)
library(dplyr)

#AE aCRF - https://github.com/pharmaverse/pharmaverseraw/blob/main/vignettes/articles/aCRFs/AdverseEvent_aCRF.pdf

# Read in Raw dataset ----
ae_raw <- pharmaverseraw::ae_raw

# Generate oak_id_vars ----
ae_raw <- ae_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ae_raw"
  )

# Read in Controlled Terminology
study_ct <-  data.frame(
  codelist_code = c("C66742", "C66742"),
  term_code = c("C49487", "C49488"),
  term_value = c("N", "Y"),
  collected_value = c("No", "Yes"),
  term_preferred_term = c("No", "Yes"),
  term_synonyms = c("No", "Yes"),
  stringsAsFactors = FALSE
)

# Exercise 1 ------------------------------------------------
# Map AETERM from raw_var=IT.AETERM, tgt_var=AETERM
ae <-
  # Derive topic variable
  # Map AETERM using assign_no_ct, raw_var=IT.AETERM, tgt_var=AETERM
  assign_no_ct(
    raw_dat = ae_raw,
    raw_var = "IT.AETERM",
    tgt_var = "AETERM",
    id_vars = oak_id_vars()
  ) %>%
  # Exercise 2 ------------------------------------------------
# Map AESER using assign_no_ct, raw_var=IT.AESER, tgt_var=AESER
assign_ct(
  raw_dat = ae_raw,
  raw_var = "IT.AESER",
  tgt_var = "AESER",
  ct_spec = study_ct,
  ct_clst = "C66742",
  id_vars = oak_id_vars()
) %>%
  # Exercise 3 ------------------------------------------------
# Map AESDTH from raw_var=IT.AESDTH, tgt_var=AESDTH.Annotation text is 
#    If "Yes" then AESDTH = "Y" else Not Submitted. Codelist code for AESDTH is C66742
# Map AESDTH using condition_add & assign_ct, raw_var=IT.AESDTH, tgt_var=AESDTH
assign_ct(
  raw_dat = condition_add(ae_raw, IT.AESDTH == "Yes"),
  raw_var = "IT.AESDTH",
  tgt_var = "AESDTH",
  ct_spec = study_ct,
  ct_clst = "C66742",
  id_vars = oak_id_vars()
)