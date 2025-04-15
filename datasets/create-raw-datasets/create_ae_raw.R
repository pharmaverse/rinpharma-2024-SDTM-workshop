#########Create Raw AE Dataset#########
sdtm_ae <- pharmaversesdtm::ae
ae_raw <- sdtm_ae |>
  dplyr::mutate(
    STUDY = "CDISCPILOT01",
    PATNUM = substr(USUBJID, 4, 11),
    FOLDER = "AE",
    FOLDERL = "Adverse Events",
    IT.AETERM = tools::toTitleCase(tolower(AETERM)),
    IT.AESEV = ifelse(AESEV == "MILD", "Mild Adverse Event",
                      ifelse(AESEV == "MODERATE", "Moderate Adverse Event",
                             "SEVERE", "Severe Adverse Event")),
    IT.AESER = ifelse(AESER == "Y", "Yes", "No"),
    IT.AEACN = AEACN,
    IT.AEREL = tools::toTitleCase(tolower(AEREL)), #CT
    IT.AEREL = ifelse(IT.AEREL == "Possible", "Possibly Related",
                      ifelse(IT.AEREL == "Probable", "Probably Related",
                             ifelse(IT.AEREL == "None", "Not Related", IT.AEREL))),
    AEOUTCOME = tools::toTitleCase(tolower(AEOUT)), #CT
    AESCAN = ifelse(AESCAN == "Y", "Yes", "No"),
    AESCNO = ifelse(AESCONG == "Y", "Yes", "No"),
    AEDIS = ifelse(AESDISAB == "Y", "Yes", "No"),
    IT.AESDTH = ifelse(AESDTH == "Y", "Yes", "No"),
    IT.AESHOSP = ifelse(AESHOSP == "Y", "Yes", "No"),
    IT.AESLIFE = ifelse(AESLIFE == "Y", "Yes", "No"),
    AESOD = ifelse(AESOD == "Y", "Yes", "No"),
    AEDTCOL = format(as.Date(AEDTC, format = "%Y-%m-%d"), "%m/%d/%Y"),
    IT.AESTDAT = ifelse(nchar(AESTDTC) == 10, format(as.Date(AESTDTC, format = "%Y-%m-%d"), "%m/%d/%Y"),
                        ifelse(nchar(AESTDTC) == 7, format(as.Date(AESTDTC, format = "%Y-%m"), "%m/%Y"),
                               ifelse(nchar(AESTDTC) == 4, format(as.Date(AESTDTC, format = "%Y"), "%Y"),
                                      AESTDTC))),
    IT.AEENDAT = ifelse(nchar(AEENDTC) == 10, format(as.Date(AEENDTC, format = "%Y-%m-%d"), "%m/%d/%Y"),
                        ifelse(nchar(AEENDTC) == 7, format(as.Date(AEENDTC, format = "%Y-%m"), "%m/%Y"),
                               ifelse(nchar(AEENDTC) == 4, format(as.Date(AEENDTC, format = "%Y"), "%Y"),
                                      AEENDTC)))
  ) |>
  dplyr::select(
    STUDY, PATNUM, FOLDER, FOLDERL, IT.AETERM, AEOUTCOME,
    AELLT, AELLTCD, AEDECOD, AEPTCD, AEHLT, AEHLTCD, AEHLGT, AEHLGTCD, AEBODSYS, AEBDSYCD, AESOC, AESOCCD,
    IT.AESEV, IT.AESER, IT.AEREL, IT.AEACN, IT.AEREL,
    AEOUTCOME, AESCAN, AESCNO, AEDIS,
    IT.AESDTH, IT.AESHOSP, IT.AESLIFE,
    AESOD, AEDTCOL, IT.AESTDAT, IT.AEENDAT
  )

# Map code from Meddra dictionary
# In this dataset, only the AELLTCD and AESOCCD can find a match
meddra <- arrow::read_parquet("xxx") # please update the file path here

for (col in names(meddra)) {
  attributes(meddra[[col]]) <- NULL
}

term <- tolower(unique(ae_raw$IT.AETERM))

sub_meddra <- meddra |>
  filter(tolower(PT_NAME) %in% term) |>
  rename(IT.AETERM = PT_NAME,
         AELLT = LLT_NAME,
         AESOC = SOC_NAME) |>
  mutate(IT.AETERM = tools::toTitleCase(tolower(IT.AETERM))) |>
  mutate(across(c("AELLT", "AESOC", "HLT_NAME", "HLGT_NAME"), toupper)) |>
  select(IT.AETERM, AELLT, LLT_CODE, AESOC, SOC_CODE) |>
  distinct()

ae_raw <- ae_raw |>
  dplyr::left_join(sub_meddra, by = c("IT.AETERM", "AELLT", "AESOC")) |>
  mutate(AELLTCD = LLT_CODE,
         AESOCCD = SOC_CODE) |>
  select(-LLT_CODE, -SOC_CODE)

for (col in names(ae_raw)) {
  attributes(ae_raw[[col]]) <- NULL
}

write.csv(ae_raw, file = "~/rinpharma-2024-SDTM-workshop/datasets/ae_raw_data.csv", row.names = FALSE)
