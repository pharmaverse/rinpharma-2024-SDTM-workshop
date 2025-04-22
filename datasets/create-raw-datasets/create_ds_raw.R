#########Create Raw DS Dataset#########
sdtm_ds <- pharmaversesdtm::ds
ds_raw <- pharmaversesdtm::ds |>
  dplyr::mutate(
    STUDY = "CDISCPILOT01",
    PATNUM = substr(USUBJID, 4, 11),
    SITENM = "CDISCPILOT",
    INSTANCE = tools::toTitleCase(tolower(VISIT)),
    FORM = "DISC1",
    FORML = "Subject Disposition",
    IT.DSTERM = tools::toTitleCase(tolower(DSTERM)),
    IT.DSDECOD = tools::toTitleCase(tolower(DSDECOD)), #CT
    OTHERSP = NA_character_,
    DSDTCOL = ifelse(nchar(DSDTC) == 10, format(as.Date(DSDTC, format = "%Y-%m-%d"), "%m-%d-%Y"),
                     format(as.Date(DSDTC, format = "%Y-%m-%dT%H:%M"), "%m-%d-%Y")),
    DSTMCOL = ifelse(nchar(DSDTC) == 10, NA_character_, substr(DSDTC, 12, 16)),
    IT.DSSTDAT = format(as.Date(DSSTDTC, format = "%Y-%m-%d"), "%m-%d-%Y")
  ) |>
  dplyr::select(
    STUDY, PATNUM, SITENM, INSTANCE, FORM, FORML,
    IT.DSTERM, IT.DSDECOD, DSDTCOL, DSTMCOL, IT.DSSTDAT
  )

for (col in names(ds_raw)) {
  attributes(ds_raw[[col]]) <- NULL
}

write.csv(ds_raw, file = "~/rinpharma-2024-SDTM-workshop/datasets/subject_disposition_raw_data.csv", row.names = FALSE)
