#########Create Raw Exposure Dataset#########
sdtm_ex <- pharmaversesdtm::ex

ec_raw <- sdtm_ex |>
  dplyr::mutate(
    STUDY = "CDISCPILOT01",
    PATNUM = substr(USUBJID, 4, 11),
    VISITNAME = tools::toTitleCase(tolower(VISIT)),
    FOLDER = "EC",
    FOLDERL = "Exposure as Collected",
    IT.ECREFID = "123",
    DRUGAD = EXTRT,
    IT.ECSTDAT = format(as.Date(EXSTDTC, format = "%Y-%m-%d"), "%d-%b-%Y"),
    IT.ECENDAT = ifelse(is.na(EXENDTC), NA_character_, 
                              format(as.Date(EXENDTC, format = "%Y-%m-%d"), "%d-%b-%Y")),
    IT.ECDSTXT = EXDOSE,
    IT.ECDOSU = "Milligram", #CT
    DOSFM = tolower(EXDOSFRM), #CT
    DOSFRQ = "Daily", #CT
    IT.ECROUTE = tools::toTitleCase(tolower(EXROUTE)) #CT
  ) |>
  dplyr::select(
    STUDY, PATNUM, VISITNAME, FOLDER, FOLDERL, IT.ECREFID, DRUGAD,
    IT.ECSTDAT, IT.ECENDAT, IT.ECDSTXT, IT.ECDOSU, DOSFM, DOSFRQ, IT.ECROUTE
  )

for (col in names(ec_raw)) {
  attributes(ec_raw[[col]]) <- NULL
}

write.csv(ec_raw, file = "~/rinpharma-2024-SDTM-workshop/datasets/exposure_raw_data.csv", row.names = FALSE)
