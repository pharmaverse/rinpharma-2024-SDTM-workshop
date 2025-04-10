#########Create Raw AE Dataset#########
sdtm_ae <- pharmaversesdtm::ae
ae_raw <- sdtm_ae |>
  dplyr::mutate(
    STUDY = "CDISCPILOT01",
    PATNUM = substr(USUBJID, 4, 11),
    FOLDER = "AE",
    FOLDERL = "Adverse Events",
    IT.AETERM = tools::toTitleCase(tolower(AETERM)),
    IT.AESEV = tools::toTitleCase(tolower(AESEV)),
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
    AELLT, AELLTCD, AEDECOD, AEHLT, AEHLTCD, AEHLGT, AEHLGTCD, AEBODSYS, AEBDSYCD, AESOC, AESOCCD,
    IT.AESEV, IT.AESER, IT.AEREL, IT.AEACN, IT.AEREL,
    AEOUTCOME, AESCAN, AESCNO, AEDIS,
    IT.AESDTH, IT.AESHOSP, IT.AESLIFE,
    AESOD, AEDTCOL, IT.AESTDAT, IT.AEENDAT
  )

for (col in names(ae_raw)) {
  attributes(ae_raw[[col]]) <- NULL
}
