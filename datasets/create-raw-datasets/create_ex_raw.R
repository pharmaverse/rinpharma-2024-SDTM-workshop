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
    IT.ECSTDAT = format(as.Date(EXSTDTC, format = "%Y-%m-%d"), "%m/%d/%Y"),
    IT.ECENDAT = ifelse(is.na(EXENDTC), NA_character_, 
                              format(as.Date(EXENDTC, format = "%Y-%m-%d"), "%m/%d/%Y")),
    IT.ECDSTXT = EXDOSE,
    IT.ECDOSU = tolower(EXDOSU), #CT
    DOSFM = tolower(EXDOSFRM), #CT
    DOSFRQ = "Daily", #CT
    IT.ECROUTE = tools::toTitleCase(tolower(EXROUTE)) #CT
  ) |>
  dplyr::select(
    STUDY, PATNUM, VISITNAME, FOLDER, FOLDERL, IT.ECREFID, DRUGAD,
    IT.ECSTDAT, IT.ECENDAT, IT.ECDSTXT, IT.ECDOSU, DOSFM, DOSFRQ, IT.ECROUTE
  )
  