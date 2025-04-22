#########Create Raw Demographics Dataset#########
sdtm_dm <- pharmaversesdtm::dm
dm_raw <- sdtm_dm |>
  dplyr::mutate(
    STUDY = "CDISCPILOT01",
    PATNUM = substr(USUBJID, 4, 11),
    IT.AGE = AGE, #CT
    IT.SEX = ifelse(SEX == "F", "Female", "Male"), #CT
    IT.ETHNIC = tools::toTitleCase(tolower(ETHNIC)), #CT,
    IT.RACE = tools::toTitleCase(tolower(RACE)), #CT
    COUNTRY = COUNTRY,
    PLANNED_ARM = ifelse(ARM == "Xanomeline High Dose", "Xan High", 
                         ifelse(ARM == "Xanomeline Low Dose", "Xan Low", ARM)), #Customized CT
    PLANNED_ARMCD = ARMCD,
    ACTUAL_ARM = ifelse(ACTARM == "Xanomeline High Dose", "Xan High", 
                        ifelse(ACTARM == "Xanomeline Low Dose", "Xan Low", ACTARM)), #Customized CT
    ACTUAL_ARMCD = ACTARMCD,
    COL_DT = format(as.Date(DMDTC, format = "%Y-%m-%d"), "%m/%d/%Y"),
    IC_DT = RFICDTC
  ) |>
  dplyr::select(
    STUDY, PATNUM, IT.AGE, IT.ETHNIC, IT.RACE, COUNTRY, 
    PLANNED_ARM, PLANNED_ARMCD, ACTUAL_ARM, ACTUAL_ARMCD, COL_DT
  )

for (col in names(dm_raw)) {
  attributes(dm_raw[[col]]) <- NULL
}

write.csv(dm_raw, file = "~/rinpharma-2024-SDTM-workshop/datasets/dm_raw_data.csv", row.names = FALSE)
