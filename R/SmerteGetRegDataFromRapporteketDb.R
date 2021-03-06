#' Query registry data from smerte db
#'
#' @param avdresh_vector vector of key(s) for department(s) from which data is
#' returned
#' @return a data frame with registry data
#' @export

smerteGetRegDataFromRapporteketDb <- function(avdresh_vector) {
  
  registryName <- "smerte"
  dbType <- "mysql"
  
  query <- "
  SELECT
    PasientID as oqr_pasientid,
    ForlopsID as oqr_forlopsid,
    AntTilsLege as emp22_caregiver_doctor_times,
    AntTilsSykPleier as emp22_caregiver_nurse_times,
    AntTilsFysioT as emp22_caregiver_physiotherapist_times,
    AntTilsPsyk as emp22_caregiver_psychologist_times,
    AntTilsSosio as emp22_caregiver_social_worker_times,
    Tilsett as mce_supervision,
    InnlAvd as emp11_department,
    AntPasTils as emp22_consultations,
    StartdatoTO as mce_registered_date,
    AvdRESH
  FROM
    AlleVarNum
"
  
  query <- paste(query, "WHERE AvdRESH IN (",
                 paste(avdresh_vector, collapse = ', '), ")")
  
  
  dat <- rapbase::LoadRegData(registryName, query)
  
  dat$mce_registered_date <- readr::parse_datetime(dat$mce_registered_date,
                                                   format = "%Y-%m-%d")
  
  return(dat)
  
}