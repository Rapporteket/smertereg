#' Query registry data from smerte db
#'
#' @return a data frame with registry data
#' @export

smerteGetRegDataFromRapporteketDb <- function() {
  
  registryName <- "smerte"
  dbType <- "mysql"
  
  query <- "
  SELECT
    PasientID,
    ForlopsID,
    AntTilsLege,
    AntTilsSykPleier,
    AntTilsFysioT,
    AntTilsPsyk,
    AntTilsSosio,
    Tilsett,
    InnlAvd,
    AntPasTils,
    StartdatoTO,
    AvdRESH
  FROM
    AlleVarNum
"
  
  dat <- rapbase::LoadRegData(registryName, query)
  
  dat$StartdatoTO <- lapply(dat$StartdatoTO, readr::parse_datetime,
                            format = "%Y-%m-%d")
  
  return(dat)
  
}