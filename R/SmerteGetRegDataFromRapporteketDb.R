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
  
  return(dat)
  
}