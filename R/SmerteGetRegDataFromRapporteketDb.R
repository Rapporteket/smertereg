smerteGetRegDataFromRapporteketDb <- function() {
  
  registryName <- "smerte"
  dbType <- "mysql"
  
  query <- "SELECT
    FROM
    Tables_in_SmerteReportDataStaging
1                           AlleVar
2                        AlleVarNum
3                 Avdelingsoversikt
4                       Brukerliste
5                   ForlopsOversikt
6                    SkjemaOversikt
7                   SmerteDiagnoser
8                SmerteDiagnoserNum"
  
  dat <- rapbase::LoadRegData(registryName, query)
  
  return(dat)
  
}