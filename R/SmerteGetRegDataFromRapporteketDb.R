smerteGetRegDataFromRapporteketDb <- function() {
  
  registryName <- "smerte"
  dbType <- "mysql"
  
  query <- "SELECT
    FROM"
  
  dat <- rapbase::LoadRegData(registryName, query)
  
  return(dat)
  
}