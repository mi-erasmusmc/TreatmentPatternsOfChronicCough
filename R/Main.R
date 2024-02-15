#' @export
execute <- function(createCohorts = TRUE, 
                    executeDiagnostics = TRUE, 
                    executeTreatmentPatterns = TRUE, 
                    connectionDetails, 
                    analysisDetails){
  
  if (createCohorts){
    createCohorts(connectionDetails, 
                  analysisDetails)
  }
  
  if (executeDiagnostics){
    performDiagnostics(connectionDetails, 
                       analysisDetails, 
                       dropTables = FALSE, 
                       createResultsFile = TRUE)
  }
  
  if (executeTreatmentPatterns){
    performTreatmentPatterns(connectionDetails, 
                             analysisDetails)
  }
}