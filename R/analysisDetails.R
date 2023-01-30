#' @export
createAnalysisDetails <- function(cdmDatabaseSchema,
                                  cohortDatabaseSchema,
                                  cohortTableNames, 
                                  cohortDefinitionSet,
                                  cohortTable, 
                                  databaseId, 
                                  minCellCount, 
                                  baseUrl, 
                                  diagnosticsExportFolder,
                                  treatmentPatternsExportFolder){
  
  settings <- list(cdmDatabaseSchema = cdmDatabaseSchema, 
                   cohortDatabaseSchema = cohortDatabaseSchema, 
                   cohortTableNames = cohortTableNames, 
                   cohortDefinitionSet = cohortDefinitionSet, 
                   cohortTable = cohortTable, 
                   databaseId = databaseId, 
                   minCellCount = minCellCount, 
                   baseUrl = baseUrl, 
                   diagnosticsExportFolder = diagnosticsExportFolder, 
                   treatmentPatternsExportFolder = treatmentPatternsExportFolder
                   )
  
  return(settings)
}