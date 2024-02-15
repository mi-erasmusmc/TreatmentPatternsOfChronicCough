#' @export
performDiagnostics <- function(connectionDetails, 
                               analysisDetails, 
                               dropTables = TRUE, 
                               createResultsFile = TRUE){
  
  cohortDefinitionSet <- analysisDetails$cohortDefinitionSet
  cohortTable <- analysisDetails$cohortTable
  cdmDatabaseSchema <- analysisDetails$cdmDatabaseSchema
  cohortDatabaseSchema <- analysisDetails$cohortDatabaseSchema
  exportFolder <- analysisDetails$diagnosticsExportFolder
  minCellCount <- analysisDetails$minCellCount
  cohortTableNames = analysisDetails$cohortTableNames
  databaseId = analysisDetails$databaseId
  
  CohortDiagnostics::executeDiagnostics(cohortDefinitionSet = cohortDefinitionSet,
                                        connectionDetails = connectionDetails, 
                                        cohortTable = cohortTable, 
                                        cohortDatabaseSchema = cohortDatabaseSchema, 
                                        cdmDatabaseSchema = cdmDatabaseSchema, 
                                        exportFolder = exportFolder, 
                                        databaseId = databaseId, 
                                        minCellCount = minCellCount)
  
  if (dropTables){
    CohortGenerator::dropCohortStatsTables(
      connectionDetails = connectionDetails,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTableNames = cohortTableNames
    )
  }
  
  if (createResultsFile){
    CohortDiagnostics::createMergedResultsFile(exportFolder)
  }
  
  return(invisible())
}