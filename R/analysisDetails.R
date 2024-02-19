#' @export
createAnalysisDetails <- function(connection = NULL, 
                                  cdm = NULL, 
                                  cdmDatabaseSchema = NULL,
                                  cohortDatabaseSchema = NULL,
                                  resultsDatabaseSchema = NULL, 
                                  cohortTableNames, 
                                  cohortDefinitionSet,
                                  cohortTable, 
                                  databaseId, 
                                  baseUrl, 
                                  incremental = FALSE, 
                                  databaseName, 
                                  cohorts, 
                                  treatmentPattersSettings, 
                                  diagnosticsExportFolder,
                                  treatmentPatternsExportFolder){
  
  settings <- vector("list", 2)
  names(settings) <- c("cdmSettings", "cohortSettings")
  settings$cdmSettings <- list(connection = connection, 
                               cdm = cdm, 
                               cdmDatabaseSchema = cdmDatabaseSchema, 
                               resultsDatabaseSchema = resultsDatabaseSchema,
                               cohortDatabaseSchema = cohortDatabaseSchema, 
                               cohortTableNames = cohortTableNames, 
                               cohortDefinitionSet = cohortDefinitionSet, 
                               cohortTable = cohortTable, 
                               databaseId = databaseId, 
                               baseUrl = baseUrl, 
                               incremental = incremental, 
                               databaseName = databaseName)
  settings$cohortSettings <- vector("list", length(cohorts))
  for (i in seq_along(cohorts)) {
    settings$cohortSettings[[i]] <- list(cohorts = cohorts[[i]],
                                         treatmentPattersSettings = treatmentPattersSettings[[i]],
                                         diagnosticsExportFolder = file.path(diagnosticsExportFolder, paste0(names(cohorts[i]))), 
                                         treatmentPatternsExportFolder = file.path(treatmentPatternsExportFolder, paste0(names(cohorts[i])))
    )       
  }
  names(settings$cohortSettings) <- names(cohorts)
  
  return(settings)
}
