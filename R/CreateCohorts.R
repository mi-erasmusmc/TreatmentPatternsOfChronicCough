#' @export
createCohorts <- function(connectionDetails, 
                          analysisDetails){
  
  cohortTable <- analysisDetails$cohortTable
  cdmDatabaseSchema <- analysisDetails$cdmDatabaseSchema
  cohortDatabaseSchema <- analysisDetails$cohortDatabaseSchema
  incremental <- analysisDetails$incremental
  projectRoot <- system.file(package = "TreatmentPatternsOfChronicCough")
  
  
  cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(settingsFileName = file.path(projectRoot, "settings", "CohortsToCreate.csv"), 
                                                                jsonFolder = file.path(projectRoot,"cohorts"), 
                                                                sqlFolder = file.path(projectRoot, "sql", "sql_server")
                                                                )
  
  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)
  
  CohortGenerator::createCohortTables(connectionDetails = connectionDetails, 
                                      cohortDatabaseSchema = cohortDatabaseSchema, 
                                      cohortTableNames = cohortTableNames, incremental = FALSE)
  
  CohortGenerator::generateCohortSet(connectionDetails = connectionDetails,
                                     cdmDatabaseSchema = cdmDatabaseSchema, 
                                     cohortDatabaseSchema = cohortDatabaseSchema,
                                     cohortTableNames = cohortTableNames, 
                                     cohortDefinitionSet = cohortDefinitionSet, 
                                     incremental = FALSE)
  return(invisible())
}