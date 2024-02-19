#' @export
createCohorts <- function(connectionDetails, 
                          analysisDetails){
  
  cohortTable <- analysisDetails$cdmSettings$cohortTable
  cdmDatabaseSchema <- analysisDetails$cdmSettings$cdmDatabaseSchema
  cohortDatabaseSchema <- analysisDetails$cdmSettings$cohortDatabaseSchema
  cohortDefinitionSet <- analysisDetails$cdmSettings$cohortDefinitionSet
  incremental <- analysisDetails$cdmSettings$incremental
  projectRoot <- system.file(package = "TreatmentPatternsOfChronicCough")
  
  
  # cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(settingsFileName = file.path(projectRoot, "settings", "CohortsToCreate.csv"), 
  #                                                               jsonFolder = file.path(projectRoot,"cohorts"), 
  #                                                               sqlFolder = file.path(projectRoot, "sql", "sql_server")
  #                                                               )
  
  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)
  
  CohortGenerator::createCohortTables(connectionDetails = connectionDetails, 
                                      cohortDatabaseSchema = cohortDatabaseSchema, 
                                      cohortTableNames = cohortTableNames,
                                      incremental = incremental)
  
  CohortGenerator::generateCohortSet(connectionDetails = connectionDetails,
                                     cdmDatabaseSchema = cdmDatabaseSchema, 
                                     cohortDatabaseSchema = cohortDatabaseSchema,
                                     cohortTableNames = cohortTableNames, 
                                     cohortDefinitionSet = cohortDefinitionSet, 
                                     incremental = incremental)
  cohortCounts <- CohortGenerator::getCohortCounts(connectionDetails = connectionDetails,
                                                   cohortDatabaseSchema = cohortDatabaseSchema, 
                                                   cohortTable = cohortTable)
  
  message(paste0("Cohorts created."))
  print(cohortCounts)
  
  return(invisible())
}