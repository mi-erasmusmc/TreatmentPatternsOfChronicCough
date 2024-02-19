#' @export
performTreatmentPatterns <- function(connectionDetails, 
                                     analysisDetails
                                     ){
  
  cdmDatabaseSchema = analysisDetails$cdmSettings$cdmDatabaseSchema
  cohortDatabaseSchema = analysisDetails$cdmSettings$cohortDatabaseSchema
  resultsDatabaseSchema = analysisDetails$cdmSettings$resultsDatabaseSchema
  cohortTable = analysisDetails$cdmSettings$cohortTable
  cdm <- analysisDetails$cdmSettings$cdm
  baseUrl = analysisDetails$cdmSettings$baseUrl
  cohortList = analysisDetails$cohortSettings
  
  for (i in seq_along(cohortList)) {
    
    if (!dir.exists(file.path(analysisDetails$cohortSettings[[i]]$treatmentPatternsExportFolder))) {
      dir.create(file.path(analysisDetails$cohortSettings[[i]]$treatmentPatternsExportFolder), recursive = TRUE)
    }
    
    pathways <- TreatmentPatterns::computePathways(cohorts = cohortList[[i]]$cohorts, 
                                                   cohortTableName = cohortTable, 
                                                   connectionDetails = connectionDetails,
                                                   cdmSchema = cdmDatabaseSchema,
                                                   resultSchema = resultsDatabaseSchema,
                                                   includeTreatments = cohortList[[i]]$treatmentPattersSettings$includeTreatments, 
                                                   periodPriorToIndex = cohortList[[i]]$treatmentPattersSettings$periodPriorToIndex,
                                                   minEraDuration = cohortList[[i]]$treatmentPattersSettings$minEraDuration, 
                                                   splitEventCohorts = cohortList[[i]]$treatmentPattersSettings$splitEventCohorts,
                                                   splitTime = cohortList[[i]]$treatmentPattersSettings$splitTime, 
                                                   combinationWindow = cohortList[[i]]$treatmentPattersSettings$combinationWindow, 
                                                   minPostCombinationDuration = cohortList[[i]]$treatmentPattersSettings$minPostCombinationDuration, 
                                                   eraCollapseSize = cohortList[[i]]$treatmentPattersSettings$eraCollapseSize,
                                                   filterTreatments = cohortList[[i]]$treatmentPattersSettings$filterTreatments, 
                                                   maxPathLength = cohortList[[i]]$treatmentPattersSettings$maxPathLength)
    
    Andromeda::saveAndromeda(andromeda = pathways, 
                             fileName = file.path(analysisDetails$cohortSettings[[i]]$treatmentPatternsExportFolder, "pathways"), 
                             maintainConnection = T)
    
    TreatmentPatternsOfChronicCough::exportTPCC(pathways, 
                                                outputPath = file.path(analysisDetails$cohortSettings[[i]]$treatmentPatternsExportFolder), 
                                                censorType = "remove",
                                                minCellCount = 0)
  }
  
  
  return(invisible())
  
}