#' @export
performTreatmentPatterns <- function(connectionDetails, 
                                     analysisDetails, 
                                     runCreateCohorts = FALSE, 
                                     runCohortCharacterization = TRUE, 
                                     runConstructPathways = TRUE, 
                                     runGenerateOutput = TRUE, 
                                     launchShiny = FALSE){
  
  cdmDatabaseSchema = analysisDetails$cdmDatabaseSchema
  cohortDatabaseSchema = analysisDetails$cohortDatabaseSchema
  cohortTable = analysisDetails$cohortTable
  saveSettings = analysisDetails$saveSettings
  baseUrl = analysisDetails$baseUrl
  
  
  cohortSettings <- TreatmentPatterns::createCohortSettings(
    targetCohorts = data.frame(cohortId = c(1, 2),
                               atlasId = c(1062, 1076),
                               cohortName = c('ChronicCough', 'Sensitivity'),
                               conceptSet = c("", "")),
    eventCohorts = data.frame(cohortId =  c(11:21),
                              atlasId = c(1064:1074),
                              cohortName = c("Inh.Cortecosteroids",
                                             "MacrolideAntibiotics",
                                             "Bronchodilators",
                                             "DrugsForAcidDis",
                                             "GastrointestinalMotilit",
                                             "LeukotrieneRecAntag",
                                             "SystemicAntihistamines",
                                             "Gabapentin",
                                             "TriglycicAntidepr",
                                             "Pregabalin",
                                             "Opioids"),
                              conceptSet = c(rep("", 11))),
    baseUrl = baseUrl,
    loadCohorts = TRUE)
  
characterizationSettings <- TreatmentPatterns::createCharacterizationSettings(
  baselineCovariates =  data.frame(covariateName = c('Male', 'Age', 'Charlson comorbidity index score'),
                                   covariateId = c(8507001, 1002, 1901)),
  returnCovariates = "selection")

dataSettings <- TreatmentPatterns::createDataSettings(OMOP_CDM = TRUE,
                                   connectionDetails = connectionDetails,
                                   cdmDatabaseSchema = cdmDatabaseSchema,
                                   cohortDatabaseSchema = cohortDatabaseSchema,
                                   cohortTable = cohortTable)

pathwaySettings <- TreatmentPatterns::createPathwaySettings(pathwaySettings_list = list(
  TreatmentPatterns::addPathwaySettings(studyName = c("default"), 
  targetCohortId = 1, 
  eventCohortIds = c(11:21)
   ), 
   TreatmentPatterns::addPathwaySettings(studyName = c("sensitivity"), 
   targetCohortId = 2, 
   eventCohortIds = c(11:21)
   )
  )
  )



TreatmentPatterns::executeTreatmentPatterns(dataSettings = dataSettings,
                                            cohortSettings = cohortSettings,
                                            characterizationSettings = characterizationSettings,
                                            pathwaySettings = pathwaySettings,
                                            saveSettings = saveSettings, 
                                            runCreateCohorts = runCreateCohorts, 
                                            runCohortCharacterization = runCohortCharacterization, 
                                            runConstructPathways = runConstructPathways, 
                                            runGenerateOutput = runGenerateOutput, 
                                            launchShiny = launchShiny)

return(invisible())

}