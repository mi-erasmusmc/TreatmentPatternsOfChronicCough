library(DatabaseConnector)
library(CohortGenerator)

library(TreatmentPatternsOfChronicCough)

# Specify study details
cdmDatabaseSchema = "YOUR_CDM_SCHEMA"
cohortDatabaseSchema = "YOUR_COHORT_SCHEMA"
vocabDatabaseSchema = 'YOUR_VOCABULARY_SCHEMA'
resultsDatabaseSchema = 'YOUR_RESULTS_SCHEMA'
databaseId <- 'YOUR_DATABASE_ID'
databaseName <- "YOUR_DATABASE_NAME"
cohortTable <- 'chronic_cough'
outputFolder <- paste0("results_", stringr::str_replace_all(string = Sys.Date(), pattern = "-",replacement = ""))

# Specify CohortDiagnostics details
diagnosticsExportFolder = file.path(outputFolder, "cohortDiagnosticsAnalysis")


# Specify TreatmentPatterns details
saveSettings <- TreatmentPatterns::createSaveSettings(databaseName = databaseName,
                                                      outputFolder = file.path(outputFolder, "treatmentPatternsAnalysis"))
treatmentPatternsExportFolder = file.path(outputFolder, "treatmentPatternsAnalysis")


# Specify connection Details
dbms = "YOUR_DBMS"
server = "DATABASE_SERVER"
user = Sys.getenv("LOGNAME")
password = Sys.getenv("PASSWORD")
baseUrl = "YOUR_ATLAS_BASE_URL"
cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(settingsFileName = file.path(getwd(), "inst/settings/CohortsToCreate.csv"),
                                                               jsonFolder = file.path(getwd(), "inst/cohorts"),
                                                               sqlFolder = file.path(getwd(), "inst/sql/sql_server"))

cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = "chronic_cough_diagnostics")

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms, 
                                                                user = user,
                                                                password = password,
                                                                server = server
                                                                )

analysisDetails <- createAnalysisDetails(
  cdmDatabaseSchema = cdmDatabaseSchema, 
  cohortDatabaseSchema = cohortDatabaseSchema, 
  cohortTableNames = cohortTableNames, 
  cohortDefinitionSet = cohortDefinitionSet, 
  cohortTable = cohortTable, 
  databaseId = databaseId, 
  databaseName = databaseName,
  minCellCount = minCellCount, 
  saveSettings = saveSettings, 
  baseUrl = baseUrl, 
  diagnosticsExportFolder = diagnosticsExportFolder, 
  treatmentPatternsExportFolder = treatmentPatternsExportFolder
)

TreatmentPatternsOfChronicCough::execute(createCohorts = TRUE, 
                                         executeDiagnostics = TRUE, 
                                         executeTreatmentPatterns = TRUE, 
                                         connectionDetails = connectionDetails, 
                                         analysisDetails = analysisDetails)

