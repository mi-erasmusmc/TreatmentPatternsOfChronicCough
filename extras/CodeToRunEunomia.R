# Run Eunomia example
library(DatabaseConnector)
library(CDMConnector)
library(TreatmentPatterns)
library(TreatmentPatternsOfChronicCough)
library(Eunomia)
library(dplyr)
library(stringr)

######################################################
# Specify cdm details
cdmDatabaseSchema = "main"
cohortDatabaseSchema = "main"
resultsDatabaseSchema = 'main'
databaseId <- 'Eunomia'
databaseName <- "Eunomia"
cohortTable <- 'cohort_table'
outputFolder <- paste0("resultsEunomia_", stringr::str_replace_all(string = Sys.Date(), pattern = "-",replacement = "")) # change if you like

# Specify connection Details
# dbms = "YOUR_DBMS"
# server = "DATABASE_SERVER"
# user = Sys.getenv("LOGNAME")
# password = Sys.getenv("PASSWORD")
baseUrl = "NULL"
connectionDetails <- getEunomiaConnectionDetails()

######################################################
##### Study Specific Details #####
includeTreatments = "startDate" 
periodPriorToIndex = 0
minEraDuration = 0 
splitEventCohorts = NULL
splitTime = 21 
combinationWindow = 30
minPostCombinationDuration = 30
eraCollapseSize = 30
filterTreatments = "All"
maxPathLength = 5 

######################################################
##### DO NOT EDIT #####
con <- DBI::dbConnect(
  drv = duckdb::duckdb(),
  dbdir = eunomia_dir()
)

cdm <- CDMConnector::cdmFromCon(
  con = con,
  cdmSchema = cdmDatabaseSchema,
  writeSchema = resultsDatabaseSchema
)

# CohortDiagnostics details
diagnosticsExportFolder = file.path(outputFolder, "cohortDiagnosticsAnalysis")

# TreatmentPatterns details
treatmentPatternsExportFolder = file.path(outputFolder, "treatmentPatternsAnalysis")
## Cohorts to create
cohortDefinitionSet <- readCohortSet(
  path = system.file(package = "TreatmentPatterns", "exampleCohorts")
) %>%
  dplyr::mutate(cohortId = cohort_definition_id)
sqlDf<- data.frame()
for(i in seq_along(1:nrow(cohortDefinitionSet))){
  cohortName <- cohortDefinitionSet$cohort_name[i]
  cohortJson <- unlist(cohortDefinitionSet$json[i])
  cohortExpression= CirceR::cohortExpressionFromJson(cohortJson)
  cohortSql = CirceR::buildCohortQuery(cohortExpression, options = CirceR::createGenerateOptions(generateStats = FALSE))
  sqlDf <- rbind(sqlDf, data.frame(cohortId = i,
                        cohortName = cohortName, 
                        sql = cohortSql,
                        stringsAsFactors = FALSE))
}

cohortDefinitionSet <- dplyr::inner_join(cohortDefinitionSet, sqlDf, by = "cohortId")

## Cohort specification
cohorts <- cohortDefinitionSet %>%
  dplyr::select(-"cohort", -"json") %>%
  dplyr::mutate(type = c("event", "event", "event", "event", "exit", "event", "event", "target")) %>%
  # dplyr::rename(
  #   cohortId = "cohort_definition_id",
  #   cohortName = "cohort_name",
  # ) %>%
  dplyr::select(cohortId, cohortName, type)

cohorts <- list(viralSinusitis = cohorts, 
                viralSubusitis2 = cohorts)

treatmentPatternSettings <- vector("list", length(cohorts))

for (i in seq_along(cohorts)) {
  treatmentPatternSettings[[i]] <- list(includeTreatments = includeTreatments, 
                                      periodPriorToIndex = periodPriorToIndex,
                                      minEraDuration = minEraDuration, 
                                      splitEventCohorts = splitEventCohorts,
                                      splitTime = splitTime, 
                                      combinationWindow = combinationWindow, 
                                      minPostCombinationDuration = minPostCombinationDuration, 
                                      eraCollapseSize = eraCollapseSize,
                                      filterTreatments = filterTreatments, 
                                      maxPathLength = maxPathLength)
}

analysisDetails <- createAnalysisDetails(
  connection = con, 
  cdm = cdm, 
  cdmDatabaseSchema = cdmDatabaseSchema, 
  cohortDatabaseSchema = cohortDatabaseSchema, 
  resultsDatabaseSchema = resultsDatabaseSchema,
  cohortTableNames = cohortTable, 
  cohortDefinitionSet = cohortDefinitionSet, 
  cohortTable = cohortTable, 
  databaseId = databaseId, 
  databaseName = databaseName,
  incremental = FALSE, 
  baseUrl = baseUrl, 
  cohorts = cohorts, 
  treatmentPattersSettings = treatmentPatternSettings,
  diagnosticsExportFolder = diagnosticsExportFolder, 
  treatmentPatternsExportFolder = treatmentPatternsExportFolder
)

TreatmentPatternsOfChronicCough::execute(createCohorts = TRUE, 
                                         executeDiagnostics = FALSE, 
                                         executeTreatmentPatterns = TRUE, 
                                         connectionDetails = connectionDetails, 
                                         analysisDetails = analysisDetails)




