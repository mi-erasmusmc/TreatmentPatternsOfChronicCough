#' @export
exportTPCC <- function(andromeda, outputPath, ageWindow = 10, minCellCount = 5, censorType = "minCellCount", archiveName = NULL) {
  collection <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(Andromeda::isAndromeda(andromeda), add = collection)
  checkmate::assertPathForOutput(outputPath, overwrite = TRUE, add = collection)
  checkmate::assertIntegerish(ageWindow, min.len = 1, any.missing = FALSE, unique = TRUE, add = collection)
  checkmate::assertIntegerish(minCellCount, len = 1, lower = 0, add = collection)
  checkmate::assertChoice(censorType, choices = c("minCellCount", "remove", "mean"))
  checkmate::assertCharacter(archiveName, len = 1, add = collection, null.ok = TRUE)
  checkmate::reportAssertions(collection)
  
  nrows <- andromeda$treatmentHistory %>%
    dplyr::summarize(n()) %>%
    dplyr::pull()
  
  if (nrows == 0) {
    message("Treatment History table is empty. Nothing to export.")
    return(invisible(NULL))
  }
  
  if (!dir.exists(outputPath)) {
    dir.create(outputPath)
  }
  
  treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::collect() %>%
    dplyr::select(
      "personId", "indexYear", "age", "sex", "eventCohortName", "eventCohortId", "eventSeq", "durationEra")
  
  treatmentHistory <- dplyr::bind_rows(
    treatmentHistory,
    TreatmentPatterns:::getFilteredSubjects(andromeda)
  )
  
  # metadata
  metadataPath <- file.path(outputPath, "metadata.csv")
  message(sprintf("Writing metadata to %s", metadataPath))
  metadata <- andromeda$metadata %>% dplyr::collect()
  write.csv(metadata, file = metadataPath, row.names = FALSE)
  
  # Treatment Pathways
  treatmentPathwaysPath <- file.path(outputPath, "treatmentPathways.csv")
  message(sprintf("Writing treatmentPathways to %s", treatmentPathwaysPath))
  treatmentPathways <- TreatmentPatterns:::computeTreatmentPathways(
    treatmentHistory,
    ageWindow,
    minCellCount,
    censorType
  ) %>% dplyr::distinct()
  
  write.csv(treatmentPathways, file = treatmentPathwaysPath, row.names = FALSE)
  
  # Summary statistics duration
  statsTherapyPath <- file.path(outputPath, "summaryStatsTherapyDuration.csv")
  message(sprintf("Writing summaryStatsTherapyDuration to %s", statsTherapyPath))
  statsTherapy <- TreatmentPatterns:::computeStatsTherapy(treatmentHistory)
  write.csv(statsTherapy, file = statsTherapyPath, row.names = FALSE)
  
  # Counts
  counts <- TreatmentPatterns:::computeCounts(treatmentHistory, minCellCount)
  
  countsYearPath <- file.path(outputPath, "countsYear.csv")
  message(sprintf("Writing countsYearPath to %s", countsYearPath))
  write.csv(counts$year, file = countsYearPath, row.names = FALSE)
  
  countsAgePath <- file.path(outputPath, "countsAge.csv")
  message(sprintf("Writing countsAgePath to %s", countsAgePath))
  write.csv(counts$age, file = countsAgePath, row.names = FALSE)
  
  countsSexPath <- file.path(outputPath, "countsSex.csv")
  message(sprintf("Writing countsSexPath to %s", countsSexPath))
  write.csv(counts$sex, file = countsSexPath, row.names = FALSE)
  
  if (!is.null(archiveName)) {
    zipPath <- file.path(outputPath, archiveName)
    
    message(sprintf("Zipping files to %s", zipPath))
    
    utils::zip(
      zipfile = zipPath,
      files = c(
        treatmentPathwaysPath,
        countsYearPath,
        countsAgePath,
        countsSexPath,
        statsTherapyPath
      ),
      flags = "-j"
    )
  }
  return(invisible(NULL))
}
