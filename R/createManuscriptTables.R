#' @export
createTables <- function(analysisDetails){
  
  cohortList <- analysisDetails$cohortSettings
  
  tableList <- vector("list", length(cohortList))
  names(tableList) <- names(cohortList)
  
  for (i in seq_along(cohortList)) {
    paths <- utils::read.csv(file.path(analysisDetails$cohortSettings[[i]]$treatmentPatternsExportFolder, paste0("treatmentPathways.csv")))
    
    tableList[[i]] <- countTPCC(paths)%>%
      dplyr::rename_with(~stringr::str_replace(.x, "eventSeq_", "Tx"))
  }
  
  return(tableList)
}

#' @export
formatTables <- function(tableList, dropXlsxFiles = FALSE){
  
  outputLoc <- dirname(dirname(analysisDetails$cohortSettings[[1]]$treatmentPatternsExportFolder))
  
  if (!dir.exists(file.path(outputLoc, "manuscriptResults"))){
    dir.create(file.path(outputLoc, "manuscriptResults"))
  }
  
  if (dropXlsxFiles)
    file.remove(file.path(outputLoc, "manuscriptResults", "tables.xlsx"))
  
  formatedTableList <- vector("list", 2)
  names(formatedTableList) <- c("TherapiesOverview", "MonotherapiesCounts")
  manuscriptList <- rep(list(formatedTableList), length(tableList))
  names(manuscriptList) <- names(analysisDetails$cohortSettings)
  for (i in seq_along(tableList)) {
    #Table 1
    overallTreated <- tableList[[i]] %>% filter(path == "Overall Treated") %>% dplyr::select("Overall") %>% dplyr::pull()
    
    table1 <- tableList[[i]] %>%
      dplyr::filter(path %in% c("Monotherapy", "Combinations", "Freq less than 5", "Validated Treatments", "Overall Treated")) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round((.x/overallTreated)*100, 2), .names = "{col}_%")) %>%
      dplyr::relocate(sort(names(.)), .after = path) %>%
      dplyr::select(path, dplyr::starts_with("Tx"), dplyr::starts_with("Overall"))
    
    # Table 2
    table2 <- tableList[[i]] %>%
      dplyr::filter(!(path %in% c("Monotherapy", "Combinations", "Freq less than 5", "Validated Treatments", "Overall Treated"))) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round((.x/sum(.x))*100, 2), .names = "{col}_%")) %>%
      dplyr::relocate(sort(names(.)), .after = path) %>%
      dplyr::select(path, dplyr::starts_with("Tx"), dplyr::starts_with("Overall"))
    
    xlsx::write.xlsx(table1, file = file.path(outputLoc, "manuscriptResults", "tables.xlsx"), 
                     sheetName = paste0(names(analysisDetails$cohortSettings[i]), "_t1"),  
                     append = TRUE,
                     password = NULL)
    
    xlsx::write.xlsx(table2, file = file.path(outputLoc, "manuscriptResults", "tables.xlsx"), 
                     sheetName = paste0(names(analysisDetails$cohortSettings[i]), "_t2"), 
                     append = TRUE, 
                     password = NULL)
    
    manuscriptList[[i]][[1]] <- table1
    manuscriptList[[i]][[2]] <- table2
  }
  
  return(manuscriptList)
}