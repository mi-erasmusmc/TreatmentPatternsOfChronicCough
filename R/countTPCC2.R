# Grouping function
doGroupCombinationsTPCC <- function(treatmentPathways) {
    treatmentPathways <- treatmentPathways %>%
      mutate(path = stringr::str_replace_all(path, 
        pattern = ".+\\+.+",
        replacement = "Combination"
      ))
  return(treatmentPathways)
}

# How many patients received each treatment at each step
#' @export
countTreatmentEvents2 <- function(treatmentPathways){
  
  result <- treatmentPathways %>%
    filter(age == "all", sex == "all", indexYear == "all", path != "None", freq >= 5) %>%
    group_by(path) %>%
    summarise(freq = sum(freq)) %>%
    mutate(seqNo = row_number()) %>%
    ungroup() %>%
    separate_longer_delim(path, delim = "-") %>%
    group_by(seqNo) %>%
    mutate(eventId = row_number()) %>% 
    mutate(newPath = paste0(path, collapse = "-")) %>%
    ungroup() %>%
    group_by(path, eventId) %>%
    summarise(freq = sum(freq), .groups = "drop") %>%
    ungroup() %>%
    mutate(path = case_when(
      str_detect(path, "\\+") ~ "Combination",
      .default = path
    )) %>%
    group_by(path, eventId) %>%
    summarise(freq = sum(freq), .groups = "drop") %>%
    tidyr::pivot_wider(id_cols = path, names_from = eventId, values_from = freq, names_prefix = "eventSeq_")
  
  return(result)
  
}

# Overall for each treatment
#' @export
countTreatmentsOverall2 <- function(treatmentPathways){
  result <- treatmentPathways %>%
    dplyr::filter(age == "all", sex == "all", indexYear == "all") %>%
    dplyr::filter(path != "None") %>%
    dplyr::filter(freq >= 5) %>%
    dplyr::filter(stringr::str_detect(path, "\\+", negate = TRUE)) %>%
    dplyr::mutate(seqId = dplyr::row_number()) %>%
    tidyr::separate_longer_delim(path, delim = "-") %>%
    dplyr::group_by(seqId) %>%
    dplyr::mutate(eventId = row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(seqId) %>%
    dplyr::slice_max(eventId) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(path) %>%
    dplyr::summarise(Overall = base::sum(freq), .groups = "drop")
  return(result)
}

# How many patients treated at each step with freq less than 5
#' @export
countLessThanFive2 <- function(treatmentPathways){
  result <- treatmentPathways %>%
    dplyr::filter(age == "all", sex == "all", indexYear == "all") %>%
    dplyr::filter(path != "None") %>%
    dplyr::filter(freq < 5) %>%
    dplyr::mutate(seqId = dplyr::row_number()) %>%
    tidyr::separate_longer_delim(path, delim = "-") %>%
    dplyr::group_by(seqId) %>%
    dplyr::mutate(eventId = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(seqId) %>%
    dplyr::slice_max(eventId) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(eventId) %>%
    dplyr::summarise(freq = sum(freq), .groups = "drop") %>%
    dplyr::mutate(path = "Frequency less than five") %>%
    tidyr::pivot_wider(id_cols = path, names_from = eventId, values_from = freq, names_prefix = "eventSeq_") %>%
    dplyr::rowwise() 
    # dplyr::mutate(Overall = base::sum(dplyr::c_across(dplyr::where(base::is.numeric)), na.rm = TRUE))
  
  return(result)
}

#' @export
countSwitches <- function(treatmentPathways){
  result <- treatmentPathways %>%
    filter(age == "all", sex == "all", indexYear == "all", path != "None", freq >= 5) %>%
    group_by(path) %>%
    summarise(freq = sum(freq)) %>%
    mutate(seqNo = row_number()) %>%
    ungroup() %>%
    separate_longer_delim(path, delim = "-") %>%
    group_by(seqNo) %>%
    mutate(eventId = row_number()) %>% 
    ungroup() %>%
    mutate(path = case_when(
      str_detect(path, "\\+") ~ "Combination",
      .default = path
    )) %>%
    group_by(seqNo) %>%
    mutate(switch = if_else(path != lag(path), 1, 0)) %>%
    ungroup() %>%
    group_by(eventId) %>%
    summarise(freq = sum(switch, na.rm = T), .groups = "drop") %>%
    mutate(path = "Switched") %>%
    tidyr::pivot_wider(id_cols = path, names_from = eventId, values_from = freq, names_prefix = "eventSeq_")
}

#' @export
countTPCC2 <- function(treatmentPathways){
  
  
  armCounts <- countTreatmentEvents2(treatmentPathways)
  
  lessThanFive <- countLessThanFive2(treatmentPathways)
  
  switches <- countSwitches(treatmentPathways)

  result <- armCounts %>%
    dplyr::bind_rows(lessThanFive, switches) %>%
    dplyr::select(path, dplyr::starts_with("eventSeq")) 
  
  #replace NA
  result[is.na(result)] <- 0
  
  treated <- result %>%
    dplyr::summarise(dplyr::across(dplyr::where(base::is.numeric), ~base::sum(., na.rm = T))) %>%
    dplyr::mutate(path = "Total treated") %>%
    dplyr::select(path, dplyr::everything())
  
  monotherapy <- result %>%
    dplyr::filter(!(path %in% c("Combination", "Frequency less than five"))) %>%
    dplyr::summarise(dplyr::across(dplyr::where(base::is.numeric), ~base::sum(., na.rm = T))) %>%
    dplyr::mutate(path = "Monotherapy") %>%
    dplyr::select(path, dplyr::everything())
  
  treatedValid <- result %>%
    dplyr::filter(!(path %in% c("Frequency less than five"))) %>%
    dplyr::summarise(dplyr::across(dplyr::where(base::is.numeric), ~base::sum(., na.rm = T))) %>%
    dplyr::mutate(path = "Valid treatments") %>%
    dplyr::select(path, dplyr::everything())
  
  result <- result %>%
    dplyr::bind_rows(treatedValid, monotherapy, treated) 
  
  return(result)
}
