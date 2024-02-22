# How many patients received each treatment at each step
#' @export
countTreatmentEvents <- function(treatmentPathways){
  
  result <- treatmentPathways %>%
    dplyr::filter(age == "all", sex == "all", indexYear == "all") %>%
    dplyr::filter(path != "None") %>%
    dplyr::filter(freq >= 5) %>%
    dplyr::filter(stringr::str_detect(path, "\\+", negate = TRUE)) %>%
    dplyr::mutate(seqId = dplyr::row_number()) %>%
    tidyr::separate_longer_delim(path, delim = "-") %>%
    dplyr::group_by(seqId) %>%
    dplyr::mutate(eventId = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(seqId) %>%
    dplyr::slice_max(eventId) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(path, eventId) %>%
    dplyr::summarise(freq = sum(freq), .groups = "drop") %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(id_cols = path, names_from = eventId, values_from = freq, names_prefix = "eventSeq_")
  
  return(result)
  
}


# How many patients treated at each step with monotherapy
#' @export
countMonotherapy <- function(treatmentPathways){
  result <- treatmentPathways %>%
    dplyr::filter(age == "all", sex == "all", indexYear == "all") %>%
    dplyr::filter(path != "None") %>%
    dplyr::filter(freq >= 5) %>%
    dplyr::filter(stringr::str_detect(path, "\\+", negate = TRUE)) %>%
    dplyr::mutate(seqId = dplyr::row_number()) %>%
    tidyr::separate_longer_delim(path, delim = "-") %>%
    dplyr::group_by(seqId) %>%
    dplyr::mutate(eventId = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(seqId) %>%
    dplyr::slice_max(eventId) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(eventId) %>%
    dplyr::summarise(freq = sum(freq)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(path = "Monotherapy") %>%
    tidyr::pivot_wider(id_cols = path, names_from = eventId, values_from = freq, names_prefix = "eventSeq_") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Overall = base::sum(dplyr::c_across(dplyr::where(base::is.numeric)), na.rm = TRUE))
  
  return(result)
}

# Overall for each treatment
#' @export
countTreatmentsOverall <- function(treatmentPathways){
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

# How many patients treated at each step with combinations
#' @export
countCombinationEvents <- function(treatmentPathways){
  result <- treatmentPathways %>%
    dplyr::filter(age == "all", sex == "all", indexYear == "all") %>%
    dplyr::filter(path != "None") %>%
    dplyr::filter(freq >= 5) %>%
    dplyr::filter(stringr::str_detect(path, "\\+")) %>%
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
    dplyr::mutate(path = "Combinations") %>%
    tidyr::pivot_wider(id_cols = path, names_from = eventId, values_from = freq, names_prefix = "eventSeq_") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Overall = base::sum(dplyr::c_across(dplyr::where(base::is.numeric)), na.rm = TRUE))
  
  return(result)
}

# How many patients treated at each step with freq less than 5
#' @export
countLessThanFive <- function(treatmentPathways){
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
    dplyr::rowwise() %>%
    dplyr::mutate(Overall = base::sum(dplyr::c_across(dplyr::where(base::is.numeric)), na.rm = TRUE))
  
  return(result)
}

#' @export
countTPCC <- function(treatmentPathways){
  
  
  armCounts <- countTreatmentEvents(treatmentPathways)
  
  monotherapy <- countMonotherapy(treatmentPathways)
  
  overall <- countTreatmentsOverall(treatmentPathways)
  
  combinations <- countCombinationEvents(treatmentPathways)
  
  lessThanFive <- countLessThanFive(treatmentPathways)
  
  
  result <- dplyr::inner_join(armCounts, overall, by = "path") %>%
    dplyr::bind_rows(monotherapy, combinations) %>%
    dplyr::bind_rows(lessThanFive) %>%
    dplyr::select(path, dplyr::starts_with("eventSeq"), Overall) 
  
  #replace NA
  result[is.na(result)] <- 0
  
  treated <- result %>%
    dplyr::filter(path %in% c("Monotherapy", "Combinations", "Frequency less than five")) %>%
    dplyr::summarise(dplyr::across(dplyr::where(base::is.numeric), ~base::sum(., na.rm = T))) %>%
    dplyr::mutate(path = "Total treated") %>%
    dplyr::select(path, dplyr::everything())
  
  treatedValid <- result %>%
    dplyr::filter(path %in% c("Monotherapy", "Combinations")) %>%
    dplyr::summarise(dplyr::across(dplyr::where(base::is.numeric), ~base::sum(., na.rm = T))) %>%
    dplyr::mutate(path = "Valid treatments") %>%
    dplyr::select(path, dplyr::everything())
  
  result <- result %>%
    dplyr::bind_rows(treatedValid) %>%
    dplyr::bind_rows(treated)
  
  #replace NA
  result[is.na(result)] <- 0
  
  return(result)
}