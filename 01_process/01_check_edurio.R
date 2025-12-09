#### SETUPS ####
source(file = list.files(pattern = "setup.R", recursive = T, full.names = T))

# Edurio data column lookup table: map core question to corresponding question code
column_lookup <- tibble(
  standard_name = c(
    # survey identifiers
    "year", "survey", "file_name", "survey_window", "time_period",
    
    # Basic identifiers
    "surveywered", "school_name",
    
    # ======================
    # TRUST PERCEPTION (Q_TRUST_01–09)
    "q_trust_01",
    "q_trust_02",
    "q_trust_03",
    "q_trust_04",
    "q_trust_05",
    "q_trust_06",
    "q_trust_07",
    "q_trust_08",
    "q_trust_09",
    
    # ======================
    # LEADERSHIP DYNAMICS (Q_LEAD_01–06)
    "q_lead_01",
    "q_lead_02",
    "q_lead_03",
    "q_lead_04",
    "q_lead_05",
    "q_lead_06",
    
    # ======================
    # COMMUNICATION (Q_COMM_01–08)
    "q_comm_01",
    "q_comm_02",
    "q_comm_03",
    "q_comm_04",
    "q_comm_05",
    "q_comm_06",
    
    # ======================
    # WORKLOAD (Q_WORK_01–07)
    "q_work_01",
    "q_work_02",
    "q_work_03",
    "q_work_04",
    "q_work_05",
    "q_work_06",
    "q_work_07",
    
    # ======================
    # PROFESSIONAL SUPPORT (Q_SUP_01–06)
    "q_sup_01",
    "q_sup_02",
    "q_sup_03",
    "q_sup_04",
    "q_sup_05",
    "q_sup_06",
    
    # ======================
    # OPPORTUNITIES & ARRANGEMENTS (Q_OPP_01–07)
    "q_opp_01",
    "q_opp_02",
    "q_opp_03",
    "q_opp_04",
    "q_opp_05",
    "q_opp_06",
    "q_opp_07",
    
    # ======================
    # STUDENT BEHAVIOUR (Q_BEH_01–04)
    "q_beh_01",
    "q_beh_02",
    "q_beh_03",
    "q_beh_04",
    
    # ======================
    # SAFETY (Q_SAFE_01–03)
    "q_safe_01",
    "q_safe_02",
    "q_safe_03",
    
    # ======================
    # GENERAL JOB SATISFACTION (Q_JOB_01–04)
    "q_job_01",
    "q_job_02",
    "q_job_03",
    "q_job_04"
    
  ),
  
  # ======================
  # Variants across years
  variations = list(
    # survey identifiers
    c("year"), c("survey"), c("file_name"), c("survey_window"), c("time_period"),
    c("surveywered"), c("prop.schoolsurvey"),
    
    # TRUST
    c("staff_ret_72_aet"),
    c("staff_ret_49"),
    c("staff_ret_50"),
    c("staff_ret_53"),
    c("staff_ret_62_aet"),
    c("staff_ret_63_aet"),
    c("staff_ret_64_aet"),
    c("staff_ret_65_aet"),
    c("staff_ret_71_aet"),
    
    # LEADERSHIP
    c("staff_ret_9"),
    c("staff_ret_1"),
    c("staff_ret_2"),
    c("staff_ret_14"),
    c("staff_ret_14_a"),
    c("staff_ret_13"),
    
    # COMMUNICATION
    c("staff_ret_54"),
    c("staff_ret_11"),
    c("staff_ret_12"),
    c("staff_ret_55","staff_ret_55_AET"),
    c("staff_ret_56"),
    c("staff_ret_57"),
    
    # WORKLOAD
    c("staff_ret_22"),
    c("staff_ret_24"),
    c("staff_ret_25"),
    c("staff_ret_26"),
    c("staff_ret_27"),
    c("staff_ret_27_a"),
    c("staff_ret_75_aet"),
    
    # PROFESSIONAL SUPPORT
    c("staff_ret_6", "staff_ret_6_aet_2024"),
    c("staff_ret_8"),
    c("staff_ret_3"),
    c("staff_ret_4"),
    c("staff_ret_7"),
    c("staff_ret_5"),
    
    # OPPORTUNITIES & ARRANGEMENTS
    c("staff_ret_74_aet"),
    c("staff_ret_21"),
    c("staff_ret_33"),
    c("staff_ret_79_aet"),
    c("staff_ret_31", "staff_ret_31_aet_2024"),
    c("staff_ret_32"),
    c("edi_11"),
    
    # BEHAVIOUR
    c("staff_ret_61"),
    c("staff_ret_37"),
    c("staff_ret_35"),
    c("staff_ret_36"),
    
    # SAFETY
    c("staff_ret_39"),
    c("staff_ret_40"),
    c("staff_ret_41"),
    
    # JOB SATISFACTION
    c("staff_ret_aet_5", "staff_ret_81_aet"),
    c("staff_ret_15"),
    c("staff_ret_43"),
    c("staff_ret_48")
  )
)

# Run the review of the column name lookup
review_lookup_mappings(lookup_table = column_lookup)

# Create the reverse lookup for school edurio data
reverse_lookup <- create_reverse_lookup(column_lookup)

# make list into named vector 
reverse_lookup <- unlist(reverse_lookup)

# Convert named list to data frame
reverse_lookup <- do.call(rbind, lapply(names(reverse_lookup), function(x) {
  c(code = x, 
    standard_name = reverse_lookup[[x]])
}))

# LOOP OVER ALL YEARS 
for (i in seq_along(years)) {
  
  year = years[i]
  print(year)

  # read in data
  df <- xlsx::read.xlsx(file = files[i], sheetIndex = 1, password = pw, encoding = "UTF-8")
  # subset for complete survey responses
  df <- subset(df, survey_answered != "0")
  
  # determine question_ids
  question_ids <- names(df)
  question_ids <- suppressWarnings(na.omit(as.numeric(unique(gsub("ans.|txt.", "", question_ids)))))
  
  # # open txt file
  # sink(file = file.path(dir_misc, paste0(year, "_unique_values.txt")))
  # # check unique values for each column
  # print(apply(df, 2, unique))
  # # close txt file
  # sink(file = NULL)
  rm(df)
  
  # read in survey key
  key <- xlsx::read.xlsx(file = files[i], sheetIndex = 2, password = pw, encoding = "UTF-8")
  
  # subset to relevant survey question_ids
  key <- key[key$question_id %in% question_ids, ]
  
  # use information encoded in rules column to populate for_groups info for2018 & 2019
  
  if(year %in% c(2018, 2019)){
    
    # copy column content
    key$for_groups <- key$rules
    # set NA for all "pseudo-rules" encoding group allocation
    key$rules <- ifelse(! key$rules == "staff_ret_43:1,staff_ret_43:2,staff_ret_43:3", NA, key$rules)
    # overwrite item response rules with target groups
    key$for_groups <- gsub("staff_ret_43:1,staff_ret_43:2,staff_ret_43:3", NA, key$for_groups)
    key$for_groups <- gsub("staff_ret_d_6_aet:1", "Teacher,TeachingAssistant,MiddleLeadership,OtherLeadership,SeniorLeadership,Maintenance,Administrative,OtherStaff", key$for_groups)
    key$for_groups <- gsub("staff_ret_d_6_aet:2", "SchoolSupportServices", key$for_groups)
    key$for_groups <- gsub("staff_ret_d_1:1", "Teacher", key$for_groups)
    key$for_groups <- gsub("staff_ret_d_1:2", "TeachingAssistant", key$for_groups)
    key$for_groups <- gsub("staff_ret_d_5:2", "MiddleLeadership", key$for_groups)
    key$for_groups <- gsub("staff_ret_d_5:3", "OtherLeadership", key$for_groups)
    key$for_groups[is.na(key$for_groups)] <- "Teacher,TeachingAssistant,MiddleLeadership,OtherLeadership,SeniorLeadership,Maintenance,Administrative,OtherStaff,SchoolSupportServices"
  }

  
  # merge standard names with survey key  
  key <- merge(reverse_lookup, key, by = "code", all.y = T)
  
  # extract answer option text
  key$answers <- apply(key, 1, function(x) {
    n <- as.numeric(x["n_of_answers"])
    if (n != 0) {
      start <- which(names(key) == "ans1_en")
      stop <- which(names(key) == paste0("ans", n, "_en"))
      answers <- paste(x[start:stop], collapse = ",")
    } else {
      answers <- NA
    }
    return(answers)
  })
  
  # add measurement scale
  key$scale <- ifelse(key$n_of_answers == 2, "binary", 
                      ifelse(key$n_of_answers >= 10, "continuous", 
                             "ordinal"))
  
  # check if a group is included in for_group
  key$TeachTA <- grepl("Teacher|TeachingAssistant", key$for_groups)
  key$MLOL <- grepl("MiddleLeadership|OtherLeadership", key$for_groups)
  key$SL <- grepl("SeniorLeadership", key$for_groups)
  key$SSS <- grepl("SSSOutsideSchool|SSSInSchool|SchoolSupportServices", key$for_groups)
  key$Other <- grepl("Maintenance|Administrative|OtherStaff", key$for_groups)
  
  # determine cols to extract
  cols <- c("code", "module_en", "question_id", "question_text_en", "n_of_answers", "scale", "answers", "for_groups", "rules", "TeachTA", "MLOL", "SL", "SSS", "Other")
  
  # extract matched questions - will be merged
  tmp1 <- key[!is.na(key$standard_name), c("standard_name", cols)]
  names(tmp1) <- paste0(names(tmp1), "_", year)
  names(tmp1)[names(tmp1) == paste0("standard_name_", year)] <- "standard_name"

  # extract not matched questions - will be rbound
  tmp2 <- key[is.na(key$standard_name), cols]
  tmp2$year <- year
  
  # add year to key
  key$year <- year
  
  if(year == years[1]){
    matched <- tmp1
    unmatched <- tmp2
    keys <- key
  } else {
    matched <- merge(matched, tmp1, by = "standard_name", all = T)
    unmatched <- rbind.all.columns(unmatched, tmp2)
    keys <- rbind.all.columns(keys, key)
  }
  
  # save file
  # write.csv(key, file = file.path(dir_misc, paste0(year, "_survey_key.csv")), row.names = F)
  
  gc()
}

# export which items were matched onto each other
write.csv(matched, file = file.path(dir_misc, paste0(file_stem, "_survey_items_matched.csv")), row.names = F, fileEncoding = "UTF-8")
write.csv(unmatched, file = file.path(dir_misc, paste0(file_stem, "_survey_items_unmatched.csv")), row.names = F, fileEncoding = "UTF-8")
write.csv(keys, file = file.path(dir_misc, paste0(file_stem, "_survey_keys.csv")), row.names = F, fileEncoding = "UTF-8")


unmatched %>% group_by(code) %>% summarise(n = n()) %>% filter(n > 4)
unmatched %>% group_by(code) %>% summarise(n = n()) %>% arrange(desc(n))
