#### SETUPS ####
source(file = list.files(pattern = "setup.R", recursive = T, full.names = T))

lookup <- data.frame(
  school = 
    c("Unity City Academy", "New Rickstones Academy", "Greensward Academy", "Maltings Academy", "Clacton Coastal Academy", "Aylward Academy", "Richmond Park Academy", 
      "Ryde Academy", "Bexleyheath Academy", "Tendring Technology College", "Ashingdon Primary Academy", "Plumberow Primary Academy", "Westerings Primary Academy", 
      "Hamford Primary Academy", "St James the Great Academy", "Columbus School and College", "Pioneer School", "Barton Hill Academy", "Percy Shurmer Academy", 
      "The Ridge Academy", "Tamworth Enterprise College", "Sir Herbert Leon Academy", "Charles Warren Academy", "New Forest Academy", "Noel Park Primary School", 
      "Trinity Primary Academy", "Brockworth Primary Academy", "Newington Primary Academy", "The Green Way Academy", "Hall Road Academy", "The Rawlett School", "Wishmore Cross Academy", 
      "Offa's Mead Academy", "North Ormesby Primary Academy", "Montgomery Primary Academy", "Feversham Primary Academy", "Winton Community Academy", "Cottingley Primary Academy", 
      "Anglesey Primary Academy", "Broadlands Academy", "Lea Forest Primary Academy", "Shafton Primary Academy", "Beacon Academy", "St Helen's Primary Academy", 
      "Four Dwellings Academy", "Greenwood Academy", "Kingswood Academy", "Four Dwellings Primary Academy", "Meadstead Primary Academy", "Hazelwood Academy", "Kingsley Academy", 
      "Firth Park Academy", "Caldicotes Primary Academy", "North Thoresby Primary School", "Utterby Primary Academy", "Newlands Academy", "Hockley Primary School"
  ),
  establishmentname = 
    c("Lift Unity City", "Lift New Rickstones", "Lift Greensward", "Lift Maltings", "Lift Clacton", "Lift Aylward", "Lift Richmond Park", 
      "Lift Ryde", "Lift Bexleyheath", "Lift Tendring", "Lift Ashingdon", "Lift Plumberow", "Lift Westerings", 
      "Lift Hamford", "Lift St James the Great", "Lift Columbus", "Lift Pioneer", "Lift Barton Hill", "Lift Percy Shurmer", 
      "Lift Crescent View", "Lift Tamworth", "Lift Sir Herbert Leon", "Lift Charles Warren", "Lift New Forest", "Lift Noel Park", 
      "Lift Trinity", "Lift Brockworth", "Lift Newington", "Lift Green Way", "Lift Hall Road", "Lift Rawlett", "Lift Wishmore", 
      "Lift Offa's Mead", "Lift North Ormesby", "Lift Montgomery", "Lift Feversham", "Lift Winton", "Lift Cottingley", 
      "Lift Anglesey", "Lift Broadlands", "Lift Lea Forest", "Lift Shafton", "Lift Beacon", "Lift St Helen's", 
      "Lift Four Dwellings Secondary", "Lift Greenwood", "Lift Kingswood", "Lift Four Dwellings Primary", "Lift Meadstead", "Lift Hazelwood", 
      "Lift Kingsley", "Lift Firth Park", "Lift Caldicotes", "Lift North Thoresby", "Lift Utterby", "Lift Newlands", "Lift Hockley")
)

# NO LONGER PART OF LIFT (but have some edurio data)
# "Felixstowe Academy", 
# "Hillsview Academy", 
# "Langer Primary Academy",
# "Nightingale Academy", 
# "Severn View Primary Academy", 

# get master survey key
matched <- read.csv(file.path(dir_misc, "01_check_edurio_survey_items_matched.csv"), fileEncoding = "UTF-8")



# Best Practice for Matching Inconsistent Column Names Across Years

## 1. Create a Column Name Correspondence Table
## 2. Automated Renaming Using dplyr and purrr
## 3. Use bind_rows() for Efficient Combining
## 4. Clean id information

# Edurio data column lookup table: map core question to corresponding question_id
column_lookup <- tibble(
  standard_name = c(
    # survey identifiers
    "year", 
    # "survey", 
    # "file_name", 
    "survey_window", 
    "time_period",
    "academic_year",
    
    # Basic identifiers
    "school",
    # survey response vars
    "survey_answered", # provided by edurio
    "senior_leader",
    "count_ans_SL", "count_ans_all", 
    "count_na_SL", "count_na_all",
    
    # TRUST PERCEPTION #
    # Q_TRUST_01 – Knowledge/good practice sharing across AET (staff_ret_72_aet)
    "q_trust_01_ans", "q_trust_01_txt",
    
    # LEADERSHIP DYNAMICS # 
    # - NOT presented to SeniorLeadership - #
    # Q_LEAD_01 – Comfortable voicing concerns (staff_ret_9)
    "q_lead_01_ans", "q_lead_01_txt",
    # Q_LEAD_02 – Leadership understands challenges (staff_ret_1)
    "q_lead_02_ans", "q_lead_02_txt",
    # Q_LEAD_03 – Leadership addresses needs (staff_ret_2)
    "q_lead_03_ans", "q_lead_03_txt",
    # Q_LEAD_04 – Fair treatment (staff_ret_14)
    "q_lead_04_ans", "q_lead_04_txt",
    # Q_LEAD_05 – Appreciation (staff_ret_14_a)
    "q_lead_05_ans", "q_lead_05_txt",
    
    # COMMUNICATION #
    # Q_COMM_01 – General satisfaction with leadership communication (staff_ret_54)
    "q_comm_01_ans", "q_comm_01_txt",
    # Q_COMM_02 – Consultation before decision-making (staff_ret_11) - NOT presented to SeniorLeadership
    "q_comm_02_ans", "q_comm_02_txt",
    
    # WORKLOAD #
    # Q_WORK_01 – Staying on top of responsibilities (staff_ret_22)
    "q_work_01_ans", "q_work_01_txt",
    
    # PROFESSIONAL SUPPORT #
    # Q_SUP_01 – Support for admin/data (staff_ret_6 / 6_aet_2024)
    "q_sup_01_ans", "q_sup_01_txt",
    # Q_SUP_02 – Support for mental wellbeing (staff_ret_8)
    "q_sup_02_ans", "q_sup_02_txt",
    # Q_SUP_03 – Support with lesson preparation (staff_ret_3) - NOT presented to SeniorLeadership
    "q_sup_03_ans", "q_sup_03_txt",
    # Q_SUP_04 – Support for marking/assessment (staff_ret_4) - NOT presented to SeniorLeadership
    "q_sup_04_ans", "q_sup_04_txt",
    # Q_SUP_05 – Support liaising with families (staff_ret_7) - NOT presented to SeniorLeadership
    "q_sup_05_ans", "q_sup_05_txt",
    
    # WORKING ARRANGEMENTS AND CAREER OPPORTUNITIES #
    # Q_OPP_01 – Flexible working options (staff_ret_74_aet)
    "q_opp_01_ans", "q_opp_01_txt",
    # Q_OPP_02 – PD suits needs (staff_ret_21)
    "q_opp_02_ans", "q_opp_02_txt",
    # Q_OPP_03 – Career progression satisfaction (staff_ret_33)
    "q_opp_03_ans", "q_opp_03_txt",
    # Q_OPP_04 – Discouraged from promotion (staff_ret_79_aet)
    "q_opp_04_ans", "q_opp_04_txt",
    # Q_OPP_05 – Workplace environment/premises (staff_ret_31)
    "q_opp_05_ans", "q_opp_05_txt",
    
    # STUDENT BEHAVIOUR #
    # Q_BEH_01 – Student behaviour rating (staff_ret_61)
    "q_beh_01_ans", "q_beh_01_txt",
    # Q_BEH_02 – Feeling safe re: behaviour (staff_ret_37)
    "q_beh_02_ans", "q_beh_02_txt",
    
    # SAFETY #
    # Q_SAFE_01 – Violence from student (staff_ret_39)
    "q_safe_01_ans", "q_safe_01_txt",
    # Q_SAFE_02 – Violence from staff (staff_ret_40)
    "q_safe_02_ans", "q_safe_02_txt",
    # Q_SAFE_03 – Violence from family member (staff_ret_41)
    "q_safe_03_ans", "q_safe_03_txt",
    
    # GENERAL JOB SATISFACTION #
    # Q_JOB_01 – Recommend AET (staff_ret_81_aet)
    "q_job_01_ans", "q_job_01_txt",
    # Q_JOB_02 – Part of a team (staff_ret_15)
    "q_job_02_ans", "q_job_02_txt",
    # Q_JOB_03 – Considered resigning (staff_ret_43)
    "q_job_03_ans", "q_job_03_txt",
    # Q_JOB_04 – If resigned, remain or leave profession (staff_ret_48)
    "q_job_04_ans", "q_job_04_txt"
    
  ),
  variations = list(
    # survey identifiers
    c("year"),
    # c("survey"),
    # c("file_name"),
    c("survey_window"),
    c("time_period"),
    c("academic_year"),
    
    # Basic identifiers
    c("prop.schoolsurvey"),
    
    # survey response vars
    c("survey_answered"),
    c("senior_leader"),
    c("count_ans_SL"), c("count_ans_all"), 
    c("count_na_SL"), c("count_na_all"), 
    
    # TRUST PERCEPTION #
    c("ans.457338","ans.370085","ans.283791","ans.194907","ans.146286"),               # q_trust_01_ans
    c("txt.457338","txt.370085","txt.283791","txt.194907","txt.146286"),               # q_trust_01_txt
    
    # LEADERSHIP DYNAMICS #
    c("ans.457349","ans.370092","ans.283794","ans.194911","ans.146239","ans.117238"),  # q_lead_01_ans
    c("txt.457349","txt.370092","txt.283794","txt.194911","txt.146239","txt.117238"),  # q_lead_01_txt
    c("ans.457350","ans.370093","ans.283795","ans.194912","ans.146240","ans.117239"),  # q_lead_02_ans
    c("txt.457350","txt.370093","txt.283795","txt.194912","txt.146240","txt.117239"),  # q_lead_02_txt
    c("ans.457351","ans.370094","ans.283796","ans.194913","ans.146241","ans.117240"),  # q_lead_03_ans
    c("txt.457351","txt.370094","txt.283796","txt.194913","txt.146241","txt.117240"),  # q_lead_03_txt
    c("ans.457352","ans.370095","ans.283797","ans.194915","ans.146243","ans.117244"),  # q_lead_04_ans
    c("txt.457352","txt.370095","txt.283797","txt.194915","txt.146243","txt.117244"),  # q_lead_04_txt
    c("ans.457353","ans.370096","ans.283798","ans.194916","ans.146244","ans.117245"),  # q_lead_05_ans
    c("txt.457353","txt.370096","txt.283798","txt.194916","txt.146244","txt.117245"),  # q_lead_05_txt
    
    # COMMUNICATION #
    c("ans.457356","ans.370076","ans.283777","ans.194886","ans.146222"),               # q_comm_01_ans
    c("txt.457356","txt.370076","txt.283777","txt.194886","txt.146222"),               # q_comm_01_txt
    c("ans.457354","ans.370077","ans.283780","ans.194891","ans.146227","ans.117241"),  # q_comm_02_ans
    c("txt.457354","txt.370077","txt.283780","txt.194891","txt.146227","txt.117241"),  # q_comm_02_txt
    
    # WORKLOAD #
    c("ans.457359","ans.370097","ans.283804","ans.194922","ans.146250","ans.117219"),  # q_work_01_ans
    c("txt.457359","txt.370097","txt.283804","txt.194922","txt.146250","txt.117219"),  # q_work_01_txt
    
    # PROFESSIONAL SUPPORT #
    c("ans.457365","ans.370100","ans.283808","ans.194926","ans.146254","ans.117235"),  # q_sup_01_ans
    c("txt.457365","txt.370100","txt.283808","txt.194926","txt.146254","txt.117235"),  # q_sup_01_txt
    c("ans.457367","ans.370102","ans.283810","ans.194928","ans.146256","ans.117237"),  # q_sup_02_ans
    c("txt.457367","txt.370102","txt.283810","txt.194928","txt.146256","txt.117237"),  # q_sup_02_txt
    c("ans.457363","ans.370098","ans.283806","ans.194924","ans.146252","ans.117232"),  # q_sup_03_ans
    c("txt.457363","txt.370098","txt.283806","txt.194924","txt.146252","txt.117232"),  # q_sup_03_txt
    c("ans.457364","ans.370099","ans.283807","ans.194925","ans.146253","ans.117233"),  # q_sup_04_ans
    c("txt.457364","txt.370099","txt.283807","txt.194925","txt.146253","txt.117233"),  # q_sup_04_txt
    c("ans.457366","ans.370101","ans.283809","ans.194927","ans.146255","ans.117236"),  # q_sup_05_ans
    c("txt.457366","txt.370101","txt.283809","txt.194927","txt.146255","txt.117236"),  # q_sup_05_txt
    
    # WORKING ARRANGEMENTS AND CAREER OPPORTUNITIES #
    c("ans.457375","ans.370104","ans.283812","ans.194929","ans.146257"),               # q_opp_01_ans
    c("txt.457375","txt.370104","txt.283812","txt.194929","txt.146257"),               # q_opp_01_txt
    c("ans.457376","ans.370105","ans.283813","ans.194931","ans.146259","ans.117227"),  # q_opp_02_ans
    c("txt.457376","txt.370105","txt.283813","txt.194931","txt.146259","txt.117227"),  # q_opp_02_txt
    c("ans.457377","ans.370106","ans.283815","ans.194933","ans.146261","ans.117229"),  # q_opp_03_ans
    c("txt.457377","txt.370106","txt.283815","txt.194933","txt.146261","txt.117229"),  # q_opp_03_txt
    c("ans.457378","ans.370107","ans.283816","ans.194934","ans.146262"),               # q_opp_04_ans
    c("txt.457378","txt.370107","txt.283816","txt.194934","txt.146262"),               # q_opp_04_txt
    c("ans.457379","ans.370108","ans.283817","ans.194937","ans.146265"),               # q_opp_05_ans
    c("txt.457379","txt.370108","txt.283817","txt.194937","txt.146265"),               # q_opp_05_txt
    
    # STUDENT BEHAVIOUR #
    c("ans.457382","ans.370112","ans.283821","ans.194938","ans.146266"),               # q_beh_01_ans
    c("txt.457382","txt.370112","txt.283821","txt.194938","txt.146266"),               # q_beh_01_txt
    c("ans.457383","ans.370113","ans.283825","ans.194942","ans.146270","ans.117251"),  # q_beh_02_ans
    c("txt.457383","txt.370113","txt.283825","txt.194942","txt.146270","txt.117251"),  # q_beh_02_txt
    
    # SAFETY #
    c("ans.457385","ans.370115","ans.283827","ans.194943","ans.146271","ans.117252"),  # q_safe_01_ans
    c("txt.457385","txt.370115","txt.283827","txt.194943","txt.146271","txt.117252"),  # q_safe_01_txt
    c("ans.457386","ans.370116","ans.283828","ans.194944","ans.146272","ans.117253"),  # q_safe_02_ans
    c("txt.457386","txt.370116","txt.283828","txt.194944","txt.146272","txt.117253"),  # q_safe_02_txt
    c("ans.457387","ans.370117","ans.283829","ans.194945","ans.146273","ans.117254"),  # q_safe_03_ans
    c("txt.457387","txt.370117","txt.283829","txt.194945","txt.146273","txt.117254"),  # q_safe_03_txt
    
    # GENERAL JOB SATISFACTION #
    c("ans.457395","ans.370122","ans.283834","ans.194949","ans.146277","ans.117259"),  # q_job_01_ans
    c("txt.457395","txt.370122","txt.283834","txt.194949","txt.146277","txt.117259"),  # q_job_01_txt
    c("ans.457391","ans.370119","ans.283831","ans.194946","ans.146274","ans.117248"),  # q_job_02_ans
    c("txt.457391","txt.370119","txt.283831","txt.194946","txt.146274","txt.117248"),  # q_job_02_txt
    c("ans.457392","ans.370120","ans.283832","ans.194947","ans.146275","ans.117256"),  # q_job_03_ans
    c("txt.457392","txt.370120","txt.283832","txt.194947","txt.146275","txt.117256"),  # q_job_03_txt
    c("ans.457394","ans.370121","ans.283833","ans.194948","ans.146276"),               # q_job_04_ans
    c("txt.457394","txt.370121","txt.283833","txt.194948","txt.146276")                # q_job_04_txt
    
  )
)


# Run the review of the column name lookup
review_lookup_mappings(lookup_table = column_lookup)
# write.csv(apply(column_lookup, 2, as.character), file = file.path(dir_misc, "meta_edurio.csv"), row.names = F)

# Create the reverse lookup for school edurio data
reverse_lookup <- create_reverse_lookup(column_lookup)

# Initialize empty list to store all processed datasets
df_all <- list()

# LOOP OVER ALL YEARS 
for (i in seq_along(years)) {
  
  year = years[i]
  print(year)
  academic_year <- periods$academic_year[periods$year == year]
  
  # read in data #
  tmp <- xlsx::read.xlsx(file = files[i], sheetIndex = 1, password = pw, encoding = "UTF-8")
  tmp <- subset(tmp, survey_answered != "Survey Answered") # remove second header row
  # tmp <- subset(tmp, survey_answered == "1") # only complete survey responses
  
  # add information on survey to data
  tmp$year <- year
  tmp <- merge(periods, tmp, by = "year", all.y = T)
  
  # **standardise COLUMN NAMES**
  tmp <- standardise_column_names(tmp, lookup = reverse_lookup)
  
  # process survey key #
  
  # get key for this year
  key <- matched[, grepl(paste0(year, "|standard_name"), names(matched))]
  
  # change column names
  names(key) <- gsub(paste0("_", year), "", names(key))
  names(key)[names(key) == "question_text_en"] <- "quest"
  names(key)[names(key) == "answers"] <- "ops"
  
  # add year column
  key$year <- year
  
  # create vector with standard_names
  std_names <- names(tmp)[grepl("_txt", names(tmp))]
  std_names <- gsub("_txt", "", std_names)
  
  # select rows
  key <- key[key$standard_name %in% std_names, ]
  
  # identify SL #
  
  # get all columns containing survey answers, removing items that are rule-dependent
  ans_cols <- paste0(key$standard_name[is.na(key$rules)], "_ans")
  
  # define items not included for senior leaders
  not_senior <- paste0(key$standard_name[!key$SL], "_ans")
  
  # get all columns containing survey answers, excluding items not shown to senior leaders
  ans_cols_senior <- ans_cols[! ans_cols %in% not_senior]
  
  # add column to indicate that ppt is likely SeniorLeadership
  # this would mean that some items were not presented, e.g., q_lead_01-05
  tmp$tmp <- rowSums(is.na(tmp[, not_senior]))
  tmp$senior_leader <- ifelse(tmp$tmp == length(not_senior), TRUE, FALSE)
  tmp$tmp <- NULL

  # count expected number of answers per row
  tmp$count_ans_SL <- ifelse(tmp$senior_leader == T, length(ans_cols_senior), 
                             ifelse(tmp$senior_leader == F, length(ans_cols),
                                    NA))
  tmp$count_ans_all <- length(ans_cols)

  # count NAs per row
  tmp$count_na_SL <- ifelse(tmp$senior_leader == T, rowSums(is.na(tmp[, ans_cols_senior])),
                         ifelse(tmp$senior_leader == F, rowSums(is.na(tmp[, ans_cols])), 
                                NA))
  tmp$count_na_all <- rowSums(is.na(tmp[, ans_cols]))
  
  # combine across years
  if(year == years[1]){
    survey_key <- key
  } else {
    survey_key <- rbind.all.columns(survey_key, key)
  }
  
  # Store processed dataset in list with academic year (encoded as time_period) as name
  df_all[[academic_year]] <- tmp
  
  # Print progress
  cat(paste("Processed", academic_year, "- Rows:", nrow(tmp), "Columns:", ncol(tmp), "\n"))
  
  rm(tmp)
  
}

write.csv(survey_key, file = file.path(dir_misc, paste0(file_stem, "_survey_key.csv")), row.names = F, fileEncoding = "UTF-8")

# survey_key %>% group_by(standard_name) %>% summarise(n = n()) %>% filter(n < 6)

# **BIND ALL DATASETS TOGETHER**
# Get all unique column names across all datasets
cols <- unique(unlist(lapply(df_all, names)))

# Ensure all datasets have the same columns (fill missing with NA)
df_stan <- lapply(df_all, function(df) {
  missing_cols <- setdiff(cols, names(df))
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  return(df[cols])  # Reorder columns consistently
})

# Combine all datasets using bind_rows
edurio <- bind_rows(df_stan, .id = "academic_year")

# check that no column is missing
column_lookup$standard_name[! column_lookup$standard_name %in% names(edurio)]

# re-order columns
edurio <- edurio[, column_lookup$standard_name]

# make all [question_id]_ans columns numeric
edurio[, grepl("_ans", names(edurio))] <- apply(edurio[, grepl("_ans", names(edurio))], 2, as.numeric)

# add establishment name from lookup
edurio <- merge(lookup, edurio, by = "school", all.y = T)

# check the merge of information
check <- unique(edurio[, c("school", "establishmentname")])
check <- check[!is.na(check$establishmentname), ]
check <- merge(lift, check, by = "establishmentname", all.x = T)

# add urn and laestab, only use schools that are still part of Lift (all.x = T)
edurio <- merge(lift, edurio, by = "establishmentname", all.x = T)

# check available data for leaders
edurio %>% group_by(year, senior_leader) %>% summarise(n = n())
edurio %>% filter(survey_answered == 1) %>% group_by(year, senior_leader) %>% summarise(n = n())

# remove all survey responses with missing data at item level
# only select data from participants that do not have any NAs in any of the required items (i.e., count_na_all == 0)
edurio <- edurio[edurio$count_na_all == 0, ]

# check data
check <- edurio %>% 
  group_by(year) %>%
  summarise_all(~sum(!is.na(.)))
t(check)

# sort rows
edurio <- edurio %>% 
  arrange(establishmentname, desc(year)) %>%
  as.data.frame()

# save data file
write.csv(edurio, file = file.path(dir_data, "tmp_data_edurio.csv"), row.names = F, fileEncoding = "UTF-8")
