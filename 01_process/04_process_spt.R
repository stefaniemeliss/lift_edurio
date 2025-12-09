#### SETUPS ####

source(file = list.files(pattern = "setup.R", recursive = T, full.names = T))

# load libraries
library(dplyr)
library(data.table)

## VARIABLES ##

# determine years
year_list <- c(201819,
               201920,
               202021,
               202122,
               202223,
               202324)

# determine URNs of 
urn_list <- unique(lift$urn)

#### school level data ####

# read in data
pupils <- fread(file.path(dir_data, "data_pupils.csv"))

# process absence data
pupils <- pupils %>%
  # Lift schools only
  filter(urn %in% urn_list) %>%
  # only years for which we also have edurio data
  filter(time_period %in% year_list) %>%
  # select relevant ID and absence columns
  select(! c(school)) %>%
  # select(! starts_with("board")) %>%
  # select(! starts_with("fte")) %>%
  # select(! starts_with("num_pup_re")) %>%
  # select(! starts_with("num_pup_nu")) %>%
  # select(! starts_with("num_pup_ey")) %>%
  # select(! starts_with("num_pup_k")) %>%
  # select(! starts_with("num_pup_ethn")) %>%
  # select(! starts_with("perc_pup_ethn")) %>%
  # select(! ends_with("_efl")) %>%
  # select(! ends_with("_ufl")) %>%
  # process
  mutate(perc_pup_sen = ifelse(perc_pup_sen > 100, 100, perc_pup_sen)) %>%
  # return as df
  as.data.frame()


#### absence data ####

# read in data
abs <- fread(file.path(dir_data, "data_spt_absences.csv"))

# process absence data
abs <- abs %>%
  # Lift schools only
  filter(urn %in% urn_list) %>%
  # only years for which we also have edurio data
  filter(time_period %in% year_list) %>%
  # select relevant ID and absence columns
  # PERCTOT	- Percentage of overall absence	(authorised and unauthorised)
  # PPERSABS10 - Percentage of enrolments who are persistent absentees (missing 10% or more of possible sessions across the full academic year)
  select(time_period, urn, urn_abs, laestab, perctot, ppersabs10) %>%
  # return as df
  as.data.frame()


#### kS2 data ####

# read in data
ks2 <- fread(file.path(dir_data, "tmp_data_spt_ks2.csv"))

# process absence data
ks2 <- ks2 %>%
  # Lift schools only
  filter(urn %in% urn_list) %>%
  # only years for which we also have edurio data
  filter(time_period %in% year_list) %>%
  # select relevant ID and absence columns
  select(-school) %>%
  select(! ends_with("_hi")) %>%
  select(! ends_with("_mi")) %>%
  select(! ends_with("_lo")) %>%
  select(! ends_with("_na")) %>%
  select(! ends_with("_efl")) %>%
  select(! ends_with("_ufl")) %>%
  select(! starts_with("prog_")) %>%
  select(! starts_with("att_read")) %>%
  select(! starts_with("att_math")) %>%
  select(! starts_with("att_writ")) %>%
  select(! starts_with("att_gps")) %>%
  select(! starts_with("att_scita")) %>%
  # rename columns
  rename(num_ks2_disadv = num_ks2_fsmcla1a) %>%
  rename(perc_ks2_disadv = perc_ks2_fsmcla1a) %>%
  rename(num_ks2_notdisadv = num_ks2_not_fsmcla1a) %>%
  rename(perc_ks2_notdisadv = perc_ks2_not_fsmcla1a) %>%
  # return as df
  as.data.frame()

# Exclude cols that fully consist of NA values
ks2 <- ks2[, apply(ks2, 2, function(col) !all(is.na(col)))]

apply(ks2, 2, function(x){sum(is.na(x))})


#### kS4 data ####

# read in data
ks4 <- fread(file.path(dir_data, "tmp_data_spt_ks4.csv"))

# process absence data
ks4 <- ks4 %>%
  # Lift schools only
  filter(urn %in% urn_list) %>%
  # only years for which we also have edurio data
  filter(time_period %in% year_list) %>%
  # select relevant ID and absence columns
  select(-school) %>%
  select(! ends_with("_efl")) %>%
  select(! ends_with("_ufl")) %>%
  select(! starts_with("totatt")) %>%
  select(! starts_with("em")) %>%
  select(! starts_with("gcse")) %>%
  select(! starts_with("ebacc")) %>%
  select(! starts_with("aps")) %>%
  select(! starts_with("att")) %>%
  # rename columns
  rename(num_ks4_disadv = num_ks4_fsmcla1a) %>%
  rename(perc_ks4_disadv = perc_ks4_fsmcla1a) %>%
  rename(num_ks4_notdisadv = num_ks4_not_fsmcla1a) %>%
  rename(perc_ks4_notdisadv = perc_ks4_not_fsmcla1a) %>%
  # return as df
  as.data.frame()

# Exclude cols that fully consist of NA values
ks4 <- ks4[, apply(ks4, 2, function(col) !all(is.na(col)))]


#### school workforce ####

# read in data
swf <- fread(file.path(dir_data, "data_swf.csv"))

# process absence data
swf <- swf %>%
  # Lift schools only
  filter(urn %in% urn_list) %>%
  # only years for which we also have edurio data
  filter(time_period %in% year_list) %>%
  # select relevant ID and absence columns
  select(time_period, urn, laestab, 
         urn_swf, fte_all_teachers, fte_classroom_teachers, fte_leadership_teachers, fte_teaching_assistants,
         urn_wtc, fte_perc_sex_female, fte_avg_age, fte_perc_ethnicity_white,
         urn_ptr, pupil_to_qual_teacher_ratio, 
         urn_abs, total_teachers_taking_absence, percentage_taking_absence, total_number_of_days_lost, average_number_of_days_taken, average_number_of_days_all_teachers, 
         urn_tto, retention_rate, recruitment_rate) %>%
  rename(urn_abs_swf = urn_abs) %>%
  mutate(percentage_taking_absence = percentage_taking_absence / 100) %>%
  # return as df
  as.data.frame()



#### combine data ####

data <- full_join(lift, swf)
data <- full_join(data, pupils)
data <- full_join(data, abs)
data <- full_join(data, ks2)
data <- full_join(data, ks4)

write.csv(data, file = file.path(dir_data, "data_DfE.csv"), row.names = F)

check <- data %>% group_by(urn, time_period) %>%
  summarise(across(everything(), ~length(unique(.))))
apply(check, 2, max)
apply(data, 2, function(x){sum(is.na(x))})


### combine school level data and edurio data ###

edurio <- read.csv(file.path(dir_data, "data_edurio.csv"))


all <- full_join(data, edurio)
apply(all, 2, function(x){sum(is.na(x))})

all <- arrange(all, urn, time_period)

write.csv(all, file = file.path(dir_data, "data_all.csv"), row.names = F)

