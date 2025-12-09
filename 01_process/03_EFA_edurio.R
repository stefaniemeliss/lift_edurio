#### SETUPS ####
source(file = list.files(pattern = "setup.R", recursive = T, full.names = T))
library(psych)

# read in data
df <- read.csv(file = file.path(dir_data, "tmp_data_edurio.csv"))

# get master survey key
matched <- read.csv(file.path(dir_misc, "01_check_edurio_survey_items_matched.csv"), fileEncoding = "UTF-8")
survey_key <- read.csv(file.path(dir_misc, "02_process_edurio_survey_key.csv"), fileEncoding = "UTF-8")

# limit to most recent 
key <- survey_key[survey_key$year == max(survey_key$year), ]

# remove all job satisfaction questions from FA
key <- key[! grepl("job", key$standard_name), ]

# re-code items #

# extract binary and ordinary items
i_bin <- key$standard_name[key$scale == "binary"]
i_ord <- key$standard_name[key$scale == "ordinal"]

# determine list of items were "Not applicable" needs to be re-coded
items <- key$standard_name[grepl("Not applicable", key$ops)]
length(items)

# For each item, set *_ans to NA where *_txt is "Not applicable"
for (item in items) {
  ans_col <- paste0(item, "_ans")
  txt_col <- paste0(item, "_txt")
  df[, ans_col][df[, txt_col] == "Not applicable"] <- NA
}

# identify ordinary items to re-code, so that higher values indicate more positive workplace attitudes
items <- paste0(key$standard_name[key$scale == "ordinal" & ! grepl("1", key$ops)], "_ans")
# recode items
df[, items] <- apply(df[, items], 2, function(x){ifelse(is.na(x), NA, 6 - x)})

# identify binary items to re-code so that 1 indicates positive workplace and 0 negative workplace
items <- paste0(key$standard_name[key$scale == "binary"], "_ans")
# recode items
df[, items] <- apply(df[, items], 2, function(x){ifelse(is.na(x), NA, x - 1)})

# define the sets of items
items_all <- key$standard_name
items_2018 <- intersect(items_all, survey_key$standard_name[survey_key$year == 2018])
items <- setdiff(c("q_beh_01", items_2018), "q_sup_02")

# update binary and ordinary items
i_bin <- intersect(items, i_bin)
i_ord <- intersect(items, i_ord)

# identify binary items with more than 90% of responses in one category
i_tmp <- names(which(colSums(df[paste0(i_bin, "_ans")])/nrow(df) > .9))
# remove those items
items <- setdiff(items, gsub("_ans", "", i_tmp))

# examine missing data proportions #

# identify items with more than 10% missing responses
i_tmp <- names(which(colSums(is.na(df[, paste0(items, "_ans")]))/nrow(df) > .1))
# ignore item not included in 2018/19
i_tmp <- setdiff(i_tmp, "q_beh_01_ans")
# remove those items
items <- setdiff(items, gsub("_ans", "", i_tmp))

# remove respondents with more than 25% missing responses
df <- df[rowSums(is.na(df[, paste0(items, "_ans")]))/ncol(df[, paste0(items, "_ans")]) <= 0.25, ]

# run EFA
fa <- fa(df[, paste0(items, "_ans")], cor = "poly", use = "pairwise", correct = 0,
         nfactors = 4, fm = "minres",
         rotate = "oblimin", scores = "regression",
         missing = T, impute = "none")
fa

# rename factors
colnames(fa$scores)[colnames(fa$scores) == "MR1"] <- "f_lead"
colnames(fa$scores)[colnames(fa$scores) == "MR2"] <- "f_beh"
colnames(fa$scores)[colnames(fa$scores) == "MR3"] <- "f_supp"
colnames(fa$scores)[colnames(fa$scores) == "MR4"] <- "f_opp"

# add factor scores to edurio data
df <- merge(df, fa$scores, by = 0)
df$Row.names <- NULL

# save data file
write.csv(df, file = file.path(dir_data, "data_edurio.csv"), row.names = F, fileEncoding = "UTF-8")
