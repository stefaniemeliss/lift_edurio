options(scipen=999)
#### SETUPS ####
source(file = list.files(path = "..", pattern = "setup.R", recursive = T, full.names = T))
library(psych)
library(kableExtra)
library(lmerTest)

# script level flow vars #
run_da <- FALSE
run_gam <- TRUE

# read in data
data <- read.csv(file = file.path(dir_data, "data_all.csv"))

data <- data %>%
  mutate(
    # for mixed-effects models in R, the grouping variable should be a factor
    establishmentname = as.factor(establishmentname),
    academic_year = as.factor(academic_year),
    gor = as.factor(gor),
    phaseofeducation = as.factor(phaseofeducation),
    # apply min-max scaling to harmonise inconsistent response scales
    q_rec_work = 
      ifelse(year == 2018, (q_job_01_ans - 0) / (10 - 0),
             ifelse(year != 2018, (q_job_01_ans - 1) / (10 - 1),
                    NA)),
    # re-code resign variable, so that smaller numbers means less considerations of resigning
    q_resign_freq = 6 - q_job_03_ans,
    # re-code variable on resign considerations to retention at school level
    q_ret_school = ifelse(q_resign_freq < 3, 1, # never or rarely
                          ifelse(q_resign_freq >= 3, 0, NA)), # sometimes, often or constantly
    # re-code school-level leavers to retention at sector level
    q_ret_sect = ifelse(q_job_04_txt == "Remain in the profession", 1,
                        ifelse(q_job_04_txt == "Leave the profession", 0, NA))
  )

# variables and explanation #

grp = "establishmentname" # factor var
yr = "academic_year" # factor var
# establishmentname: The name of the school or educational institution.
# academic_year: The academic year the data relates to (for example, 2023/24).

controls_cat = c("phaseofeducation", "gor") # factor vars
# phaseofeducation: The phase or stage of education, indicating whether the establishment is a primary school, a secondary school, or does not fit into either category.
# gor: Government Office Region – a geographical region in England used for administrative purposes.

controls_cont = c("num_pup_tot", "perc_pup_girls", "perc_pup_fsm6", "perc_pup_eal", "perc_pup_sen")
# num_pup_tot: Number of pupils total – the total number of pupils enrolled at the establishment.
# perc_pup_girls: Percentage of pupils who are girls.
# perc_pup_fsm6: Percentage of pupils eligible for free school meals at any time in the last six years (FSM6).
# perc_pup_eal: Percentage of pupils with English as an additional language (EAL).
# perc_pup_sen: Percentage of pupils with special educational needs (SEN).

factors <- c("f_lead", "f_beh", "f_supp", "f_opp")
# f_lead: Factor score relating to leadership based on Edurio survey data.
# f_beh: Factor score relating to pupil behaviour based on Edurio survey data.
# f_supp: Factor score relating to collegial support based on Edurio survey data.
# f_opp: Factor score relating to professional opportunities based on Edurio survey data.

scores <- c("q_rec_work", "q_resign_freq", "q_ret_school", "q_ret_sect")
bin <- c("q_ret_school", "q_ret_sect")
# q_rec_work: Whether staff would recommend working at the school.

# q_resign_freq: How frequently in the last three months staff resign from the school.

# q_ret_school: A binary variable indicating retention at the school level. 
# Staff who report that they ‘never’ or ‘rarely’ consider resigning (q_resign_freq < 3) are coded as retained at the school (1). 
# Staff who consider resigning ‘sometimes’, ‘often’, or ‘constantly’ (q_resign_freq ≥ 3) are coded as not retained at the school (0).

# q_ret_sect: A binary variable indicating retention in the education sector. 
# This question was asked only of staff who reported that they considered resigning ‘sometimes’, ‘often’, or ‘constantly’ (q_resign_freq ≥ 3). 
# Staff who intend to ‘Remain in the profession’ (q_job_04_txt) are coded as retained in the sector (1). 
# Staff who intend to ‘Leave the profession’ are coded as not retained in the sector (0). 

# compute LOO for edurio factors #

# copy data 
data_loo <- data  

# compute leave one out mean
for (factor in factors) {
  data_loo <- data_loo %>%
    group_by(!!sym(yr), !!sym(grp)) %>%
    mutate(
      !!sym(factor) := ifelse(
        # check if there is more than one non-missing value in the group
        sum(!is.na(.data[[factor]])) > 1, 
        # LEAVE ONE OUT
        # compute sum of all values without the current row value
        # and divide that by the number of observations for this variable after excl NAs and subtracting 1 for current row value
        (sum(.data[[factor]], na.rm = TRUE) - .data[[factor]]) / (sum(!is.na(.data[[factor]])) - 1),
        NA_real_
      )
    ) %>%
    ungroup()
}


# correlation matrices #

# Calculate correlation matrix for raw data
result_raw <- cluster_corr_matrix(data[, c(grp, yr, scores, factors, controls_cont)], cluster_vars = c(grp, yr), standardise = TRUE)

# Calculate correlation matrix for loo data
result_loo <- cluster_corr_matrix(data_loo[, c(grp, yr, scores, factors, controls_cont)], cluster_vars = c(grp, yr), standardise = TRUE)

# round results
raw <- round(result_raw$correlation_filtered, 2)
loo <- round(result_loo$correlation_filtered, 2)

# export
write.csv(raw, file = file.path(file_dir, paste0(file_stem, "_cor_raw.csv")), na = "")
write.csv(loo, file = file.path(file_dir, paste0(file_stem, "_cor_loo.csv")), na = "")

# Create an empty data frames to store results #
out_icc <- data.frame(
  outcome = character(),
  ICC_school_only = numeric(),
  ICC_year_only = numeric(),
  ICC_school_both = numeric(),
  ICC_year_both = numeric(),
  stringsAsFactors = FALSE
)

out_lm <- data.frame(
  outcome = character(),
  predictor = character(),
  category = character(),
  model = character(),
  # rec = character(),
  clustering = character(),
  Beta_unstd = numeric(),
  Beta_std = numeric(),
  pcor_unstd = numeric(),
  pcor_unstd_CR = numeric(),
  pcor_std = numeric(),
  pcor_std_CR = numeric(),
  pcor_resid = numeric(),
  Log_odds_unstd = numeric(),
  Log_odds_std = numeric(),
  OR_unstd = numeric(),
  OR_std = numeric(),
  SE_unstd = numeric(),
  SE_unstd_CR = numeric(),
  SE_std = numeric(),
  SE_std_CR = numeric(),
  t_unstd = numeric(),
  t_unstd_CR = numeric(),
  t_std = numeric(),
  t_std_CR = numeric(),
  z_unstd = numeric(),
  z_unstd_CR = numeric(),
  z_std = numeric(),
  z_std_CR = numeric(),
  p_unstd = numeric(),
  p_unstd_CR = numeric(),
  p_std = numeric(),
  p_std_CR = numeric(),
  df_unstd = numeric(),
  df_std = numeric(),
  n_obs = numeric(),
  n_clust   = numeric(),
  model_fit = character(),
  param_fit = character(),
  value_fit = numeric(),
  p_fit = numeric(),
  df_fit = character(),
  measure_var = character(),
  value_var = numeric(),
  formula_unstd = character(),
  formula_std = character(),
  stringsAsFactors = FALSE
)
cols <- names(out_lm)

if (run_gam) {
  out_boot <- data.frame(
    predictor = character(),
    outcome   = character(),
    category = character(),
    model = character(),
    family    = character(),   
    link      = character(),
    clustering = character(),
    smooth    = character(),
    edf       = numeric(),
    Ref.df    = numeric(),
    F_value   = numeric(),
    p_value   = numeric(),
    mean_ME   = numeric(),
    sd_ME     = numeric(),
    min_ME    = numeric(),
    max_ME    = numeric(),
    rank_used = numeric(),
    rank_total = numeric(),
    adj_r2    = numeric(),
    dev_explained = numeric(),
    scale_est = numeric(),
    n_obs     = numeric(),
    n_clust   = numeric(),
    
    edf_boot  = numeric(),
    Ref.df_boot    = numeric(),
    F_value_boot   = numeric(),
    p_value_boot   = numeric(),
    mean_ME_boot   = numeric(),
    sd_ME_boot     = numeric(),
    min_ME_boot    = numeric(),
    max_ME_boot    = numeric(),
    rank_used_boot = numeric(),
    rank_total_boot = numeric(),
    adj_r2_boot    = numeric(),
    dev_explained_boot = numeric(),
    scale_est_boot = numeric(),
    n_obs_boot     = numeric(),
    n_clust_boot   = numeric(),
    n_boot    = numeric(),
    n_success = numeric(),
    n_cores   = numeric(),
    AME       = numeric(),
    SE        = numeric(),
    ci_l      = numeric(),
    ci_u      = numeric(),
    z         = numeric(),
    p         = numeric(),
    duration  = difftime(numeric(0), numeric(0)),
    formula   = character(),
    stringsAsFactors = F
  )
}

if (run_da) {
  out_da <- data.frame(
    data = character(),
    outcome = character(),
    predictor = character(),
    contr_abs = numeric(),
    contr_rel = numeric(),
    row.names = NULL)
  
  out_mat <- data.frame(
    data = character(),
    outcome = character(),
    dominance = character(),
    predictor = character(),
    row.names = NULL)
  
  preds_da <- c(grp, yr, controls_cont, controls_cat, factors)
  
  for (i in 1:length(preds_da)) {
    out_mat[, preds_da[i]] <- numeric()
  }
  
}

clust_name <- c("school RE + year RE", "school RE + year FE", "school FE + year FE")


for (outcome in scores) {
  
  # DEBUG
  # outcome <- "q_rec_work"
  # outcome <- "q_ret_school"
  # p = 1
  preds <- factors
  
  # Combine all variables used in any model
  vars_needed <- c(grp, yr, controls_cat, controls_cont, preds, outcome)
  
  # Remove rows with missing values in any of these variables
  df <- data[complete.cases(data[ , vars_needed]), vars_needed]
  
  # Add clusters
  df$clusters <- interaction(df[[grp]], df[[yr]], drop = TRUE)
  
  # extract ICC for factor scores #
  if(outcome == scores[1]){
    for (f in 1:length(factors)) {
      tmp <- icc_school_year(df, outcome = factors[f], school_var = grp, year_var = yr)
      # combine results
      out_icc <- rbind.all.columns(out_icc, tmp)
    }
  }
  
  # extract ICC for outcomes #
  if (! outcome %in% bin){
    tmp <- icc_school_year(df, outcome = outcome, school_var = grp, year_var = yr)
  } else {
    tmp <- icc_school_year(df, outcome = outcome, school_var = grp, year_var = yr, binary_outcome = T)
  }
  # combine results
  out_icc <- rbind.all.columns(out_icc, tmp)
  
  
  #### 'working environment' variables "entered individually" ####
  
  for (p in 1:length(preds)) {
    
    if (! outcome %in% bin) {
      tmp <- get_betas_cluster(df = df, outcome = outcome, predictors = c(preds[p]),
                               fe_vars = NULL, cluster_vars = c(grp, yr), binary_outcome = FALSE)
      tmp$model <- "lm() with vars entered individually (no controls)"
    } else {
      tmp <- get_betas_cluster(df = df, outcome = outcome, predictors = c(controls_cont, preds[p]),
                               fe_vars = NULL, cluster_vars = c(grp, yr), binary_outcome = TRUE)
      tmp$model <- "glm() with vars entered individually (no controls)"
    }
    
    # combine results
    tmp$clustering <- clust_name[3]
    tmp$n_obs <- nrow(df)
    out_lm <- rbind.all.columns(out_lm, tmp)
    
  }
  
  #### control variables + 'working environment' variables "entered individually" ####
  
  for (p in 1:length(preds)) {
    
    if (! outcome %in% bin) {
      tmp <- get_betas_cluster(df = df, outcome = outcome, predictors = c(controls_cont, preds[p]),
                               fe_vars = controls_cat, cluster_vars = c(grp, yr), binary_outcome = FALSE)
      tmp$model <- "lm() with vars entered individually"
    } else {
      tmp <- get_betas_cluster(df = df, outcome = outcome, predictors = c(controls_cont, preds[p]),
                               fe_vars = controls_cat, cluster_vars = c(grp, yr), binary_outcome = TRUE)
      tmp$model <- "glm() with vars entered individually"
    }
    
    # combine results
    tmp$clustering <- clust_name[3]
    tmp$n_obs <- nrow(df)
    out_lm <- rbind.all.columns(out_lm, tmp)
    
  }
  
  #### control variables + 'working environment' variables "entered together" ####
  
  if (! outcome %in% bin) {
    tmp <- get_betas_cluster(df = df, outcome = outcome, predictors = c(controls_cont, preds),
                             fe_vars = controls_cat, cluster_vars = c(grp, yr), binary_outcome = FALSE)
    tmp$model <- "lm() with vars entered together + controls"
  } else {
    tmp <- get_betas_cluster(df = df, outcome = outcome, predictors = c(controls_cont, preds),
                             fe_vars = controls_cat, cluster_vars = c(grp, yr), binary_outcome = TRUE)
    tmp$model <- "glm() with vars entered together + controls"
  }
  
  # combine results
  tmp$clustering <- clust_name[3]
  tmp$n_obs <- nrow(df)
  out_lm <- rbind.all.columns(out_lm, tmp)
  
  #### control variables + 'working environment' variables "entered together" + school FE + year FE ####
  
  if (! outcome %in% bin) {
    
    # school fixed effects, year fixed effect: ALL PREDICTORS
    tmp <- get_betas_cluster(df = df, outcome = outcome,  predictors = c(controls_cont, preds),
                             fe_vars = c(controls_cat, grp, yr), cluster_vars = c(grp, yr),
                             binary_outcome = F)
    tmp$model <- "lm() with vars entered together + controls + school + year effect"
    # tmp$rec <- rec
    tmp$clustering <- clust_name[3]
    tmp$n_obs <- nrow(df)
    out_lm <- rbind.all.columns(out_lm, tmp)
    
  } else {
    
    # school fixed effects, year fixed effect: ALL PREDICTORS
    tmp <- get_betas_cluster(df = df, outcome = outcome,  predictors = c(controls_cont, preds),
                             fe_vars = c(controls_cat, grp, yr), cluster_vars = c(grp, yr),
                             binary_outcome = T)
    tmp$model <- "glm() with vars entered together + controls + school + year effect"
    # tmp$rec <- rec
    tmp$clustering <- clust_name[3]
    tmp$n_obs <- nrow(df)
    out_lm <- rbind.all.columns(out_lm, tmp)
    
  }
  
  #### Leave-one-out prediction (control variables + 'working environment' variables "entered together" + school FE + year FE) ####
  
  # Remove rows with missing values in any of these variables
  df_loo <- data_loo[complete.cases(data_loo[ , vars_needed]), vars_needed]
  
  # Add clusters
  df_loo$clusters <- interaction(df_loo[[grp]], df_loo[[yr]], drop = TRUE)
  
  if (! outcome %in% bin) {
    
    # school fixed effects, year fixed effect: ALL PREDICTORS
    tmp <- get_betas_cluster(df = df_loo, outcome = outcome,  predictors = c(controls_cont, preds),
                             fe_vars = c(controls_cat, grp, yr), cluster_vars = c(grp, yr),
                             binary_outcome = F)
    tmp$model <- "LOO lm() with vars entered together + controls + school + year effect"
    
  } else {
    
    # school fixed effects, year fixed effect: ALL PREDICTORS
    tmp <- get_betas_cluster(df = df_loo, outcome = outcome,  predictors = c(controls_cont, preds),
                             fe_vars = c(controls_cat, grp, yr), cluster_vars = c(grp, yr),
                             binary_outcome = T)
    tmp$model <- "LOO glm() with vars entered together + controls + school + year effect"
    
  }
  
  tmp$clustering <- clust_name[3]
  tmp$n_obs <- nrow(df_loo)
  out_lm <- rbind.all.columns(out_lm, tmp)
  
  #### Dominance analysis (control variables + 'working environment' variables "entered together" + school FE + year FE) ####
  
  if (run_da) {
    if (! outcome %in% bin) {
      
      tmp <- run_dominance_analysis(df = df, df_loo = df_loo, 
                                    outcome = outcome,
                                    predictors = preds_da,
                                    matrices = c("complete", "conditional", "general"),
                                    binary_outcome = F)
      
    } else {
      
      tmp <- run_dominance_analysis(df = df, df_loo = df_loo, 
                                    outcome = outcome,
                                    predictors = preds_da,
                                    matrices = c("complete", "conditional", "general"),
                                    binary_outcome = T)
      
    }
    
    out_da <- rbind.all.columns(out_da, tmp$average_contribution)
    out_mat <- rbind.all.columns(out_mat, tmp$dominance_matrix)
  }
  
  #### Non-parametric regression (control variables + 'working environment' variables "entered together" + school FE + year FE) ####
  
  if (run_gam) {
    
    if (! outcome %in% bin) {
      
      # school fixed effects, year fixed effect: ALL PREDICTORS
      tmp <- get_gam_betas_cluster(df = df_loo, outcome = outcome, predictors = c(controls_cont, preds),
                                   fe_vars = c(controls_cat, grp, yr), cluster_vars = c(grp, yr),
                                   binary_outcome = F, n_cores = 15, n_boot = 1000)
      tmp$model <- "LOO gam(family = gaussian) with vars entered together + controls + school + year effect"
      
    } else {
      
      # school fixed effects, year fixed effect: ALL PREDICTORS
      tmp <- get_gam_betas_cluster(df = df_loo, outcome = outcome, predictors = c(controls_cont, preds),
                                   fe_vars = c(controls_cat, grp, yr), cluster_vars = c(grp, yr),
                                   binary_outcome = T, n_cores = 15, n_boot = 1000)
      tmp$model <- "LOO gam(family = binomial) with vars entered together + controls + school + year effect"
      
    }
    
    tmp$clustering <- clust_name[3]
    out_boot <- rbind.all.columns(out_boot, tmp)
    
  }
  
}

## format lm output ##

# add idx for ordering
out_lm$idx <- as.numeric(row.names(out_lm))

# create lookup to label predictors
lookup <- data.frame(
  predictor = unique(out_lm$predictor))
lookup$category <- ifelse(lookup$predictor %in% factors, "factor",
                          ifelse(lookup$predictor %in% controls_cont, "controls_cont",
                                 ifelse(lookup$predictor %in% controls_cat, "controls_cat", NA)))

# remove category (NAs)
out_lm$category <- NULL

# combine output with look
out_lm <- merge(lookup, out_lm)

# order rows, arrange columns and remove idx
out_lm <- out_lm[order(out_lm$idx), cols]

write.csv(out_lm, file = file.path(file_dir, paste0(file_stem, "_lm.csv")), row.names = F)
write.csv(out_icc, file = file.path(file_dir, paste0(file_stem, "_icc.csv")), row.names = F)
if (run_da) {
  write.csv(out_da, file = file.path(file_dir, paste0(file_stem, "_da_contribution.csv")), row.names = F)
  write.csv(out_mat, file = file.path(file_dir, paste0(file_stem, "_da_dominancematrix.csv")), row.names = F)
} else {
  out_da <- read.csv(file = file.path(file_dir, paste0(file_stem, "_da_contribution.csv")))
  out_mat <- read.csv(file = file.path(file_dir, paste0(file_stem, "_da_dominancematrix.csv")))
}

## format gam output ##
if (run_gam) {
  
  # add idx for ordering
  out_boot$idx <- as.numeric(row.names(out_boot))
  
  # create lookup to label predictors
  lookup <- data.frame(
    predictor = unique(out_boot$predictor))
  lookup$category <- ifelse(lookup$predictor %in% factors, "factor",
                            ifelse(lookup$predictor %in% controls_cont, "controls_cont",
                                   ifelse(lookup$predictor %in% controls_cat, "controls_cat", NA)))
  
  # remove category (NAs)
  out_boot$category <- NULL
  
  # combine output with look
  out_boot <- merge(lookup, out_boot)
  
  # order rows
  out_boot <- out_boot[order(out_boot$idx), ]
  out_boot$idx <- NULL
  
  write.csv(out_boot, file = file.path(file_dir, paste0(file_stem, "_gam.csv")), row.names = F)
} else {
  out_boot <- read.csv(file = file.path(file_dir, paste0(file_stem, "_gam.csv")), row.names = F)
}
#### create output summary table ####

p_to_stars <- function(p) {
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01, "**",
                ifelse(p < 0.05, "*", "")
         )
  )
}

# define model names
cols <- c("Variables entered individually", 
          "Control variables added", 
          "Variables entered together", 
          "School and year effects added", 
          "Common method corrected", 
          "Generalised additive model")
cols <- stringr::str_wrap(cols, width = 8)
cols <- paste0("M", 1:length(cols), "\n", cols)

for (s in 1:length(scores)) {
  
  # subset out_lm table for this outcome and only for the factor score predictors
  out <- out_lm[out_lm$outcome == scores[s] & out_lm$category == "factor", ]
  
  # create empty tables
  table_lm <- data.frame(
    outcome = rep(scores[s], length(unique(out$predictor))),
    predictor = c(unique(out$predictor))
  )
  
  tmp <- data.frame(
    outcome = scores[s],
    predictor = "Variance explained"
  )
  
  # identify models that were used for this outcome
  models <- unique(out$model)
  
  for (m in 1:length(models)) {
    model = models[m]
    
    # extract values to round and format with three digits
    if (scores[s] %in% bin) {
      coef <- out$OR_std[out$model == model]
      coef <- sprintf("%.3f", round(coef, 3))
    } else {
      coef <- out$Beta_std[out$model == model]
      coef <- sprintf("%.3f", round(coef, 3))
    }
    
    se_std_cr <- out$SE_std_CR[out$model == model]
    se_std_cr <- sprintf("%.3f", round(se_std_cr, 3))
    
    p_std_cr <- out$p_std_CR[out$model == model]
    p_std_cr <- p_to_stars(p_std_cr)
    
    # extract variance explained
    var_exp <- unique(out$value_var[out$model == model])
    if(m %in% c(1, 2)) var_exp <- c(min(var_exp), max(var_exp))
    var_exp <- sprintf("%.2f", round(var_exp * 100, 2)) # make percentage
    var_exp <- paste0(var_exp, "%") # add %
    if(m %in% c(1, 2)) var_exp <- paste0(var_exp[1], " - ", var_exp[2])
    
    # paste into tables
    table_lm[, cols[m]] <- paste0(coef, p_std_cr, "\n(", se_std_cr, ")")
    tmp[, cols[m]] <- var_exp
    
  }
  
  # combine tables
  table_lm <- rbind(table_lm, tmp)
  
  # subset out_boot table for this outcome and only for the factor score predictors
  boot <- out_boot[out_boot$outcome == scores[s] & out_boot$category == "factor", ]
  
  # create empty table
  table_gam <- data.frame(
    outcome = rep(scores[s], length(unique(boot$predictor))),
    predictor = c(unique(boot$predictor))
  )
  
  # extract values to round and format with three digits
  coef <- boot$mean_ME_boot
  coef <- sprintf("%.3f", round(coef, 3))
  
  se_std_cr <- boot$SE
  se_std_cr <- sprintf("%.3f", round(se_std_cr, 3))
  
  p_std_cr <- boot$p
  p_std_cr <- p_to_stars(p_std_cr)
  
  var_exp <- unique(boot$adj_r2)
  var_exp <- sprintf("%.2f", round(var_exp * 100, 2)) # make percentage
  var_exp <- paste0(var_exp, "%") # add %
  
  # GAM data
  table_gam[, cols[m+1]] <- paste0(coef, p_std_cr, "\n(", se_std_cr, ")")
  tmp <- data.frame(
    outcome = scores[s],
    predictor = "Variance explained")
  tmp[, cols[m+1]] <- var_exp
  
  # combine tables
  table_gam <- rbind(table_gam, tmp)
  
  # combine lm and gam
  table <- merge(table_lm, table_gam)
  
  if (s == 1) output <- table else output <- rbind(output, table)
  
}

## add dominance analysis to table ##

# filter for factors
tmp <- out_da[out_da$predictor %in% factors, ]

tmp <- out_da %>%
  filter(predictor %in% factors) %>%
  group_by(outcome, data) %>%
  summarise(contr_abs  = sum(contr_abs),
            contr_rel  = sum(contr_rel),
            .groups = "drop") %>%
  mutate(predictor = "Variance explained") %>%
  bind_rows(., tmp) %>%
  arrange(outcome, data, predictor)

# extract values to round and format with one digit
abs <- tmp$contr_abs * 100
abs <- sprintf("%.2f%%", round(abs, 2))

rel <- tmp$contr_rel * 100
rel <- sprintf("%.2f%%", round(rel, 2))

# paste into tables
tmp$contr <- paste0(abs, " (", rel, ")")

# drop cols
tmp <- tmp[, c("data", "outcome", "predictor", "contr")]

# get wide format
tmp <- tmp %>%
  tidyr::pivot_wider(
    names_from = data, 
    values_from = contr) %>%
  rename(`Dominance\nanalysis\nM4` = raw) %>%
  rename(`Dominance\nanalysis\nM5` = loo)

# add labels - predictors #

# define labels and order
pred_label <- c("School", "Year", 
                "Count pupils", "% of girls", "% FSM6 elegible", "% with EAL", "% with SEN",
                "Phase", "Region",
                "Leadership", "Pupil behaviour", "Collegial support", "Professional opportunities", "Variance explained")
pred_label <- stringr::str_wrap(pred_label, width = 14)

pred_label_order <- c("Leadership", "Pupil behaviour", "Collegial support", "Professional opportunities",
                      "School", "Year",
                      "Count pupils", "% of girls", "% FSM6 elegible", "% with EAL", "% with SEN",
                      "Phase", "Region", "Variance explained")
pred_label_order <- stringr::str_wrap(pred_label_order, width = 14)

# create lookup
lookup_pred <- data.frame(
  predictor = factor(c(unique(out_da$predictor), "Variance explained"), levels = c(grp, yr, controls_cont, controls_cat, factors, "Variance explained")),
  pred_label = factor(pred_label, levels = pred_label_order)
)

# add labels - outcomes #

# define labels and add line break
out_label <- c("Workplace recommendation", "Frequency considering resignation", "Retention (school-level)", "Retention (sector-level)")
out_label <- stringr::str_wrap(out_label, width = 14)

# create lookup
lookup_out <- data.frame(
  outcome = unique(out_da$outcome),
  out_label = factor(out_label, levels = out_label)
)

# merge lm, gam, da and labels #
output <- output %>%
  left_join(., tmp) %>%
  left_join(., lookup_out) %>%
  left_join(., lookup_pred) %>%
  select(-c(outcome, predictor)) %>%
  rename(Outcome = out_label, 
         Predictor = pred_label) %>%
  relocate(`Dominance\nanalysis\nM4`, .after = cols[4]) %>%
  relocate(`Dominance\nanalysis\nM5`, .after = cols[5]) %>%
  relocate(Outcome, Predictor, .before = cols[1]) %>%
  arrange(Outcome, Predictor)

write.csv(output, file = file.path(file_dir, paste0(file_stem, "_out.csv")), row.names = F)

## format output for paper ##

# Create an empty row
empty_row <- output[1, ]
empty_row[] <- NA

# Determine where to insert empty rows:
n <- nrow(output)
x <- length(unique(output$Predictor))  # Repeat every 3 rows

# Calculate positions: start (0), then every x rows
insert_positions <- seq(0, n-1, by = x) # not at end

# Build the new data frame:
formatted <- data.frame()  # Start with an empty tibble

for (i in 1:length(insert_positions)) {
  
  # Rows to take from original data
  start <- insert_positions[i] + 1
  end <- min(insert_positions[i] + x, n)
  
  # populate one column in empty_row
  empty_row$Predictor <- unique(output$Outcome)[i]
  
  # Add empty row
  formatted <- bind_rows(formatted, empty_row)
  
  # Add next block of rows
  if (start <= n) {
    formatted <- bind_rows(formatted, output[start:end, ])
  }
}

print(formatted)

# drop Outcome column (redundant now)
formatted$Outcome <- NULL

# # change column names
# names(formatted)[-1] <- c("M1", "M2", "M3", "M4", "DA M4", "M5", "DA M5", "M6")

formatted[-1] <- apply(formatted[-1], 2, gsub, pattern = "\n", replacement = " ")
# formatted[-1] <- apply(formatted[-1], 2, stringr::str_wrap, width = 8)
write.csv(formatted, file = file.path(file_dir, paste0(file_stem, "_out_formatted.csv")), row.names = F)


#### Plot dominance analysis ####

# copy data
out_da2 <- out_da

# add labels for plots - predictors #

# define different order here
pred_label_order <- c("% with SEN", "% with EAL", "% FSM6 elegible", "% of girls", "Count pupils", 
                      "Phase", "Region",
                      "Year", "School",
                      "Professional opportunities", "Collegial support", "Pupil behaviour", "Leadership")
pred_label_order <- stringr::str_wrap(pred_label_order, width = 14)

lookup_pred$pred_label <- factor(lookup_pred$pred_label, levels = pred_label_order)

# add to data 
out_da2 <- merge(out_da2, lookup_pred)

# add labels for plots - outcomes #
out_da2 <- merge(out_da2, lookup_out)

# Relative contribution - stacked barplot #

# define colour palette incl. ITT green
palette <- c(navy, navy40, "#C1D10F", #navy20, 
             black, white, 
             blue, red, 
             yellow, orange, 
             purple, teal, coral, cyan)

# add coordinates to data
out_da_rel <- out_da2 %>%
  group_by(data, outcome) %>%
  arrange(data, outcome, desc(pred_label)) %>%
  mutate(
    pos_y_top = cumsum(contr_rel),                  # Top of each segment
    pos_y_mid = pos_y_top - (contr_rel / 2),        # Middle of each segment
    pos_x = ifelse(data == "raw", 1, 3),            # Middle of each segment
    pos_x_lab = ifelse(data == "raw", -0.5, 4.5),   # Position label
    pos_x_seg = ifelse(data == "raw", 0, 4)) %>%    # End of segment
  ungroup()

# add filter to only show labels if data refers to a factor OR contribution is >= 5% in at least one dataset
out_da_rel <- out_da_rel %>%
  group_by(predictor, outcome) %>%
  mutate(label_show = any(contr_rel >= 0.05)) %>%
  mutate(label_show = ifelse(predictor %in% factors, TRUE, label_show)) %>%
  ungroup()

# create data for the label containing the overall R2
tmp <- out_lm[grepl("school + year effect", out_lm$model, fixed = T), c("outcome", "model", "value_var")]
tmp <- tmp[!duplicated(tmp), ]
tmp <- merge(lookup, tmp)
tmp$pos_x <- ifelse(grepl("LOO", tmp$model), 3, 1)
tmp$pos_y <- 1

# generate plot
p <- ggplot(data = out_da_rel, aes(x = pos_x, y = contr_rel, fill = pred_label)) +
  # set up stacked bargraph
  geom_bar(stat = "identity", color = black) + 
  facet_grid(. ~ out_label) +
  ambition_theme + 
  theme(legend.position = "right") +
  scale_fill_manual(values = palette) +
  guides(fill = guide_legend(title = "Predictor", reverse = TRUE)) +
  scale_y_continuous(breaks = seq(0, 1, .1), labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = c(1, 3), labels = c("Raw", "LOO")) +
  labs(x = "Dataset", y = "Relative average contribution") +
  # add R2 as label
  geom_label(data = tmp, 
             aes(x = pos_x, y = pos_y, label = scales::percent(value_var, accuracy = 1)),
             size = 4, color = black, vjust = -0.5, inherit.aes = FALSE) +
  coord_cartesian(ylim = c(-0, 1.1)) +
  # add percentages as text
  geom_text(data = out_da_rel %>% filter(label_show), 
            aes(x = pos_x_lab, y = pos_y_mid, label = scales::percent(contr_rel, accuracy = 1)),
            size = 4, color = black) +
  coord_cartesian(xlim = c(-1, 5), ylim = c(-0, 1.1)) +
  # draw connecting lines between bar graph and text label
  geom_segment(data = out_da_rel %>% filter(label_show),
               aes(x = pos_x, xend = pos_x_seg, y = pos_y_mid, yend = pos_y_mid),
               colour = black
  )
p

# save plot
ggsave(plot = p,
       filename = paste0(file_stem, "_da_rel.jpg"),
       path = file_dir,
       units = "cm",
       width = 28, height = 15)

# Absolute contribution - dodged barplot #

# extract data of total variance explained
tmp <- out_da2 %>%
  group_by(outcome, data, out_label) %>%
  summarise(contr_abs = sum(contr_abs),
            contr_rel = sum(contr_rel), .groups = "drop") %>%
  ungroup() %>%
  mutate(predictor = "Full model", 
         pred_label = "Full model",
         rank = NA)

# create data for plotting
out_da_abs <- out_da2 %>%
  # focus on factors and cluster var
  filter(predictor %in% c("Full model", grp, yr, factors)) %>%
  # determine rank of contribution
  group_by(data, outcome) %>%
  mutate(rank = min_rank(desc(contr_abs))) %>%
  ungroup() %>%
  # add total variance explained data
  bind_rows(tmp) %>%
  # fix labels
  mutate(
    pred_label = stringr::str_wrap(pred_label, width = 14),
    pred_label = factor(pred_label, 
                        levels = c("Full model", "Leadership", "Pupil\nbehaviour", "Collegial\nsupport", 
                                   "Professional\nopportunities", "School", "Year")),
    data = ifelse(data == "raw", "Raw", "LOO"),
    data = factor(data, levels = c("Raw", "LOO")),
    facet = ifelse(pred_label == "Full model", " ", "Average absolute contribution of each predictor")) %>%
  # add coordinates for plot
  group_by(outcome) %>%
  mutate(pos_y = max(contr_abs)) %>%
  ungroup()

# generate plot
p <- ggplot(data = out_da_abs, aes(x = data, y = contr_abs, fill = data)) +
  # dodge barplot
  geom_bar(stat = "identity", position = "dodge", colour = black) + 
  scale_fill_manual(values = c(red, blue)) +
  ggh4x::facet_nested(out_label ~ facet + pred_label, scales = "free") +
  ambition_theme +
  theme(legend.position = "none") +
  guides(fill = guide_legend(title = "Dataset")) +
  scale_y_continuous(breaks = seq(0, 1, .1), 
                     labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.15, 0.15))) +
  labs(x = "Dataset", y = "Explained variance") +
  # add contributions as text
  geom_text(aes(label = scales::percent(contr_abs, accuracy = 0.1)),
            size = 4, color = black, vjust = -0.5) +
  # add rank as label
  geom_label(aes(x = data, y = pos_y, label = rank, colour = data),
             size = 4, label.colour = black,
             vjust = 1, inherit.aes = FALSE) +
  scale_colour_manual(values = c(red, blue))
p

# save plot
ggsave(plot = p,
       filename = paste0(file_stem, "_da_abs.jpg"),
       path = file_dir,
       units = "cm",
       width = 25, height = 22)
