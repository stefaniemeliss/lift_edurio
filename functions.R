#### ALL-PURPOSE HELPER FUNCTIONS ####

# source code
source_code(target_repo = "helper_functions", file_name = "functions.R")

#### PROJECT-SPECIFIC FUNCTIONS ####

# functions for data processing #

# Function to insert "/" in the format "YYYYYY" to "YYYY/YY"
insert_slash <- function(number) {
  sub("(\\d{4})(\\d{2})", "\\1/\\2", number)
}

# Function to review column lookup table mappings
review_lookup_mappings <- function(lookup_table = column_lookup) {
  cat("=== COLUMN LOOKUP TABLE REVIEW ===\n\n")
  
  for (i in 1:nrow(lookup_table)) {
    standard <- lookup_table$standard_name[i]
    variations <- lookup_table$variations[[i]]
    
    cat(sprintf("Standard Name %d of %d:\n", i, nrow(lookup_table)))
    cat("Standard: ", standard, "\n")
    cat("Variations (", length(variations), "):\n")
    
    for (j in 1:length(variations)) {
      cat("  ", j, ". ", variations[j], "\n", sep = "")
    }
    cat("\n", paste(rep("-", 80), collapse = ""), "\n\n")
  }
}

# Create the reverse lookup function
#  transforms column lookup table from its current structure into a format 
# that's optimized for fast column name standardisation.
create_reverse_lookup <- function(lookup_table) {
  reverse_lookup <- list()
  
  for (i in 1:nrow(lookup_table)) {
    standard <- lookup_table$standard_name[i]
    variations <- lookup_table$variations[[i]]
    
    for (var in variations) {
      reverse_lookup[[var]] <- standard
    }
  }
  
  return(reverse_lookup)
}

# Enhanced function that only keeps columns included in the lookup table
# core data pipeline processor: 
## 1. Column Filtering & Selection
## 2. Column Name Standardisation
## 3. Duplicate Detection & Handling
## 4. Quality Control & Reporting
standardise_column_names <- function(df, lookup = reverse_lookup) {
  current_names <- names(df)
  
  # Get all possible column names that are in the lookup (variations that can be mapped)
  lookup_columns <- names(lookup)
  
  # Identify which columns from the dataframe are in the lookup
  columns_to_keep <- current_names[current_names %in% lookup_columns]
  
  if (length(columns_to_keep) == 0) {
    warning("No columns found that match the lookup table")
    return(df[, FALSE])  # Return empty dataframe
  }
  
  # Filter dataframe to only keep columns that are in the lookup
  df_filtered <- df[, columns_to_keep, drop = FALSE]
  
  # Now standardize the column names
  new_names <- names(df_filtered)
  for (i in 1:length(new_names)) {
    old_name <- new_names[i]
    if (old_name %in% names(lookup)) {
      new_names[i] <- lookup[[old_name]]
    }
  }
  
  # Check for duplicates BEFORE renaming
  if (any(duplicated(new_names))) {
    duplicate_names <- new_names[duplicated(new_names)]
    cat("WARNING: The following duplicate column names would be created:\n")
    print(duplicate_names)
    
    # Show which original columns are causing the duplicates
    for (dup_name in unique(duplicate_names)) {
      original_cols <- names(df_filtered)[new_names == dup_name]
      cat(paste("Columns mapping to", dup_name, ":", paste(original_cols, collapse = ", "), "\n"))
    }
    
    # Handle duplicates by keeping only the first occurrence and renaming others
    for (dup_name in unique(duplicate_names)) {
      dup_indices <- which(new_names == dup_name)
      if (length(dup_indices) > 1) {
        # Keep the first one, modify the others
        for (j in 2:length(dup_indices)) {
          new_names[dup_indices[j]] <- paste0(new_names[dup_indices[j]], "_", j-1)
        }
      }
    }
  }
  
  # Rename columns
  names(df_filtered) <- new_names
  
  # Report what happened
  original_count <- length(current_names)
  kept_count <- length(columns_to_keep)
  dropped_count <- original_count - kept_count
  
  cat(paste("Year:", ifelse(exists("academic_year"), academic_year, "Unknown"), "\n"))
  cat(paste("Original columns:", original_count, "\n"))
  cat(paste("Columns kept (in lookup):", kept_count, "\n"))
  cat(paste("Columns dropped (not in lookup):", dropped_count, "\n"))
  
  # Report column name changes
  changes <- data.frame(
    old_name = columns_to_keep[columns_to_keep != new_names],
    new_name = new_names[columns_to_keep != new_names],
    stringsAsFactors = FALSE
  )
  
  if (nrow(changes) > 0) {
    cat("Column name changes made:\n")
    print(changes)
  } else {
    cat("No column name changes needed.\n")
  }
  
  # Show which columns were dropped (for reference)
  dropped_columns <- current_names[!current_names %in% columns_to_keep]
  if (length(dropped_columns) > 0 && length(dropped_columns) <= 10) {
    cat("Dropped columns:", paste(dropped_columns, collapse = ", "), "\n")
  } else if (length(dropped_columns) > 10) {
    cat("Dropped columns (first 10):", paste(dropped_columns[1:10], collapse = ", "), "... and", length(dropped_columns) - 10, "more\n")
  }
  
  cat("\n")
  
  return(df_filtered)
}

# Function to produce histograms by group
plot_histogram <- function(data = df, 
                           xvar = x,
                           xlower = NULL,
                           xupper = NULL,
                           title = "",
                           xlab = "",
                           bin_width = NULL) {
  plot <- 
    # define variables to plot based on input
    ggplot(data, aes(x = get(xvar))) +
    # basic histogram with density
    geom_histogram(aes(y = after_stat(density)), color = nondominant_col, fill = nondominant_col, bins = 30, binwidth = bin_width) +
    # add density plot
    geom_density(alpha = .2, fill = dominant_col, col = dominant_col) +
    # add mean as vertical line
    geom_vline(aes(xintercept = mean(get(xvar), na.rm = T)),
               color = dominant_col, linetype = "dashed", linewidth = 1) +
    # add theme
    ambition_theme +
    # determine titles
    ggtitle(paste0(title)) + xlab(paste0(xlab)) + ylab("Density") 
  
  
  # determine coord system + scales
  if (!is.null(xlower) | !is.null(xupper)) {
    
    #  modify x axis
    plot <- plot + coord_cartesian(xlim = c(xlower, xupper))
  }
  return(plot)
  
}


# Helper function for residuals-based partial correlation
partial_correlation_residuals <- function(df, outcome, predictor, controls) {
  formula_y <- as.formula(
    paste(outcome, "~", paste(controls, collapse = " + "))
  )
  formula_x <- as.formula(
    paste(predictor, "~", paste(controls, collapse = " + "))
  )
  resid_y <- residuals(lm(formula_y, data = df))
  resid_x <- residuals(lm(formula_x, data = df))
  cor(resid_y, resid_x)
}

get_betas_cluster <- function(df, outcome, predictors, fe_vars, cluster_vars, binary_outcome = TRUE) {
  
  # Build formulas
  if (binary_outcome) {
    formula_unstd <- as.formula(
      paste(outcome, "~", paste(c(predictors, fe_vars), collapse = " + "))
    )
    formula_std <- as.formula(
      paste0(outcome, " ~ ", paste0("scale(", predictors, ")", collapse = " + "), " + ", paste0(fe_vars, collapse = " + "))
    )
  } else {
    formula_unstd <- as.formula(
      paste(outcome, "~", paste(c(predictors, fe_vars), collapse = " + "))
    )
    formula_std <- as.formula(
      paste0("scale(", outcome, ") ~ ", paste0("scale(", predictors, ")", collapse = " + "), " + ", paste0(fe_vars, collapse = " + "))
    )
  }
  
  # Fit models
  if (binary_outcome) {
    model_unstd <- glm(formula_unstd, data = df, family = binomial())
    model_std   <- glm(formula_std,   data = df, family = binomial())
  } else {
    model_unstd <- lm(formula_unstd, data = df)
    model_std   <- lm(formula_std,   data = df)
  }
  
  # Multiway cluster-robust SEs: school, year, school-year pair
  cluster_list <- list(df[[cluster_vars[1]]], df[[cluster_vars[2]]], interaction(df[[cluster_vars[1]]], df[[cluster_vars[2]]]))
  vcov_unstd <- sandwich::vcovCL(model_unstd, cluster = cluster_list)
  vcov_std   <- sandwich::vcovCL(model_std,   cluster = cluster_list)
  
  # Raw coefficients
  coefs_unstd <- summary(model_unstd)$coefficients
  coefs_std   <- summary(model_std)$coefficients
  robust_unstd <- lmtest::coeftest(model_unstd, vcov. = vcov_unstd)
  robust_std   <- lmtest::coeftest(model_std,   vcov. = vcov_std)
  
  # Names for standardised predictors in model
  preds_std_names <- paste0("scale(", predictors, ")")
  
  # Extract rows for predictors only
  unstd_rows <- rownames(coefs_unstd) %in% predictors
  std_rows   <- rownames(coefs_std)   %in% preds_std_names
  robust_unstd_rows <- rownames(robust_unstd) %in% predictors
  robust_std_rows   <- rownames(robust_std)   %in% preds_std_names
  
  # For continuous outcomes, calculate partial correlations using residuals
  if (!binary_outcome) {
    # Controls for partial correlation: all other predictors and fixed effects
    controls <- c(setdiff(predictors, predictors), fe_vars) # placeholder, will be replaced below
    pcor_resid <- sapply(predictors, function(pred) {
      controls <- c(setdiff(predictors, pred), fe_vars)
      partial_correlation_residuals(df, outcome, pred, controls)
    })
  }
  
  # Combine into one table in your requested order
  if (binary_outcome) {
    result_table <- data.frame(
      outcome = outcome,
      predictor = predictors,
      # Log odds and odds ratios
      Log_odds_unstd = coefs_unstd[unstd_rows, "Estimate"],
      Log_odds_std   = coefs_std[std_rows, "Estimate"],
      OR_unstd = exp(coefs_unstd[unstd_rows, "Estimate"]),
      OR_std   = exp(coefs_std[std_rows, "Estimate"]),
      # SEs
      SE_unstd       = coefs_unstd[unstd_rows, "Std. Error"],
      SE_unstd_CR    = robust_unstd[robust_unstd_rows, "Std. Error"],
      SE_std         = coefs_std[std_rows, "Std. Error"],
      SE_std_CR      = robust_std[robust_std_rows, "Std. Error"],
      # z values
      z_unstd        = coefs_unstd[unstd_rows, "z value"],
      z_unstd_CR     = robust_unstd[robust_unstd_rows, "z value"],
      z_std          = coefs_std[std_rows, "z value"],
      z_std_CR       = robust_std[robust_std_rows, "z value"],
      # p values
      p_unstd        = coefs_unstd[unstd_rows, "Pr(>|z|)"],
      p_unstd_CR     = robust_unstd[robust_unstd_rows, "Pr(>|z|)"],
      p_std          = coefs_std[std_rows, "Pr(>|z|)"],
      p_std_CR       = robust_std[robust_std_rows, "Pr(>|z|)"],
      # df
      df_unstd   = df.residual(model_unstd),
      df_std     = df.residual(model_std),
      # formula
      formula_unstd = rep(paste(as.character(formula_unstd)[2], "~", as.character(formula_unstd)[3]), length(predictors)),
      formula_std  = rep(paste(as.character(formula_std)[2], "~", as.character(formula_std)[3]), length(predictors)),
      row.names = NULL
    )
  } else {
    # Calculate partial correlations for continuous outcomes
    t_unstd    <- coefs_unstd[unstd_rows, "t value"]
    t_unstd_CR <- robust_unstd[robust_unstd_rows, "t value"]
    t_std      <- coefs_std[std_rows, "t value"]
    t_std_CR   <- robust_std[robust_std_rows, "t value"]
    df_unstd   <- df.residual(model_unstd)
    df_std     <- df.residual(model_std)
    
    result_table <- data.frame(
      outcome = outcome,
      predictor = predictors,
      # Betas
      Beta_unstd = coefs_unstd[unstd_rows, "Estimate"],
      Beta_std   = coefs_std[std_rows, "Estimate"],
      # Partial correlations (inserted after beta columns)
      pcor_unstd    = t_unstd    / sqrt(t_unstd^2    + df_unstd),
      pcor_unstd_CR = t_unstd_CR / sqrt(t_unstd_CR^2 + df_unstd),
      pcor_std      = t_std      / sqrt(t_std^2      + df_std),
      pcor_std_CR   = t_std_CR   / sqrt(t_std_CR^2   + df_std),
      pcor_resid    = pcor_resid, # residuals-based partial correlation
      # SEs
      SE_unstd       = coefs_unstd[unstd_rows, "Std. Error"],
      SE_unstd_CR    = robust_unstd[robust_unstd_rows, "Std. Error"],
      SE_std         = coefs_std[std_rows, "Std. Error"],
      SE_std_CR      = robust_std[robust_std_rows, "Std. Error"],
      # t values
      t_unstd        = t_unstd,
      t_unstd_CR     = t_unstd_CR,
      t_std          = t_std,
      t_std_CR       = t_std_CR,
      # p values
      p_unstd        = coefs_unstd[unstd_rows, "Pr(>|t|)"],
      p_unstd_CR     = robust_unstd[robust_unstd_rows, "Pr(>|t|)"],
      p_std          = coefs_std[std_rows, "Pr(>|t|)"],
      p_std_CR       = robust_std[robust_std_rows, "Pr(>|t|)"],
      # df
      df_unstd = df_unstd,
      df_std   = df_std,
      # formula
      formula_unstd = rep(paste(as.character(formula_unstd)[2], "~", as.character(formula_unstd)[3]), length(predictors)),
      formula_std  = rep(paste(as.character(formula_std)[2], "~", as.character(formula_std)[3]), length(predictors)),
      row.names = NULL
    )
  }
  
  return(result_table)
}

get_betas_mixed <- function(
    df, outcome, predictors, fe_vars = NULL, re_vars = NULL,
    binary_outcome = TRUE
) {
  
  # If no random effects, use fixed effects model with robust SEs, clustering by fe_vars
  if (is.null(re_vars) || length(re_vars) == 0) {
    if (is.null(fe_vars) || length(fe_vars) == 0) {
      stop("No random effects specified and no fixed effects specified. At least one clustering variable (fe_vars) is required for robust SEs.")
    }
    return(get_betas_cluster(df = df, outcome = outcome,  predictors = predictors,
                             fe_vars = fe_vars, cluster_vars = fe_vars,  # use fixed effects as clustering variables
                             binary_outcome = binary_outcome
    ))
  }  
  # Build fixed and random effects parts
  fe_part <- c(predictors, fe_vars)
  fe_formula <- if (length(fe_part) > 0) paste(fe_part, collapse = " + ") else "1"
  re_formula <- paste0("(1 | ", re_vars, ")", collapse = " + ")
  
  # Build formulas using scale() in the formula
  if (binary_outcome) {
    formula_unstd <- as.formula(
      paste0(outcome, " ~ ", fe_formula, if (!is.null(re_formula)) paste0(" + ", re_formula) else "")
    )
    formula_std <- as.formula(
      paste0(outcome, " ~ ",
             paste0("scale(", predictors, ")", collapse = " + "),
             if (!is.null(fe_vars)) paste0(" + ", paste(fe_vars, collapse = " + ")) else "",
             if (!is.null(re_formula)) paste0(" + ", re_formula) else "")
    )
    model_unstd <- lme4::glmer(formula_unstd, data = df, family = binomial(), nAGQ = 0)
    model_std   <- lme4::glmer(formula_std,   data = df, family = binomial(), nAGQ = 0)
  } else {
    formula_unstd <- as.formula(
      paste0(outcome, " ~ ", fe_formula, if (!is.null(re_formula)) paste0(" + ", re_formula) else "")
    )
    formula_std <- as.formula(
      paste0("scale(", outcome, ") ~ ",
             paste0("scale(", predictors, ")", collapse = " + "),
             if (!is.null(fe_vars)) paste0(" + ", paste(fe_vars, collapse = " + ")) else "",
             if (!is.null(re_formula)) paste0(" + ", re_formula) else "")
    )
    model_unstd <- lmerTest::lmer(formula_unstd, data = df)
    model_std   <- lmerTest::lmer(formula_std,   data = df)
  }
  
  # Extract coefficients for predictors
  coefs_unstd <- summary(model_unstd)$coefficients
  coefs_std   <- summary(model_std)$coefficients
  preds_std_names <- paste0("scale(", predictors, ")")
  unstd_rows <- rownames(coefs_unstd) %in% predictors
  std_rows   <- rownames(coefs_std)   %in% preds_std_names
  
  if (binary_outcome) {
    result_table <- data.frame(
      outcome = outcome,
      predictor = predictors,
      Log_odds_unstd = coefs_unstd[unstd_rows, "Estimate"],
      Log_odds_std   = coefs_std[std_rows, "Estimate"],
      OR_unstd = exp(coefs_unstd[unstd_rows, "Estimate"]),
      OR_std   = exp(coefs_std[std_rows, "Estimate"]),
      SE_unstd = coefs_unstd[unstd_rows, "Std. Error"],
      SE_std   = coefs_std[std_rows, "Std. Error"],
      z_unstd  = coefs_unstd[unstd_rows, "Estimate"] / coefs_unstd[unstd_rows, "Std. Error"],
      z_std    = coefs_std[std_rows, "Estimate"] / coefs_std[std_rows, "Std. Error"],
      p_unstd  = coefs_unstd[unstd_rows, "Pr(>|z|)"],
      p_std    = coefs_std[std_rows, "Pr(>|z|)"],
      formula_unstd = rep(paste(as.character(formula_unstd)[2], "~", as.character(formula_unstd)[3]), length(predictors)),
      formula_std  = rep(paste(as.character(formula_std)[2], "~", as.character(formula_std)[3]), length(predictors)),
      row.names = NULL
    )
  } else {
    t_unstd <- coefs_unstd[unstd_rows, "t value"]
    t_std   <- coefs_std[std_rows, "t value"]
    df_unstd <- df.residual(model_unstd)
    df_std   <- df.residual(model_std)
    result_table <- data.frame(
      outcome = outcome,
      predictor = predictors,
      Beta_unstd = coefs_unstd[unstd_rows, "Estimate"],
      Beta_std   = coefs_std[std_rows, "Estimate"],
      SE_unstd   = coefs_unstd[unstd_rows, "Std. Error"],
      SE_std     = coefs_std[std_rows, "Std. Error"],
      t_unstd    = t_unstd,
      t_std      = t_std,
      p_unstd    = coefs_unstd[unstd_rows, "Pr(>|t|)"],
      p_std      = coefs_std[std_rows, "Pr(>|t|)"],
      df_unstd = df_unstd,
      df_std   = df_std,
      pcor_unstd = t_unstd / sqrt(t_unstd^2 + df_unstd),
      pcor_std   = t_std / sqrt(t_std^2 + df_std),
      formula_unstd = rep(paste(as.character(formula_unstd)[2], "~", as.character(formula_unstd)[3]), length(predictors)),
      formula_std  = rep(paste(as.character(formula_std)[2], "~", as.character(formula_std)[3]), length(predictors)),
      row.names = NULL
    )
  }
  
  return(result_table)
}

icc_school_year <- function(df, outcome, school_var, year_var, binary_outcome = FALSE) {
  require(lme4)
  
  # Helper for extracting variance components
  get_vars <- function(model, var1, var2 = NULL) {
    vc <- VarCorr(model)
    var_list <- list()
    var_list$var1 <- as.numeric(vc[[var1]])
    if (!is.null(var2)) var_list$var2 <- as.numeric(vc[[var2]])
    var_list$resid <- attr(vc, "sc")^2
    var_list
  }
  
  # Model with only school as random effect
  if (binary_outcome) {
    model_school <- glmer(
      as.formula(paste0(outcome, " ~ 1 + (1 | ", school_var, ")")),
      data = df, family = binomial()
    )
  } else {
    model_school <- lmer(
      as.formula(paste0(outcome, " ~ 1 + (1 | ", school_var, ")")),
      data = df
    )
  }
  vars_school <- get_vars(model_school, school_var)
  icc_school_only <- vars_school$var1 / (vars_school$var1 + vars_school$resid)
  
  # Model with only year as random effect
  if (binary_outcome) {
    model_year <- glmer(
      as.formula(paste0(outcome, " ~ 1 + (1 | ", year_var, ")")),
      data = df, family = binomial()
    )
  } else {
    model_year <- lmer(
      as.formula(paste0(outcome, " ~ 1 + (1 | ", year_var, ")")),
      data = df
    )
  }
  vars_year <- get_vars(model_year, year_var)
  icc_year_only <- vars_year$var1 / (vars_year$var1 + vars_year$resid)
  
  # Model with both school and year as random effects
  if (binary_outcome) {
    model_both <- glmer(
      as.formula(paste0(outcome, " ~ 1 + (1 | ", school_var, ") + (1 | ", year_var, ")")),
      data = df, family = binomial()
    )
  } else {
    model_both <- lmer(
      as.formula(paste0(outcome, " ~ 1 + (1 | ", school_var, ") + (1 | ", year_var, ")")),
      data = df
    )
  }
  vars_both <- get_vars(model_both, school_var, year_var)
  total_var_both <- vars_both$var1 + vars_both$var2 + vars_both$resid
  icc_school_both <- vars_both$var1 / total_var_both
  icc_year_both   <- vars_both$var2 / total_var_both
  
  # Output as data frame
  result <- data.frame(
    outcome = outcome,
    ICC_school_only = icc_school_only,
    ICC_year_only = icc_year_only,
    ICC_school_both = icc_school_both,
    ICC_year_both = icc_year_both
  )
  return(result)
}

# Positive delta AIC: The more complex model is preferred.
# Negative delta AIC: The base (simpler) model is preferred.

# General rules of thumb:
# ΔAIC < 2: Models are essentially equivalent.
# ΔAIC 4–7: Less support for the model with higher AIC.
# ΔAIC > 10: Much less support for the model with higher AIC.
# (See Burnham & Anderson, 2002, "Model Selection and Multimodel Inference")

compare_school_year_models <- function(df, outcome, predictors, school_var = "school", year_var = "year", binary_outcome = FALSE) {
  require(lme4)
  
  # Helper to build formulas
  pred_str <- paste(predictors, collapse = " + ")
  
  # Model formulas
  f_no_year  <- as.formula(paste0(outcome, " ~ ", pred_str, " + (1 | ", school_var, ")"))
  f_year_re  <- as.formula(paste0(outcome, " ~ ", pred_str, " + (1 | ", school_var, ") + (1 | ", year_var, ")"))
  f_year_fe  <- as.formula(paste0(outcome, " ~ ", pred_str, " + (1 | ", school_var, ") + factor(", year_var, ")"))
  f_no_school <- as.formula(paste0(outcome, " ~ ", pred_str, " + (1 | ", year_var, ")"))
  f_school_re <- as.formula(paste0(outcome, " ~ ", pred_str, " + (1 | ", year_var, ") + (1 | ", school_var, ")"))
  f_school_fe <- as.formula(paste0(outcome, " ~ ", pred_str, " + (1 | ", year_var, ") + factor(", school_var, ")"))
  
  # Model fitting function
  fit <- function(formula, binary) {
    if (binary) {
      lme4::glmer(formula, data = df, family = binomial(), nAGQ = 0)
    } else {
      lme4::lmer(formula, data = df)
    }
  }
  
  # 1. Random effect for year vs. no year effect (random school in both)
  m1_base <- fit(f_no_year, binary_outcome)
  m1_year_re <- fit(f_year_re, binary_outcome)
  an1 <- anova(m1_base, m1_year_re)
  deltaAIC1 <- AIC(m1_base) - AIC(m1_year_re)
  
  # 2. Year as fixed effect vs. no year effect (random school in both)
  m2_year_fe <- fit(f_year_fe, binary_outcome)
  an2 <- anova(m1_base, m2_year_fe)
  deltaAIC2 <- AIC(m1_base) - AIC(m2_year_fe)
  
  # 3. Year as random effect vs. year as fixed effect (random school in both)
  an3 <- anova(m2_year_fe, m1_year_re)
  deltaAIC3 <- AIC(m2_year_fe) - AIC(m1_year_re)
  
  # 4. Random effect for school vs. no school effect (random year in both)
  m4_base <- fit(f_no_school, binary_outcome)
  m4_school_re <- fit(f_school_re, binary_outcome)
  an4 <- anova(m4_base, m4_school_re)
  deltaAIC4 <- AIC(m4_base) - AIC(m4_school_re)
  
  # 5. School as fixed effect vs. no school effect (random year in both)
  m5_school_fe <- fit(f_school_fe, binary_outcome)
  an5 <- anova(m4_base, m5_school_fe)
  deltaAIC5 <- AIC(m4_base) - AIC(m5_school_fe)
  
  # 6. School as random effect vs. school as fixed effect (random year in both)
  an6 <- anova(m5_school_fe, m4_school_re)
  deltaAIC6 <- AIC(m5_school_fe) - AIC(m4_school_re)
  
  # Assemble results
  result <- data.frame(
    outcome = outcome,
    predictor = pred_str,
    
    Comparison = c(
      "No year effect (RE school) vs. Year RE",
      "No year effect (RE school) vs. Year FE",
      "Year FE vs. Year RE (RE school)",
      "No school effect (RE year) vs. School RE",
      "No school effect (RE year) vs. School FE",
      "School FE vs. School RE (RE year)"
    ),
    chisq = c(
      an1$Chisq[2],
      an2$Chisq[2],
      an3$Chisq[2],
      an4$Chisq[2],
      an5$Chisq[2],
      an6$Chisq[2]
    ),
    df = c(
      an1$Df[2],
      an2$Df[2],
      an3$Df[2],
      an4$Df[2],
      an5$Df[2],
      an6$Df[2]
    ),
    p = c(
      an1$`Pr(>Chisq)`[2],
      an2$`Pr(>Chisq)`[2],
      an3$`Pr(>Chisq)`[2],
      an4$`Pr(>Chisq)`[2],
      an5$`Pr(>Chisq)`[2],
      an6$`Pr(>Chisq)`[2]
    ),
    delta_AIC = c(
      deltaAIC1,
      deltaAIC2,
      deltaAIC3,
      deltaAIC4,
      deltaAIC5,
      deltaAIC6
    ),
    row.names = NULL
  )
  return(result)
}

unique_predictor_contributions <- function(df, outcome, predictors, fe_vars = NULL, re_vars = NULL, binary_outcome = FALSE) {
  require(lme4)
  require(MuMIn)
  
  # Build fixed effect terms
  fe_term <- if (!is.null(fe_vars)) paste(fe_vars, collapse = " + ") else NULL
  
  # Helper to build RHS
  build_rhs <- function(preds) {
    terms <- character()
    if (length(preds) > 0) {
      terms <- c(terms, paste0("scale(", preds, ")"))
    }
    if (!is.null(fe_vars)) terms <- c(terms, fe_vars)
    if (length(terms) == 0) {
      rhs <- "1"
    } else {
      rhs <- paste(terms, collapse = " + ")
    }
    rhs
  }
  
  build_formula <- function(rhs) {
    if (binary_outcome) {
      as.formula(paste0(outcome, " ~ ", rhs))
    } else {
      as.formula(paste0("scale(", outcome, ") ~ ", rhs))
    }
  }
  
  # Fit full model
  rhs_full <- build_rhs(predictors)
  formula_full <- if (is.null(re_vars)) {
    build_formula(rhs_full)
  } else {
    re_term <- paste0("(1 | ", re_vars, ")", collapse = " + ")
    as.formula(paste0(
      if (binary_outcome) paste0(outcome, " ~ ", rhs_full)
      else paste0("scale(", outcome, ") ~ ", rhs_full),
      " + ", re_term
    ))
  }
  
  if (is.null(re_vars)) {
    if (binary_outcome) {
      model_full <- glm(formula_full, data = df, family = binomial())
      r2_full <- pscl::pR2(model_full)[["r2CU"]]
    } else {
      model_full <- lm(formula_full, data = df)
      r2_full <- summary(model_full)$r.squared
    }
  } else {
    if (binary_outcome) {
      model_full <- glmer(formula_full, data = df, family = binomial(), nAGQ = 0)
      r2_full <- MuMIn::r.squaredGLMM(model_full)[1, "R2m"]
    } else {
      model_full <- lmer(formula_full, data = df, REML = TRUE)
      r2_full <- MuMIn::r.squaredGLMM(model_full)[, "R2m"]
    }
  }
  
  results <- list()
  
  for (p in seq_along(predictors)) {
    preds_tmp <- predictors[-p]
    rhs_reduced <- build_rhs(preds_tmp)
    if (is.null(re_vars)) {
      formula_reduced <- build_formula(rhs_reduced)
      if (binary_outcome) {
        model_reduced <- glm(formula_reduced, data = df, family = binomial())
        r2_reduced <- pscl::pR2(model_reduced)[["r2CU"]]
        lr_test <- anova(model_reduced, model_full, test = "Chisq")
      } else {
        model_reduced <- lm(formula_reduced, data = df)
        r2_reduced <- summary(model_reduced)$r.squared
        lr_test <- anova(model_reduced, model_full)
      }
    } else {
      formula_reduced <- as.formula(
        paste0(
          if (binary_outcome) paste0(outcome, " ~ ", rhs_reduced)
          else paste0("scale(", outcome, ") ~ ", rhs_reduced),
          " + ", paste0("(1 | ", re_vars, ")", collapse = " + ")
        )
      )
      if (binary_outcome) {
        model_reduced <- glmer(formula_reduced, data = df, family = binomial(), nAGQ = 0)
        r2_reduced <- MuMIn::r.squaredGLMM(model_reduced)[1, "R2m"]
        lr_test <- as.data.frame(anova(model_reduced, model_full))
      } else {
        model_reduced <- lmer(formula_reduced, data = df, REML = TRUE)
        r2_reduced <- MuMIn::r.squaredGLMM(model_reduced)[, "R2m"]
        lr_test <- as.data.frame(anova(model_reduced, model_full))
      }
    }
    results[[p]] <- data.frame(
      outcome = outcome,
      predictor = predictors[p],
      
      formula_std  = paste(as.character(formula_full)[2], "~", as.character(formula_full)[3]),
      formula_red  = paste(as.character(formula_reduced)[2], "~", as.character(formula_reduced)[3])
    )
    
    if (is.null(re_vars)) {
      if (binary_outcome) {
        results[[p]]$param_lr <- "Deviance"
        results[[p]]$value <- as.numeric(lr_test$Deviance[2])
        results[[p]]$p_lr <- as.numeric(lr_test$`Pr(>Chi)`[2])
        results[[p]]$R2 <- "pseudo"
      } else {
        results[[p]]$param_lr <- "F"
        results[[p]]$value <- as.numeric(lr_test$F[2])
        results[[p]]$p_lr <- as.numeric(lr_test$`Pr(>F)`[2])
        results[[p]]$R2 <- "std"
      }
    } else {
      results[[p]]$param_lr <- "Chisq"
      results[[p]]$value <- as.numeric(lr_test$Chisq[2])
      results[[p]]$p_lr <- as.numeric(lr_test$`Pr(>Chisq)`[2])
      results[[p]]$R2 <- "std"
    }
    results[[p]]$R2_full = r2_full
    results[[p]]$R2_reduced = r2_reduced
    results[[p]]$delta_R2 = r2_full - r2_reduced
    results[[p]]$df_lr = lr_test$Df[2]
  }
  do.call(rbind, results)
}
